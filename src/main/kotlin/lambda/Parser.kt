package lambda

import lambda.Token.Companion.get
import lambda.syntax.*


class Parser(tokens: Iterator<Spanned<Token>>) {

    val iterator = PeekableIterator(tokens)

    fun parseSourceFile(): SourceFile {
        val header = parseModuleHeader()
        val startPosition = header.span.start
        val parsedDeclarations = mutableListOf<Declaration>()
        while (true) {
            if (iterator.peek().value == Token.EOF) break
            parsedDeclarations += parseDeclaration()
        }
        val endPosition = parsedDeclarations.lastOrNull()?.span?.end ?: startPosition
        return SourceFile(header, parsedDeclarations, Span(startPosition, endPosition))
    }

    fun parseModuleHeader(): ModuleHeader {
        val startPosition = iterator.peek().span.start
        expectNext<Token.Module>(expectedError("expected module"))
        val namespace = parseNamespace()
        val imports = parseImports()
        // force unwrapping is safe here, because parseNamespace can't return the local namespace
        val endPosition = imports.lastOrNull()?.span?.end ?: namespace.span!!.end
        val span = Span(startPosition, endPosition)
        return ModuleHeader(namespace, imports, span)
    }

    private fun parseImports(): List<Import> {
        val imports = mutableListOf<Import>()
        while (iterator.peek().value is Token.Import) {
            val start = iterator.next().span.start
            val ns = parseNamespace()
            var qualifier = ns

            if (iterator.peek().value is Token.As) {
                iterator.next()
                qualifier = parseNamespace()
            }

            // force unwrapping is safe here, because parseNamespace can't return the local namespace
            imports += Import.Qualified(ns, qualifier, Span(start, qualifier.span!!.end))
        }

        return imports
    }

    private fun parseNamespace(): Namespace {
        val names = mutableListOf<Name>()
        names += parseUpperName()

        while (iterator.peek().value is Token.DoubleColon) {
            iterator.next()
            names += parseUpperName()
        }

        return Namespace(names)
    }

    fun parseDeclaration(): Declaration {
        val t = iterator.peek()
        return when (t.value) {
            is Token.Let -> {
                parseValueDeclaration()
            }
            is Token.Type -> {
                parseTypeDeclaration()
            }
            else -> throw RuntimeException("expected a type or value declaration at ${t.span.start}")
        }
    }

    fun parseValueDeclaration(): Declaration.Value {
        val startPosition = iterator.next().span.start
        val name = parseName()
        expectNext<Token.Colon>(expectedError("expected colon"))
        val scheme: Scheme = parseScheme()
        expectNext<Token.Equals>(expectedError("expected equals"))
        val expression = parseExpression()
        val semi = expectNext<Token.Semicolon>(expectedError("value declarations need to end with semicolon"))

        val span = Span(startPosition, semi.span.end)

        return Declaration.Value(name, scheme, expression, span)
    }

    fun parseMatch(): Expression.Match {
        val startPosition = iterator.next().span.start
        val expr = parseExpression()

        expectNext<Token.LBrace>(expectedError("expected open brace"))
        val cases = commaSeparated(::parseCase) { it !is Token.RBrace }
        val endPosition = expectNext<Token.RBrace>(expectedError("expected closing brace")).span.end

        return Expression.Match(expr, cases, Span(startPosition, endPosition))
    }

    private fun parseScheme(): Scheme {
        val vars = mutableListOf<TyVar>()
        val t = iterator.peek()
        if (t.value is Token.Forall) {
            iterator.next()
            while (true) {
                vars += parseTyVar()
                if (iterator.peek().value == Token.Dot) {
                    iterator.next()
                    break
                }
            }
        }
        val ty = parseType()

        return Scheme(vars, ty)
    }

    fun parseTypeDeclaration(): Declaration.Type {
        val startPosition = iterator.next().span.start
        val name = parseUpperName()
        var tyArgs = emptyList<TyVar>()

        if (iterator.peek().value is Token.LAngle) {
            iterator.next()

            tyArgs = commaSeparated(::parseTyVar) { it !is Token.RAngle }
            expectNext<Token.RAngle>(expectedError("expected closing angle"))
        }

        expectNext<Token.LBrace>(expectedError("expected open brace"))
        val dataConstructors = commaSeparated(::parseDataConstructor) { it !is Token.RBrace }
        val endPosition = expectNext<Token.RBrace>(expectedError("expected closing brace")).span.end

        return Declaration.Type(name, tyArgs, dataConstructors, Span(startPosition, endPosition))
    }

    fun parseDataConstructor(): DataConstructor {
        val name = parseUpperName()
        expectNext<Token.LParen>(expectedError("expected left paren"))
        val fields = commaSeparated(::parseType) { it !is Token.RParen }
        val endPosition = expectNext<Token.RParen>(expectedError("expected right paren")).span.end

        return DataConstructor(name, fields, Span(name.span.start, endPosition))
    }

    fun parseType(): Type {
        val arg = parseTypeAtom()
        return when (iterator.peek().value) {
            is Token.Arrow -> {
                iterator.next()
                val res = parseType()
                Type.Fun(arg, res, Span(arg.span.start, res.span.end))
            }
            else -> arg
        }
    }

    fun parseTypeAtom(): Type {
        val (start, nextToken) = iterator.peek()

        return when (nextToken) {
            is Token.LParen -> {
                iterator.next()

                val type = parseType()
                expectNext<Token.RParen>(expectedError("missing closing paren"))

                type
            }
            is Token.UpperIdent -> {
                val (ns, name) = parseQualifier()

                if (name == null)
                    throw Exception("expected a qualified type constructor at ${ns.span}")

                var tyArgs = emptyList<Type>()

                if (iterator.peek().value is Token.LAngle) {
                    iterator.next()

                    tyArgs = commaSeparated(::parseType) { it !is Token.RAngle }
                    expectNext<Token.RAngle>(expectedError("expected closing angle"))
                }

                Type.Constructor(ns, name, tyArgs)
            }
            is Token.Ident -> {
                Type.Var(parseTyVar())
            }
            else -> throw RuntimeException("expected type found $nextToken at $start")
        }
    }

    fun parseName(): Name {
        val (span, ident) = expectNext<Token.Ident>(expectedError("expected identifier"))
        return Name(ident.ident, span)
    }

    fun parseTyVar(): TyVar {
        return TyVar(parseName())
    }

    fun parseUpperName(msg: String = "expected uppercase identifier"): Name {
        val (span, ident) = expectNext<Token.UpperIdent>(expectedError(msg))
        return Name(ident.ident, span)
    }

    fun parseVar(namespace: Namespace): Expression {
        val ident = parseName()
        return Expression.Var(ident, namespace)
    }

    fun parseHole(): Expression {
        val (span, token) = expectNext<Token.HoleIdent>(expectedError("hole"))
        return Expression.Hole(Name(token.ident, span))
    }

    fun parseInt(): Expression.Literal {
        val (span, intToken) = expectNext<Token.IntToken>(expectedError("expected int literal"))
        return Expression.Literal(Lit.Int(intToken.int, span))
    }

    fun parseBool(): Expression.Literal {
        val (span, boolToken) = expectNext<Token.BoolToken>(expectedError("expected boolean literal"))
        return Expression.Literal(Lit.Bool(boolToken.bool, span))
    }

    fun parseString(): Expression.Literal {
        val (span, stringToken) = expectNext<Token.StringToken>(expectedError("expected string literal"))
        return Expression.Literal(Lit.String(stringToken.string, span))
    }


    fun parseLambda(): Expression.Lambda {
        val (start, _) = iterator.next()
        val binders = mutableListOf<Name>()
        binders += parseName()

        while (iterator.peek().value is Token.Ident) {
            binders += parseName()
        }
        expectNext<Token.Dot>(expectedError("expected dot"))

        val body = parseExpression()

        return Expression.Lambda(binders, body, Span(start.start, body.span.end))
    }


    fun parseExpression(): Expression {
        val atoms = mutableListOf<Expression>()
        while (iterator.hasNext()) {
            parseAtom()?.let {
                atoms += it
            } ?: break
        }

        return when {
            atoms.isEmpty() -> throw RuntimeException("failed to parse expression")
            atoms.size == 1 -> atoms.first()
            else -> atoms.drop(1).fold(atoms[0]) { func, arg ->
                Expression.App(func, arg, Span(func.span.start, arg.span.end))
            }
        }

    }

    private fun parseAtom(): Expression? {
        return when (iterator.peek().value) {
            is Token.LParen -> {
                val (start, _) = iterator.next()
                val expr = parseExpression()
                var type: Type? = null
                if (iterator.peek().value == Token.Colon) {
                    iterator.next()
                    type = parseType()
                }
                val (end, _) = expectNext<Token.RParen>(expectedError("missing closing paren"))

                if (type == null) {
                    expr
                } else {
                    Expression.Typed(expr, type, Span(start.start, end.end))
                }
            }
            is Token.Lam -> parseLambda()
            is Token.Ident -> parseVar(Namespace.local)
            is Token.HoleIdent -> parseHole()
            is Token.UpperIdent -> {
                //parseDataConstruction()
                val (ns, trailing) = parseQualifier()

                if (trailing == null)
                    parseVar(ns)
                else
                    parseDataConstruction(ns, trailing)
            }
            is Token.IntToken -> parseInt()
            is Token.BoolToken -> parseBool()
            is Token.StringToken -> parseString()
            is Token.Let -> parseLet(false)
            is Token.LetRec -> parseLet(true)
            is Token.If -> parseIf()
            is Token.Match -> parseMatch()
            else -> null
        }
    }

    private fun parseQualifier(): Pair<Namespace, Name?> {
        if (iterator.peek().value !is Token.UpperIdent)
            return Namespace.local to null

        val names = mutableListOf<Name>()
        names += parseUpperName()

        while (iterator.peek().value is Token.DoubleColon) {
            iterator.next()

            if (iterator.peek().value !is Token.UpperIdent)
                return Namespace(names) to null

            names += parseUpperName()
        }

        return Namespace(names.dropLast(1)) to names.last()
    }

    // Color::Red()
    private fun parseDataConstruction(namespace: Namespace, dtor: Name): Expression.Construction {
        expectNext<Token.LParen>(expectedError("expected open paren"))

        val exprs = commaSeparated(::parseExpression) { it !is Token.RParen }
        val endPos = expectNext<Token.RParen>(expectedError("expected closing paren")).span.end

        return Expression.Construction(namespace, dtor, exprs, Span((namespace.span ?: dtor.span).start, endPos))
    }

    fun parseCase(): Expression.Case {
        val (ns, dtor) = parseQualifier() // List::List::Cons

        if (dtor == null)
            throw Exception("expected a qualified data constructor at ${ns.span}")

        expectNext<Token.LParen>(expectedError("expected left paren"))
        val binders = commaSeparated(::parseName) { it !is Token.RParen }
        expectNext<Token.RParen>(expectedError("expected right paren"))
        expectNext<Token.FatArrow>(expectedError("expected fat arrow"))
        val body = parseExpression()

        return Expression.Case(ns, dtor, binders, body, Span((ns.span ?: dtor.span).start, body.span.end))
    }

    private fun parseLet(recursive: Boolean): Expression.Let {
        val (letSpan, _) = iterator.next()
        val binder = parseName()
        var scheme: Scheme? = null
        if (iterator.peek().value is Token.Colon) {
            iterator.next()
            scheme = parseScheme()
        }
        expectNext<Token.Equals>(expectedError("expected equals"))
        val expr = parseExpression()
        expectNext<Token.In>(expectedError("expected in"))
        val body = parseExpression()

        return Expression.Let(recursive, binder, scheme, expr, body, Span(letSpan.start, body.span.end))
    }

    private fun parseIf(): Expression.If { // if true then 3 else 4
        val (ifSpan, _) = iterator.next()
        val condition = parseExpression()
        expectNext<Token.Then>(expectedError("expected then"))
        val thenBranch = parseExpression()
        expectNext<Token.Else>(expectedError("expected else"))
        val elseBranch = parseExpression()

        return Expression.If(condition, thenBranch, elseBranch, Span(ifSpan.start, elseBranch.span.end))
    }

    private fun <T> commaSeparated(parser: () -> T, cont: (Token) -> Boolean): List<T> {
        val result = mutableListOf<T>()

        while (cont(iterator.peek().value)) {
            result += parser()

            if (iterator.peek().value == Token.Comma) {
                iterator.next()
            } else {
                break
            }
        }

        return result
    }

    private fun expectedError(msg: String): (Spanned<Token>) -> String = { token ->
        "$msg saw ${token.value} at ${token.span}"
    }

    private inline fun <reified T> expectNext(error: (token: Spanned<Token>) -> String): Spanned<T> {
        val (span, token) = iterator.next()

        get<T>(token)?.let {
            return Spanned(span, it)
        } ?: throw RuntimeException(error(Spanned(span, token)))
    }
}