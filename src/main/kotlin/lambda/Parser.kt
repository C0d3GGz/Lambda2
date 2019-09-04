package lambda

import lambda.Token.Companion.get
import lambda.syntax.*


class Parser(tokens: Iterator<Spanned<Token>>) {

    val iterator = PeekableIterator(tokens)

    fun parseSourceFile(): SourceFile {
        val parsedDeclarations = mutableListOf<Declaration>()
        val startPosition = iterator.peek().span.start
        while (true) {
            if (iterator.peek().value == Token.EOF) break
            parsedDeclarations += parseDeclaration()
        }
        val endPosition = parsedDeclarations.lastOrNull()?.span?.end ?: startPosition
        return SourceFile(parsedDeclarations, Span(startPosition, endPosition))
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
        val scheme: Spanned<Scheme> = parseScheme()
        expectNext<Token.Equals>(expectedError("expected equals"))
        val expression = parseExpression()
        val semi = expectNext<Token.Semicolon>(expectedError("value declarations need to end with semicolon"))

        val span = Span(startPosition, semi.span.end)

        return Declaration.Value(name, scheme, expression, span)
    }

    private fun parseScheme(): Spanned<Scheme> {
        val vars = mutableListOf<Spanned<Name>>()
        val t = iterator.peek()
        val startPosition = t.span.start
        if (t.value is Token.Forall) {
            iterator.next()
            while (true) {
                vars += parseName()
                if (iterator.peek().value == Token.Dot) {
                    iterator.next()
                    break
                }
            }
        }
        val ty = parseType()
        val endPosition = ty.span.end

        return Spanned(Span(startPosition, endPosition), Scheme(vars.map { it.value }, ty.value))
    }


    fun parseTypeDeclaration(): Declaration.Type {
        val startPosition = iterator.next().span.start
        val name = parseUpperName()
        expectNext<Token.LBrace>(expectedError("expected open brace"))

        val dataConstructors = mutableListOf<DataConstructor>()
        val endPosition: Position

        while (true) {
            dataConstructors += parseDataConstructor()
            expectNext<Token.Comma>(expectedError("expected comma"))
            val peeked = iterator.peek()

            if (peeked.value == Token.RBrace) {
                endPosition = peeked.span.end
                iterator.next()
                break
            }
        }

        return Declaration.Type(name, dataConstructors, Span(startPosition, endPosition))
    }

    fun parseDataConstructor(): DataConstructor {
        val name = parseUpperName()
        val fields = mutableListOf<Type>()

        val endPos: Position

        expectNext<Token.LParen>(expectedError("expected left paren"))
        while (true) {
            if (iterator.peek().value == Token.RParen) {
                endPos = iterator.next().span.end
                break
            }

            fields += parseType().value
            expectNext<Token.Comma>(expectedError("expected comma"))
        }

        return DataConstructor(name.value, fields, Span(name.span.start, endPos))
    }


    fun parseType(): Spanned<Type> {
        val arg = parseTypeAtom()
        return when (iterator.peek().value) {
            is Token.Arrow -> {
                iterator.next()
                val res = parseType()
                Spanned(Span(arg.span.start, res.span.end), Type.Fun(arg, res))
            }
            else -> arg
        }
    }

    fun parseTypeAtom(): Spanned<Type> {
        val (start, nextToken) = iterator.peek()

        return when (nextToken) {
            is Token.LParen -> {
                iterator.next()

                val (_, type) = parseType()
                val (end, _) = expectNext<Token.RParen>(expectedError("missing closing paren"))

                Spanned(Span(start.start, end.end), type)
            }
            is Token.UpperIdent -> {
                val name = parseUpperName()
                Spanned(name.span, Type.Constructor(name))
            }
            is Token.Ident -> {
                val (span, name) = parseName()
                Spanned(span, Type.Var(name))
            }
            else -> throw RuntimeException("expected type found $nextToken at $start")
        }
    }

    fun parseName(): Spanned<Name> {
        val ident = expectNext<Token.Ident>(expectedError("expected identifier"))
        return Spanned(ident.span, Name(ident.value.ident))
    }

    fun parseUpperName(msg: String = "expected uppercase identifier"): Spanned<Name> {
        val ident = expectNext<Token.UpperIdent>(expectedError(msg))
        return Spanned(ident.span, Name(ident.value.ident))
    }

    fun parseVar(): Spanned<Expression.Var> {
        val (span, ident) = parseName()
        return Spanned(span, Expression.Var(ident))
    }

    fun parseInt(): Spanned<Expression.Literal> {
        val (span, intToken) = expectNext<Token.IntToken>(expectedError("expected int literal"))
        return Spanned(span, Expression.Literal(IntLit(intToken.int)))
    }

    fun parseBool(): Spanned<Expression.Literal> {
        val (span, boolToken) = expectNext<Token.BoolToken>(expectedError("expected boolean literal"))
        return Spanned(span, Expression.Literal(BoolLit(boolToken.bool)))
    }


    fun parseLambda(): Spanned<Expression.Lambda> {
        val (start, _) = iterator.next()

        val binder = parseName()
        expectNext<Token.Dot>(expectedError("expected dot"))

        val body = parseExpression()

        return Spanned(Span(start.start, body.span.end), Expression.Lambda(binder, body))
    }


    fun parseExpression(): Spanned<Expression> {
        val atoms = mutableListOf<Spanned<Expression>>()
        while (iterator.hasNext()) {
            parseAtom()?.let {
                atoms += it
            } ?: break
        }

        return when {
            atoms.isEmpty() -> throw RuntimeException("failed to parse expression")
            atoms.size == 1 -> atoms.first()
            else -> atoms.drop(1).fold(atoms[0]) { func, arg ->
                Spanned(Span(func.span.start, arg.span.end), Expression.App(func, arg))
            }
        }

    }

    private fun parseAtom(): Spanned<Expression>? {
        return when (iterator.peek().value) {
            is Token.LParen -> {
                val (start, _) = iterator.next()
                var (exprSpan, expr) = parseExpression()
                if (iterator.peek().value == Token.Colon) {
                    iterator.next()
                    val ty = parseType()
                    expr = Expression.Typed(Spanned(exprSpan, expr), ty)
                }
                val (end, _) = expectNext<Token.RParen>(expectedError("missing closing paren"))

                Spanned(Span(start.start, end.end), expr)
            }
            is Token.Lam -> parseLambda()
            is Token.Ident -> parseVar()
            is Token.UpperIdent -> parseDataConstruction()
            is Token.IntToken -> parseInt()
            is Token.BoolToken -> parseBool()
            is Token.Let -> parseLet()
            is Token.If -> parseIf()
            else -> null
        }
    }

    // Color::Red()
    private fun parseDataConstruction(): Spanned<Expression.Construction> {
        val type = parseUpperName("expected type name")
        expectNext<Token.DoubleColon>(expectedError("expected double colon"))
        val dtor = parseUpperName("expected data constructor")
        expectNext<Token.LParen>(expectedError("expected open paren"))

        val exprs = mutableListOf<Spanned<Expression>>()
        val endPos: Position

        while (true) {
            if (iterator.peek().value == Token.RParen) {
                endPos = iterator.next().span.end
                break
            }

            exprs += parseExpression()
            expectNext<Token.Comma>(expectedError("expected comma"))
        }

        return Spanned(Span(type.span.start, endPos), Expression.Construction(type, dtor, exprs))
    }

    private fun parseLet(): Spanned<Expression.Let> {
        val (letSpan, _) = iterator.next()
        val binder = parseName()
        expectNext<Token.Equals>(expectedError("expected equals"))
        val expr = parseExpression()
        expectNext<Token.In>(expectedError("expected in"))
        val body = parseExpression()

        return Spanned(Span(letSpan.start, body.span.end), Expression.Let(binder, expr, body))
    }

    private fun parseIf(): Spanned<Expression.If> { // if true then 3 else 4
        val (ifSpan, _) = iterator.next()
        val condition = parseExpression()
        expectNext<Token.Then>(expectedError("expected then"))
        val thenBranch = parseExpression()
        expectNext<Token.Else>(expectedError("expected else"))
        val elseBranch = parseExpression()

        return Spanned(Span(ifSpan.start, elseBranch.span.end), Expression.If(condition, thenBranch, elseBranch))
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