package lambda

import lambda.Token.Companion.get


class Parser(tokens: Iterator<Spanned<Token>>) {

    val iterator = PeekableIterator(tokens)

    fun parseType(): Spanned<Type> {
        val arg = parseTypeAtom()
        return when (iterator.peek()?.value) {
            is Arrow -> {
                iterator.next()
                val res = parseType()
                Spanned(Span(arg.span.start, res.span.end), Type.Fun(arg, res))
            }
            else -> arg
        }
    }

    fun parseTypeAtom(): Spanned<Type> {
        val (start, token) = iterator.peek() ?: throw RuntimeException("Saw EOF, expected type")

        return when (token) {
            is LParen -> {
                iterator.next()

                val (_, type) = parseType()
                val (end, _) = expectNext<LParen> { token ->
                    "missing closing paren saw ${token?.value} at ${token?.span}"
                }

                Spanned(Span(start.start, end.end), type)
            }
            is Ident -> {
                val (_, ident) = expectNext<Ident> { token ->
                    "expected identifier saw ${token?.value} at ${token?.span}"
                }

                val type = when (ident.ident) {
                    "Int" -> Type.Int
                    "Bool" -> Type.Bool
                    else -> Type.Var(ident)
                }

                Spanned(start, type)
            }
            else -> throw RuntimeException("expected type found $token at $start")
        }
    }

    fun parseVar(): Spanned<Expression.Var> {
        val (span, ident) = expectNext<Ident> { token ->
            "expected identifier saw ${token?.value} at ${token?.span}"
        }
        return Spanned(span, Expression.Var(ident))
    }

    fun parseInt(): Spanned<Expression.Literal> {
        val (span, intToken) = expectNext<IntToken> { token ->
            "expected int literal saw $token"
        }
        return Spanned(span, Expression.Literal(IntLit(intToken.int)))
    }

    fun parseBool(): Spanned<Expression.Literal> {
        val (span, boolToken) = expectNext<BoolToken> { token ->
            "expected boolean literal saw $token"
        }
        return Spanned(span, Expression.Literal(BoolLit(boolToken.bool)))
    }


    fun parseLambda(): Spanned<Expression.Lambda> {
        val (start, _) = expectNext<Lam> { token ->
            "expected lambda saw $token"
        }

        val binder = expectNext<Ident> { token ->
            "expected identifier saw ${token?.value} at ${token?.span}"
        }

        expectNext<Dot> { token ->
            "expected dot saw ${token?.value} at ${token?.span}"
        }

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
            is LParen -> {
                val (start, _) = iterator.next()
                var (exprSpan, expr) = parseExpression()
                if (iterator.peek().value == Colon) {
                    iterator.next()
                    val ty = parseType()
                    expr = Expression.Typed(Spanned(exprSpan, expr), ty)
                }
                val (end, _) = expectNext<RParen> { token ->
                    "missing closing paren saw ${token?.value} at ${token?.span}"
                }

                Spanned(Span(start.start, end.end), expr)
            }
            is Lam -> parseLambda()
            is Ident -> parseVar()
            is IntToken -> parseInt()
            is BoolToken -> parseBool()
            is Let -> parseLet()
            is If -> parseIf()
            else -> null
        }
    }

    private fun expectedError(msg: String): (Spanned<Token>?) -> String = { token ->
        "$msg saw ${token?.value} at ${token?.span}"
    }

    private fun parseLet(): Spanned<Expression.Let>? {
        val (letSpan, _) = iterator.next()
        val binder = expectNext<Ident>(expectedError("expected binder"))
        expectNext<Equals>(expectedError("expected equals"))
        val expr = parseExpression()
        expectNext<In>(expectedError("expected in"))
        val body = parseExpression()

        return Spanned(Span(letSpan.start, body.span.end), Expression.Let(binder, expr, body))
    }

    private fun parseIf(): Spanned<Expression.If>? { // if true then 3 else 4
        val (ifSpan, _) = iterator.next()
        val condition = parseExpression()
        expectNext<Then>(expectedError("expected then"))
        val thenBranch = parseExpression()
        expectNext<Else>(expectedError("expected else"))
        val elseBranch = parseExpression()

        return Spanned(Span(ifSpan.start, elseBranch.span.end), Expression.If(condition, thenBranch, elseBranch))
    }

    private fun <T> expectNext(error: (token: Spanned<Token>?) -> String): Spanned<T> {
        if (!iterator.hasNext()) {
            throw RuntimeException(error(null))
        }
        val (span, token) = iterator.next()

        get<T>(token)?.let {
            return Spanned(span, it)
        } ?: throw RuntimeException(error(Spanned(span, token)))
    }
}

fun main(args: Array<String>) {
    testParse("(\\x. x) y")
    testParse("(\\x. x y)")
    testParse("(\\y. \\x. x y) x")
    testParse("(\\y. \\x. x y) z (\\k. k)")
    testEval("(\\y. (\\x. y)) 2 1")
    testParse("(\\y. (\\x. y)) 2 1")
    testEval("(\\f. \\g. add (f 1) (g 2)) (add 10) (add 5)")
    testEval("add 1 2")
    testEval("add 1 (add 1 2)")
    testEval("add (add 4 1) (add 1 2)")
    testEval("(\\f. add (f 1) (f 2)) (add 10)")
}

fun testEval(input: String) {
    println("input: $input")
    val parser = Parser(Lexer(input))
    print("evaled: ")
    val parsed = parser.parseExpression()
    println(evalExpr(parsed.value).pretty())
}

fun testParse(input: String) {
    println("input: $input")
    val parser = Parser(Lexer(input))
    print("evaled: ")
    val parsed = parser.parseExpression()
    println(Eval().eval(EvalExpression.fromExpr(parsed.value)).pretty())
}