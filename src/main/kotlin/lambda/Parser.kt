package lambda

import lambda.Token.Companion.getBoolToken
import lambda.Token.Companion.getDot
import lambda.Token.Companion.getIdent
import lambda.Token.Companion.getIntToken
import lambda.Token.Companion.getLam
import lambda.Token.Companion.getRParen


class Parser(tokens: Iterator<SpannedToken>) {

    val iterator = PeekableIterator(tokens)

    fun parseType(): Spanned<Type> {
        val arg = parseTypeAtom()
        return when (iterator.peek().token) {
            is Arrow -> {
                iterator.next()
                val res = parseType()
                Spanned(Span(arg.span.start, res.span.end), Type.Fun(arg, res))
            }
            else -> arg
        }
    }

    fun parseTypeAtom(): Spanned<Type> {
        val (start, token) = iterator.peek()

        return when (token) {
            is LParen -> {
                iterator.next()

                val (_, type) = parseType()
                val (end, _) = expectNext(::getRParen) { token ->
                    "missing closing paren saw $token"
                }

                Spanned(Span(start.start, end.end), type)
            }
            is Ident -> {
                val (_, ident) = expectNext(::getIdent) { token ->
                    "expected identifier saw $token"
                }

                val type = when (ident.ident) {
                    "Int" -> Type.Int
                    "Bool" -> Type.Bool
                    else -> Type.Var(ident)
                }

                Spanned(start, type)
            }
            else -> throw RuntimeException("expected type found $token")
        }
    }

    fun parseVar(): Spanned<Expression.Var> {
        val (span, ident) = expectNext(::getIdent) { token ->
            "expected identifier saw $token"
        }
        return Spanned(span, Expression.Var(ident))
    }

    fun parseInt(): Spanned<Expression.Literal> {
        val (span, intToken) = expectNext(::getIntToken) { token ->
            "expected int literal saw $token"
        }
        return Spanned(span, Expression.Literal(IntLit(intToken.int)))
    }

    fun parseBool(): Spanned<Expression.Literal> {
        val (span, boolToken) = expectNext(::getBoolToken) { token ->
            "expected boolean literal saw $token"
        }
        return Spanned(span, Expression.Literal(BoolLit(boolToken.bool)))
    }


    fun parseLambda(): Spanned<Expression.Lambda> {
        val (start, _) = expectNext(::getLam) { token ->
            "expected lambda saw $token"
        }

        val binder = expectNext(::getIdent) { token ->
            "expected identifier saw $token"
        }

        expectNext(::getDot) { token ->
            "expected dot saw $token"
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
        return when (iterator.peek().token) {
            is LParen -> {
                val (start, _) = iterator.next()
                var (exprSpan , expr) = parseExpression()
                if (iterator.peek().token == Colon) {
                    iterator.next()
                    val ty = parseType()
                    expr = Expression.Typed(Spanned(exprSpan, expr), ty)
                }
                val (end, _) = expectNext(::getRParen) { token ->
                    "missing closing paren saw $token"
                }

                Spanned(Span(start.start, end.end), expr)
            }
            is Lam -> parseLambda()
            is Ident -> parseVar()
            is IntToken -> parseInt()
            is BoolToken -> parseBool()
            else -> null
        }
    }

    private fun <T> expectNext(pred: (token: Token) -> T?, error: (token: Token?) -> String): Spanned<T> {
        if (!iterator.hasNext()) {
            throw RuntimeException(error(null))
        }
        val (span, token) = iterator.next()

        pred(token)?.let {
            return Spanned(span, it)
        } ?: throw RuntimeException(error(token))
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