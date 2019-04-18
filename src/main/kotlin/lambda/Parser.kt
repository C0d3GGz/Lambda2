package lambda

import lambda.Token.Companion.getDot
import lambda.Token.Companion.getIdent
import lambda.Token.Companion.getLam
import lambda.Token.Companion.getRParen
import java.lang.RuntimeException


class Parser(tokens: Iterator<Token>){

    val iterator = PeekableIterator(tokens)

    fun parseVar(): Var {
        val ident = expectNext(::getIdent) {
            token -> "expected identifier saw $token"
        }
        return Var(ident)
    }

    fun parseLambda() : Lambda {

        expectNext(::getLam) {
            token -> "expected lambda saw $token"
        }

        val binder = expectNext(::getIdent) {
            token -> "expected identifier saw $token"
        }

        expectNext(::getDot) {
            token -> "expected dot saw $token"
        }

        val body = parseExpression()

        return Lambda(binder, body)
    }


    fun parseExpression(): Expression {
        val atoms = mutableListOf<Expression>()
        while (iterator.hasNext()){
            parseAtom()?.let {
                atoms += it
            } ?: break
        }

        return when {
            atoms.isEmpty() -> throw RuntimeException("failed to parse expression")
            atoms.size == 1 -> atoms.first()
            else -> atoms.drop(1).fold(atoms[0], ::App)
        }

    }

    private fun parseAtom(): Expression? {
        return when(iterator.peek()) {
            is LParen -> {
                iterator.next()
                parseExpression().also {
                    expectNext(::getRParen){ token ->
                        "missing closing paren saw $token"
                    }
                }
            }
            is Lam -> parseLambda()
            is Ident -> parseVar()
            else -> null
        }
    }

    private fun <T>expectNext(pred : (token: Token) -> T?, error: (token: Token?) -> String): T{
        if (!iterator.hasNext()) {
            throw RuntimeException(error(null))
        }
        val token = iterator.next()
        pred(token)?.let {
            return it
        } ?: throw RuntimeException(error(token))
    }
}

fun main(args: Array<String>) {
    testParse("(\\x. x) y")
    testParse("(\\x. x y)")
    testParse("(\\y. \\x. x y) x")
    testParse("(\\y. \\x. x y) z (\\k. k)")
    testEval("(\\y. (\\x. y)) (\\k. k) (\\v. v)")
    testParse("(\\y. (\\x. y)) (\\k. k) (\\v. v)")
}

fun testEval(input: String) {
    println("input: $input")
    val parser = Parser(Lexer(input))
    print("evaled: ")
    val parsed = parser.parseExpression()
    println(evalExpr(parsed).pretty())
}

fun testParse(input: String){
    println("input: $input")
    val parser = Parser(Lexer(input))
    print("evaled: ")
    val parsed = parser.parseExpression()
    println(Eval().eval(parsed).pretty())
}