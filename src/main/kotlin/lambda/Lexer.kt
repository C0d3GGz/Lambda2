package lambda

import java.lang.RuntimeException

sealed class Token{
    override fun toString(): String = this.javaClass.simpleName
    companion object {
        fun getIdent(token: Token): Ident?{
            return when(token){
                is Ident -> token
                else -> null
            }
        }
        fun getLParen(token: Token): LParen?{
            return when(token){
                is LParen -> token
                else -> null
            }
        }
        fun getRParen(token: Token): RParen?{
            return when(token){
                is RParen -> token
                else -> null
            }
        }
        fun getLam(token: Token): Lam?{
            return when(token){
                is Lam -> token
                else -> null
            }
        }

        fun getDot(token: Token): Dot?{
            return when(token){
                is Dot -> token
                else -> null
            }
        }

        fun getIntToken(token: Token): IntToken?{
            return when(token){
                is IntToken -> token
                else -> null
            }
        }
        fun getBoolToken(token: Token): BoolToken?{
            return when(token){
                is BoolToken -> token
                else -> null
            }
        }
    }
}
object LParen : Token()
object RParen : Token()
object Lam : Token()
object Dot : Token()
data class Ident(val ident: String): Token()
data class IntToken(val int: Int): Token()
data class BoolToken(val bool: Boolean): Token()

class Lexer(input: String): Iterator<Token> {
    var iterator = PeekableIterator(input.iterator())
    init {
        consumeWhitespace()
    }

    override fun hasNext(): Boolean {
        return iterator.hasNext()
    }

    override fun next(): Token {
        return when(val c = iterator.next()){
            '(' -> LParen
            ')' -> RParen
            '\\' -> Lam
            '.' -> Dot
            else -> {
                if(c.isJavaIdentifierStart()) ident(c)
                else if (c.isDigit()) intLiteral(c)
                else throw RuntimeException()
            }
        }.also {
            consumeWhitespace()
        }
    }

    private fun intLiteral(startChar: Char): Token {
        var result: String = startChar.toString()
        while (iterator.hasNext() && iterator.peek().isDigit()){
            result += iterator.next()
        }
        return IntToken(result.toInt())
    }

    private fun consumeWhitespace(){
        while (iterator.hasNext() && iterator.peek().isWhitespace())
            iterator.next()
    }

    private fun ident(startChar: Char): Token {

        var result: String = startChar.toString()
        while (iterator.hasNext() && iterator.peek().isJavaIdentifierPart()){
            result += iterator.next()
        }

        return when (result) {
            "true" -> BoolToken(true)
            "false" -> BoolToken(false)
            else -> Ident(result)
        }
    }
}

class PeekableIterator<T>(private val iterator: Iterator<T>) : Iterator<T>{

    private var lookahead: T? = null

    override fun hasNext(): Boolean {
        return if (lookahead != null){
            true
        } else{
            iterator.hasNext()
        }
    }

    override fun next(): T {
        return if(lookahead != null){
            val temp  = lookahead!!
            lookahead = null
            temp
        } else
            iterator.next()
    }

    fun peek(): T{
        lookahead = lookahead ?: iterator.next()
        return lookahead!!
    }

}

