package lambda

sealed class Token {
    override fun toString(): String = this.javaClass.simpleName

    companion object {
        fun getIdent(token: Token): Ident? {
            return when (token) {
                is Ident -> token
                else -> null
            }
        }

        fun getLParen(token: Token): LParen? {
            return when (token) {
                is LParen -> token
                else -> null
            }
        }

        fun getRParen(token: Token): RParen? {
            return when (token) {
                is RParen -> token
                else -> null
            }
        }

        fun getLam(token: Token): Lam? {
            return when (token) {
                is Lam -> token
                else -> null
            }
        }

        fun getDot(token: Token): Dot? {
            return when (token) {
                is Dot -> token
                else -> null
            }
        }

        fun getIntToken(token: Token): IntToken? {
            return when (token) {
                is IntToken -> token
                else -> null
            }
        }

        fun getBoolToken(token: Token): BoolToken? {
            return when (token) {
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
object Colon : Token()
object Arrow : Token()
data class Ident(val ident: String) : Token()
data class IntToken(val int: Int) : Token()
data class BoolToken(val bool: Boolean) : Token()

data class Position(val line: Int, val column: Int) {
    fun shift(n: Int) = copy(column = column + n)
}

data class Span(val start: Position, val end: Position) {
    companion object {
        val DUMMY = Span(Position(-1, -1), Position(-1, -1))
    }
}
data class Spanned<out T>(val span: Span, val value: T) {
    // This is a bit dodgy... but any other option is too tedious
    override fun equals(other: Any?): Boolean {
        if (other is Spanned<*>) {
            return this.value == other.value
        }
        return false
    }

    override fun hashCode(): Int {
        return value?.hashCode() ?: 0
    }
}
data class SpannedToken(val span: Span, val token: Token)

class Lexer(input: String) : Iterator<SpannedToken> {
    var iterator = CharLocations(input.iterator())

    init {
        consumeWhitespace()
    }

    override fun hasNext(): Boolean {
        return iterator.hasNext()
    }

    override fun next(): SpannedToken {
        val start = iterator.position

        val (token, length) = when (val c = iterator.next()) {
            '(' -> LParen to 1
            ')' -> RParen to 1
            '\\' -> Lam to 1
            '.' -> Dot to 1
            ':' -> Colon to 1
            '-' -> if (iterator.next() == '>') Arrow to 2 else {
                throw RuntimeException()
            }
            else -> {
                if (c.isJavaIdentifierStart()) ident(c)
                else if (c.isDigit()) intLiteral(c)
                else throw RuntimeException()
            }
        }.also {
            consumeWhitespace()
        }

        return SpannedToken(Span(start, start.shift(length)), token)
    }

    private fun intLiteral(startChar: Char): Pair<Token, Int> {
        var result: String = startChar.toString()
        while (iterator.hasNext() && iterator.peek().isDigit()) {
            result += iterator.next()
        }
        return IntToken(result.toInt()) to result.length
    }

    private fun consumeWhitespace() {
        while (iterator.hasNext() && iterator.peek().isWhitespace())
            iterator.next()
    }

    private fun ident(startChar: Char): Pair<Token, Int> {
        var result: String = startChar.toString()
        while (iterator.hasNext() && iterator.peek().isJavaIdentifierPart()) {
            result += iterator.next()
        }

        return when (result) {
            "true" -> BoolToken(true)
            "false" -> BoolToken(false)
            else -> Ident(result)
        } to result.length
    }
}

class PeekableIterator<T>(private val iterator: Iterator<T>) : Iterator<T> {

    private var lookahead: T? = null

    override fun hasNext(): Boolean {
        return if (lookahead != null) {
            true
        } else {
            iterator.hasNext()
        }
    }

    override fun next(): T {
        return if (lookahead != null) {
            val temp = lookahead!!
            lookahead = null
            temp
        } else
            iterator.next()
    }

    fun peek(): T {
        lookahead = lookahead ?: iterator.next()
        return lookahead!!
    }
}

class CharLocations(private val iterator: Iterator<Char>) : Iterator<Char> {

    private var lookahead: Char? = null
    var position = Position(1, 0)

    override fun hasNext(): Boolean {
        return if (lookahead != null) {
            true
        } else {
            iterator.hasNext()
        }
    }

    override fun next(): Char {
        return if (lookahead != null) {
            val temp = lookahead!!
            lookahead = null
            nextChar(temp)
        } else
            nextChar(iterator.next())
    }

    fun peek(): Char {
        lookahead = lookahead ?: iterator.next()
        return lookahead!!
    }

    private fun nextChar(c: Char): Char {
        position = if (c == '\n') {
            Position(position.line + 1, 0)
        } else {
            Position(position.line, position.column + 1)
        }

        return c
    }
}