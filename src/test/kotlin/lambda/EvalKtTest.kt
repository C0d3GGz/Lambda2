package lambda

import org.junit.Test

import org.junit.Assert.*

class EvalKtTest {


    @Test
    fun freeVars() {
        fvtc("x y z", listOf("x", "y", "z"))
        fvtc("\\x.x z", listOf("z"))
        fvtc("(\\x.x) x y z", listOf("x", "y", "z"))
    }

    private fun fvtc(input: String, expectedOutput: List<String>) {
        val parser = Parser(Lexer(input))
        assertEquals(parser.parseExpression().freeVars(), expectedOutput.map(::Ident).toSet())
    }
}