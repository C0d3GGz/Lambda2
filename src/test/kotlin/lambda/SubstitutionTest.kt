package lambda

import io.vavr.kotlin.hashMap
import lambda.syntax.Name
import org.junit.Test

import org.junit.Assert.*

class SubstitutionTest {

    @Test
    fun `should leave unknown vars untouched`() {
        val substitution = Substitution(
            hashMap(
                1 to Type.Int
            )
        )

        val result = substitution.apply(Type.Unknown(2))

        assertEquals(Type.Unknown(2), result)
    }

    @Test
    fun `should substitute a known var to int`() {
        val substitution = Substitution(
            hashMap(
                1 to Type.Int
            )
        )

        val result = substitution.apply(Type.Unknown(1))

        assertEquals(Type.Int, result)
    }

    @Test
    fun `should substitute a function`() {
        val substitution = Substitution(
            hashMap(
                1 to Type.Int,
                2 to Type.Bool
            )
        )

        val result = substitution.apply(
            Type.Fun(
                Type.Unknown(1),
                Type.Unknown(2)
            )
        )

        assertEquals(Type.Fun(Type.Int, Type.Bool), result)
    }

    @Test
    fun `treat the empty substitution as identity`() {
        val empty = Substitution.empty
        val substitution = Substitution(hashMap(1 to Type.Int))

        assertEquals(
            substitution,
            substitution.compose(empty)
        )

        assertEquals(
            substitution,
            empty.compose(substitution)
        )
    }

    @Test
    fun `compose should transitively apply substitutions`() {
        val first = Substitution(hashMap(
            2 to Type.Int
        ))

        val second = Substitution(hashMap(
            1 to Type.Unknown(2)
        ))

        assertEquals(
            Substitution(hashMap(
                1 to Type.Int,
                2 to Type.Int
            )),
            first.compose(second)
        )
    }

    @Test
    fun `compose should merge right-biased`(){
        val first = Substitution(hashMap(
            2 to Type.Int
        ))

        val second = Substitution(hashMap(
            2 to Type.Bool
        ))

        assertEquals(
            Substitution(hashMap(
                2 to Type.Bool
            )),
            first.compose(second)
        )
    }
}