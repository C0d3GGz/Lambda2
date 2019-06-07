package lambda

import io.vavr.kotlin.hashMap
import org.junit.Test

import org.junit.Assert.*

class SubstitutionTest {

    @Test
    fun `should leave unknown vars untouched`() {
        val substitution = Substitution(
            hashMap(
                Ident("a") to Type.Int
            )
        )

        val result = substitution.apply(Type.Var(Ident("b")))

        assertEquals(Type.Var(Ident("b")), result)
    }

    @Test
    fun `should substitute a known var to int`() {
        val substitution = Substitution(
            hashMap(
                Ident("a") to Type.Int
            )
        )

        val result = substitution.apply(Type.Var(Ident("a")))

        assertEquals(Type.Int, result)
    }

    @Test
    fun `should substitute a function`() {
        val substitution = Substitution(
            hashMap(
                Ident("a") to Type.Int,
                Ident("b") to Type.Bool
            )
        )

        val result = substitution.apply(
            Type.Fun(
                Type.Var(Ident("a")),
                Type.Var(Ident("b"))
            )
        )

        assertEquals(Type.Fun(Type.Int, Type.Bool), result)
    }

    @Test
    fun `treat the empty substitution as identity`() {
        val empty = Substitution.empty
        val substitution = Substitution(hashMap(Ident("a") to Type.Int))

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
            Ident("b") to Type.Int
        ))

        val second = Substitution(hashMap(
            Ident("a") to Type.v("b")
        ))

        assertEquals(
            Substitution(hashMap(
                Ident("a") to Type.Int,
                Ident("b") to Type.Int
            )),
            first.compose(second)
        )
    }

    @Test
    fun `compose should merge right-biased`(){
        val first = Substitution(hashMap(
            Ident("b") to Type.Int
        ))

        val second = Substitution(hashMap(
            Ident("b") to Type.Bool
        ))

        assertEquals(
            Substitution(hashMap(
                Ident("b") to Type.Bool
            )),
            first.compose(second)
        )
    }
}