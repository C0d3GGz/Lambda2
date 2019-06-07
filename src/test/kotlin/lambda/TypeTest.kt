package lambda

import io.vavr.kotlin.hashSet
import org.junit.Test

import org.junit.Assert.*

class TypeTest {

    @Test
    fun `return emptySet for primitives`() {
        assertEquals(hashSet<Ident>(), Type.Int.freeVars())
        assertEquals(hashSet<Ident>(), Type.Bool.freeVars())
    }

    @Test
    fun `return free vars for variables`() {
        assertEquals(hashSet(Ident("a")), Type.Var(Ident("a")).freeVars())
    }

    @Test
    fun `return free vars for functions`() {
        assertEquals(
            hashSet(Ident("a"), Ident("b")),
            Type.Fun(Type.v("a"), Type.v("b")).freeVars()
        )
    }
}