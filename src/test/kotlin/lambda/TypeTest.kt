package lambda

import io.vavr.kotlin.hashSet
import lambda.syntax.Name
import org.junit.Assert.assertEquals
import org.junit.Test

class TypeTest {

    @Test
    fun `return emptySet for primitives`() {
        assertEquals(hashSet<Name>(), Type.Int.freeVars())
        assertEquals(hashSet<Name>(), Type.Bool.freeVars())
    }

    @Test
    fun `return free vars for variables`() {
        assertEquals(hashSet(Name("a")), Type.Var(Name("a")).freeVars())
    }

    @Test
    fun `return free vars for functions`() {
        assertEquals(
            hashSet(Name("a"), Name("b")),
            Type.Fun(Type.v("a"), Type.v("b")).freeVars()
        )
    }
}