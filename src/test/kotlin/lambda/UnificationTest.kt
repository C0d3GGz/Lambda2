package lambda


import io.vavr.kotlin.hashMap
import org.junit.Test

import org.junit.Assert.*
import kotlin.RuntimeException

class UnificationTest {

    private fun unify(ty1: Type, ty2: Type): Substitution {
        val typechecker = Typechecker()
        return typechecker.unify(ty1, ty2).fold({
            throw RuntimeException(it.pretty())
        }, { it })
    }

    @Test
    fun `int unifies with int`() {
        assertEquals(Substitution.empty, unify(Type.Int, Type.Int))
    }

    @Test
    fun `bool unifies with bool`() {
        assertEquals(Substitution.empty, unify(Type.Bool, Type.Bool))
    }

    @Test(expected = RuntimeException::class)
    fun `int does not unify with bool`() {
        unify(Type.Int, Type.Bool)
    }

    @Test
    fun `var unifies with int`() {
        assertEquals(
            Substitution(hashMap(Ident("a") to Type.Int)),
            unify(Type.v("a"), Type.Int)
        )
    }

    @Test
    fun `int unifies with var`() {
        assertEquals(
            Substitution(hashMap(Ident("a") to Type.Int)),
            unify(Type.Int, Type.v("a"))
        )
    }

    @Test
    fun `same vars unifies to empty`() {
        assertEquals(Substitution.empty, unify(Type.v("a"), Type.v("a")))
    }

    @Test(expected = RuntimeException::class)
    fun `unification performs the occurs check`() {
        unify(Type.Fun(Type.v("a"), Type.v("a")), Type.v("a"))
    }

    @Test
    fun `functions unify by unifying their respective argument and result types`() {
        assertEquals(
            Substitution(hashMap(Ident("a") to Type.Bool)),
            unify(Type.Fun(Type.Int, Type.v("a")), Type.Fun(Type.Int, Type.Bool))
        )
    }

    @Test
    fun `information found while unifying argument types gets propagated to the result types`() {
        assertEquals(
            Substitution(hashMap(Ident("a") to Type.Int, Ident("b") to Type.Int)),
            unify(Type.Fun(Type.v("a"), Type.v("a")), Type.Fun(Type.Int, Type.v("b")))
        )
    }

}