package lambda

sealed class Type {
    object Int : Type()
    object Bool : Type()
    data class Var(val ident: Ident): Type()
    data class Fun(val arg: Type, val result: Type) : Type()
}

data class Scheme(val vars: List<Ident>, val ty: Type)