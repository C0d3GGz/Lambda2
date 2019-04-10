package lambda

fun Expression.freeVars(): Set<Ident>{
    return when(this){
        is Var -> hashSetOf(ident)
        is Lamdba -> body.freeVars().filter { it != binder }.toSet()
        is App -> func.freeVars().union(arg.freeVars())
    }
}
