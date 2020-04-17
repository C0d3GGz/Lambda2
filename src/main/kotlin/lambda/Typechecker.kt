package lambda

import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf
import lambda.syntax.*

data class TypeInfo(val typeArgs: List<TyVar>, val dtors: List<DataConstructor>) {
    companion object {
        val empty = TypeInfo(emptyList(), emptyList())
    }
}

data class Interface(val types: HashMap<Name, TypeInfo>, val values: HashMap<Name, Scheme>)

val primInterface = Namespace.prim to Interface(
    hashMapOf(
        Name("Int") to TypeInfo.empty,
        Name("Bool") to TypeInfo.empty,
        Name("String") to TypeInfo.empty,
        Name("Unit") to TypeInfo.empty
    ),
    hashMapOf(
        Name("add") to Scheme(
            emptyList(),
            Type.Fun(
                Type.Int,
                Type.Fun(Type.Int, Type.Int)
            )
        ),
        Name("sub") to Scheme(
            emptyList(),
            Type.Fun(
                Type.Int,
                Type.Fun(Type.Int, Type.Int)
            )
        ),
        Name("eq") to Scheme(
            emptyList(),
            Type.Fun(
                Type.Int,
                Type.Fun(Type.Int, Type.Bool)
            )
        ),
        Name("concat") to Scheme(
            emptyList(),
            Type.Fun(
                Type.String,
                Type.Fun(Type.String, Type.String)
            )
        ),
        Name("int_to_string") to Scheme(
            emptyList(),
            Type.Fun(
                Type.Int,
                Type.String
            )
        ),
        Name("fix") to Scheme(
            listOf(TyVar(Name("a"))),
            Type.Fun(
                Type.Fun(Type.v("a"), Type.v("a")),
                Type.v("a")
            )
        ),
        Name("sleep") to Scheme(
            emptyList(),
            Type.Fun(
                Type.Int,
                Type.Unit
            )
        ),
        Name("print") to Scheme(
            emptyList(),
            Type.Fun(
                Type.String,
                Type.Unit
            )
        ),
        Name("clear") to Scheme(
            emptyList(),
            Type.Fun(
                Type.Unit,
                Type.Unit
            )
        )
    )
)

fun <T, U> T?.fold(empty: U, f: (T) -> U) = this?.let(f) ?: empty

typealias Namespaced = Pair<Namespace, Name>
typealias TCValueContext = PersistentMap<Namespaced, Scheme> // List::head : forall a. List<a> -> Option::Option<a>
typealias TCTypeContext = PersistentMap<Namespaced, TypeInfo> // List::List: listOf(Cons(a), Nil())

data class Substitution(val subst: HashMap<Int, Type>) {
    operator fun get(u: Int): Type? = subst[u]

    operator fun set(u: Int, type: Type) {
        subst[u] = type
    }

    fun apply(type: Type): Type {
        return when (type) {
            is Type.ErrorSentinel -> type
            is Type.Var -> type
            is Type.Constructor -> type.copy(tyArgs = type.tyArgs.map(::apply))
            is Type.Fun -> Type.Fun(apply(type.arg), apply(type.result))
            is Type.Unknown -> subst[type.u].fold(type, ::apply)
        }
    }

    fun apply(scheme: Scheme): Scheme {
        return Scheme(scheme.vars, apply(scheme.ty))
    }

    fun apply(case: Expression.Case): Expression.Case {
        return case.copy(body = apply(case.body))
    }

    fun apply(expr: Expression): Expression {
        return when (expr) {
            is Expression.Literal, is Expression.Var -> expr
            is Expression.Lambda -> Expression.Lambda(expr.binder, apply(expr.body), expr.sp)
            is Expression.App -> Expression.App(apply(expr.func), apply(expr.arg), expr.sp)
            is Expression.Typed -> apply(expr)
            is Expression.Let -> Expression.Let(
                expr.recursive,
                expr.binder,
                expr.scheme?.let { apply(it) },
                apply(expr.expr),
                apply(expr.body),
                expr.sp
            )
            is Expression.If -> Expression.If(
                apply(expr.condition),
                apply(expr.thenBranch),
                apply(expr.elseBranch),
                expr.sp
            )
            is Expression.Construction -> expr.copy(exprs = expr.exprs.map(::apply))
            is Expression.Match -> Expression.Match(apply(expr.expr), expr.cases.map(::apply), expr.sp)
        }
    }

    fun apply(expr: Expression.Typed): Expression.Typed =
        Expression.Typed(apply(expr.expr), apply(expr.type), expr.sp)

    override fun toString(): String {
        return "{ " + subst.toList().joinToString("\n, ") { (u, ty) -> "u$u ↦ ${ty.pretty()}" } + "\n}"
    }

    companion object {
        val empty = Substitution(hashMapOf())
    }
}

sealed class TypeError : Exception() {
    open var span: Span? = null

    data class Unification(val ty1: Type, val ty2: Type, val stack: MutableList<Pair<Type, Type>>) : TypeError()
    data class UnknownVar(val name: Namespaced, override var span: Span?) : TypeError()
    data class UnknownType(val name: Namespaced) : TypeError()
    data class UnknownDtor(val name: Namespaced) : TypeError()
    data class OccursCheck(val u: Int, val type: Type) : TypeError()
    data class IfCondition(val type: Type, override var span: Span?) : TypeError()
    object Followup : TypeError()

    fun pretty(): String = when (this) {
        is Unification ->
            """Failed to match ${ty1.pretty()} with ${ty2.pretty()}
  ${stack.joinToString("\n  ") { (t1, t2) -> "while trying to match ${t1.pretty()} with ${t2.pretty()}" }}"""
        is UnknownVar -> "Unknown var $name"
        is UnknownType -> "Unknown type $name"
        is UnknownDtor -> "Type ${name.first} does not have a constructor named ${name.second}"
        is OccursCheck -> "Failed to infer the infinite type u$u ~ ${type.pretty()}"
        is IfCondition -> "Condition should be of type Bool but was ${type.pretty()}"
        is Followup -> "Followup error, you shouldn't be seeing this"
    }

    override fun toString(): String = this.pretty()
}

class Typechecker {

    private var fresh: Int = 0

    private val substitution: Substitution = Substitution.empty

    private var typeCtx: TCTypeContext = persistentHashMapOf()

    private var namespace = Namespace.local

    fun freshUnknown(): Type.Unknown = Type.Unknown(++fresh)

    fun instantiate(scheme: Scheme): Type {
        val mappings = scheme.vars.map { it to freshUnknown() }
        return scheme.ty.substMany(mappings)
    }

    fun unify(t1: Type, t2: Type) {
        val t1 = zonk(t1)
        val t2 = zonk(t2)

        when {
            t1 == t2 -> return
            t1 is Type.Unknown -> varBind(t1.u, t2)
            t2 is Type.Unknown -> varBind(t2.u, t1)
            t1 is Type.Fun && t2 is Type.Fun -> try {
                unify(t1.arg, t2.arg)
                unify(t1.result, t2.result)
            } catch (err: TypeError.Unification) {
                err.stack.add(t1 to t2)
                throw err
            }
            t1 is Type.Constructor &&
                    t2 is Type.Constructor &&
                    t1.name == t2.name &&
                    t1.namespace == t2.namespace &&
                    t1.tyArgs.size == t2.tyArgs.size -> try {
                t1.tyArgs.zip(t2.tyArgs).forEach { unify(it.first, it.second) }
            } catch (err: TypeError.Unification) {
                err.stack.add(t1 to t2)
                throw err
            }
            else -> throw TypeError.Unification(t1, t2, mutableListOf())
        }
    }

    private fun zonk(type: Type): Type = substitution.apply(type)

    private fun zonk(type: Expression.Typed): Expression.Typed = substitution.apply(type)

    private fun varBind(u: Int, type: Type) {
        return if (type.unknowns().contains(u)) {
            throw TypeError.OccursCheck(u, type)
        } else {
            substitution[u] = type
        }
    }

    fun <T> withSpannedError(span: Span, action: () -> T): T =
        try {
            action()
        } catch (err: TypeError) {
            err.span = err.span ?: span
            throw err
        }

    fun reset() {
        fresh = 0
        substitution.subst.clear()
    }

    fun subsumes(lhs: Scheme, rhs: Scheme) {
        unify(instantiate(lhs), rhs.ty)
    }

    fun infer(ctx: TCValueContext, expr: Expression): Expression.Typed {
        val span = expr.span
        val tyWrap: (Expression, Type) -> Expression.Typed = { e, ty -> Expression.Typed(e, ty, span) }

        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is Lit.Int -> Type.Int
                    is Lit.Bool -> Type.Bool
                    is Lit.String -> Type.String
                }

                tyWrap(expr, t)
            }
            is Expression.Lambda -> {
                val tyBinder = freshUnknown()

                val body = withSpannedError(expr.body.span) {
                    this.infer(ctx.put(Namespace.local to expr.binder, Scheme.fromType(tyBinder)), expr.body)
                }

                tyWrap(
                    Expression.Lambda(expr.binder, body, span),
                    Type.Fun(tyBinder, body.type)
                )
            }
            is Expression.Var -> {
                val namespaced = expr.namespace to expr.name
                val scheme = ctx[namespaced] ?: throw TypeError.UnknownVar(namespaced, span)

                // If we try to look up a value that failed to type check before we immediately bail out
                if (scheme.ty is Type.ErrorSentinel) throw TypeError.Followup
                tyWrap(expr, instantiate(scheme))
            }
            is Expression.App -> {
                val tyRes = freshUnknown()
                val func = infer(ctx, expr.func)
                val arg = infer(ctx, expr.arg)

                withSpannedError(arg.span) { unify(func.type, Type.Fun(arg.type, tyRes)) }
                tyWrap(Expression.App(func, arg, span), tyRes)
            }
            is Expression.Typed -> {
                val tyExpr = infer(ctx, expr.expr)
                if (!expr.type.freeVars().isEmpty) { // TODO check wellformedness
                    throw RuntimeException("not allowed")
                }

                withSpannedError(span) { unify(tyExpr.type, expr.type) }
                tyExpr
            }
            is Expression.Let -> {
                val tyBinder = if (expr.recursive) {
                    val binderUnknown = freshUnknown()
                    val recCtx = ctx.put(Namespace.local to expr.binder, Scheme.fromType(binderUnknown))
                    val binder =
                        infer(recCtx, expr.expr)
                    unify(binderUnknown, binder.type)
                    binder
                } else {
                    infer(ctx, expr.expr)
                }

                // TODO allow generalization?
                /*if (expr.scheme != null) {
                    subsumes(generalize(tyBinder.type, ctx), expr.scheme)
                    tmpCtx[expr.binder] = expr.scheme
                } else {
                    tmpCtx[expr.binder] = Scheme.fromType(tyBinder.type)
                }*/

                val bodyCtx = ctx.put(Namespace.local to expr.binder, Scheme.fromType(tyBinder.type))
                val tyBody = infer(bodyCtx, expr.body)

                tyWrap(Expression.Let(expr.recursive, expr.binder, expr.scheme, tyBinder, tyBody, span), tyBody.type)
            }
            is Expression.If -> {
                val tyCond = infer(ctx, expr.condition)

                try {
                    unify(tyCond.type, Type.Bool)
                } catch (err: TypeError) {
                    throw TypeError.IfCondition(tyCond.type, expr.condition.span)
                }

                val tyThen = infer(ctx, expr.thenBranch)
                val tyElse = infer(ctx, expr.elseBranch)

                withSpannedError(span) { unify(tyThen.type, tyElse.type) }

                tyWrap(Expression.If(tyCond, tyThen, tyElse, span), tyThen.type)
            }

            is Expression.Construction -> {
                val span = Span((expr.namespace.span ?: expr.dtor.span).start, expr.dtor.span.end)
                val (tyArgs, fields) = withSpannedError(span) {
                    lookupDtor(expr.namespace, expr.dtor)
                }

                val typedFields = mutableListOf<Expression.Typed>()
                val freshTyArgs = tyArgs.map { it to freshUnknown() }

                expr.exprs.zip(fields).forEach { (e, f) ->
                    val t = withSpannedError(e.span) { infer(ctx, e) }
                    withSpannedError(e.span) { unify(t.type, f.substMany(freshTyArgs)) }
                    typedFields += t
                }

                val (typeNs, type) = expr.namespace.splitType()

                tyWrap(
                    expr.copy(exprs = typedFields),
                    Type.Constructor(typeNs, type, freshTyArgs.map { it.second })
                )
            }
            is Expression.Match -> {
                val tyRes = freshUnknown()
                val tyExpr = infer(ctx, expr.expr)
                val tyCases = mutableListOf<Expression.Case>()

                expr.cases.forEach { case ->

                    val bodyCtx = inferPattern(case.namespace, case.dtor, case.binders, tyExpr.type)
                        .fold(ctx) { acc, (name, type) -> acc.put(Namespace.local to name, Scheme.fromType(type)) }

                    val tyBody = infer(bodyCtx, case.body)
                    unify(tyBody.type, tyRes)

                    tyCases += case.copy(body = tyBody)
                }

                tyWrap(
                    Expression.Match(tyExpr, tyCases, span),
                    tyRes
                )
            }
        }
    }

    private fun inferPattern(ns: Namespace, dtor: Name, binders: List<Name>, tyExpr: Type): List<Pair<Name, Type>> {
        val (tyArgs, fields) = lookupDtor(ns, dtor)
        val freshTyArgs = tyArgs.map { it to freshUnknown() }
        val (typeNs, type) = ns.splitType()
        unify(Type.Constructor(typeNs, type, freshTyArgs.map { it.second }), tyExpr)

        return binders.zip(fields.map { it.substMany(freshTyArgs) })
    }

    private fun lookupDtor(ns: Namespace, dtor: Name): Pair<List<TyVar>, List<Type>> {
        val nsType = ns.splitType()
        val typeInfo = typeCtx[nsType] ?: throw TypeError.UnknownType(nsType)
        val dc = typeInfo.dtors.find { it.name == dtor } ?: throw TypeError.UnknownDtor(ns to dtor)
        return typeInfo.typeArgs to dc.fields
    }

    fun inferSourceFile(interfaces: List<Pair<Namespace, Interface>>, file: SourceFile): Interface {
        fun ctxFromInterfaces(interfaces: List<Pair<Namespace, Interface>>): Pair<TCTypeContext, TCValueContext> {
            val typeCtx0: TCTypeContext = persistentHashMapOf()
            val valueCtx0: TCValueContext = persistentHashMapOf()

            val (typeCtx, valueCtx) = interfaces.fold(typeCtx0 to valueCtx0) { (typeCtx, valueCtx), (ns, i) ->
                val newTypeCtx = typeCtx.putAll(i.types.map { (name, typeInfo) -> ((ns to name) to typeInfo) }.toMap())
                val newValueCtx = valueCtx.putAll(i.values.map { (name, scheme) -> ((ns to name) to scheme) }.toMap())
                newTypeCtx to newValueCtx
            }

            return typeCtx to valueCtx
        }

        namespace = file.header.namespace
        var errored = false

        val (typeCtx0, valueCtx0) = ctxFromInterfaces(listOf(primInterface) + interfaces)
        val typeCtx = file.typeDeclarations().fold(typeCtx0) { acc, decl ->
            if (acc.containsKey(namespace to decl.name))
                acc
            else
                acc.put(
                    namespace to decl.name,
                    TypeInfo(decl.tyArgs, decl.dataConstructors)
                )
        }

        this.typeCtx = typeCtx

        val valueCtx = file.valueDeclarations().fold(valueCtx0) { ctx, it ->
            try {
                reset()
                val t = zonk(infer(ctx, it.expr))
                // println(this.substitution.toString())
                val scheme = Scheme.fromType(t.type) //generalize(t.type, ctx)

                withSpannedError(it.span) {
                    subsumes(scheme, it.scheme)
                }

                ctx.put(namespace to it.name, it.scheme)
            } catch (err: TypeError) {
                errored = true
                if (err !is TypeError.Followup) {
                    println("error ${if (err.span == Span.DUMMY) "" else err.span.toString()}: ${err.pretty()}")
                }
                ctx.put(namespace to it.name, Scheme.fromType(Type.ErrorSentinel))
            }
        }

        if (errored) throw Exception("Type errors occurred")

        return Interface(
            HashMap(typeCtx.mapNotNull { if (it.key.first == namespace) it.key.second to it.value else null }.toMap()),
            HashMap(valueCtx.mapNotNull { if (it.key.first == namespace) it.key.second to it.value else null }.toMap())
        )
    }
}

