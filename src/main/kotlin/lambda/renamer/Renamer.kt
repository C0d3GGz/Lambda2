package lambda.renamer

import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf
import lambda.*
import lambda.syntax.*

typealias Aliases = PersistentMap<Namespace, Namespace>
typealias Types = PersistentMap<Name, Namespace>
typealias Values = PersistentMap<Name, Namespace>

data class Context(val currentNamespace: Namespace, val aliases: Aliases, val types: Types, val values: Values)

fun renameModule(module: SourceFile): SourceFile {
    val initialTypes = persistentHashMapOf(
        Name("String") to Namespace.prim,
        Name("Int") to Namespace.prim,
        Name("Bool") to Namespace.prim,
        Name("Unit") to Namespace.prim
    )

    val initialValues = persistentHashMapOf(
        Name("add") to Namespace.prim
    )

    val initialCtx = Context(module.header.namespace, persistentHashMapOf(), initialTypes, initialValues)

    val context0 = module.header.imports.fold(initialCtx) { acc, import ->
        when (import) {
            is Import.Qualified -> acc.copy(aliases = acc.aliases.put(import.alias, import.namespace))
        }
    }

    val context1 = module.typeDeclarations().fold(context0) { acc, type ->
        if (acc.types.contains(type.name))
            throw Exception("can't define the same type twice at ${type.name.span}")
        if (acc.aliases.contains(Namespace(type.name)))
            throw Exception("can't define the type ${type.name} because it shadows an import")

        acc.copy(
            aliases = acc.aliases.put(Namespace(type.name), acc.currentNamespace.nest(type.name)),
            types = acc.types.put(type.name, acc.currentNamespace)
        )
    }

    return SourceFile(
        module.header.copy(imports = module.header.imports.map(::removeAlias)),
        module.declarations.fold(context1 to listOf<Declaration>()) { (ctx, decls), decl ->
            val (newCtx, newDecl) = renameDeclaration(ctx, decl)
            newCtx to decls + newDecl
        }.second,
        module.span
    )
}

private fun renameDeclaration(ctx: Context, declaration: Declaration): Pair<Context, Declaration> {
    return when (declaration) {
        is Declaration.Value -> {
            ctx.copy(values = ctx.values.put(declaration.name, ctx.currentNamespace)) to
                    declaration.copy(
                        scheme = renameScheme(ctx, declaration.scheme),
                        expr = renameExpr(ctx, declaration.expr)
                    )
        }
        is Declaration.Type -> {
            ctx to declaration
                .copy(dataConstructors = declaration.dataConstructors.map { renameDataConstructor(ctx, it) })
        }
    }
}

private fun renameScheme(ctx: Context, scheme: Scheme): Scheme {
    return scheme.copy(ty = renameType(ctx, scheme.ty))
}

private fun renameExpr(ctx: Context, expr: Expression): Expression {
    return when (expr) {
        is Expression.Literal -> expr
        is Expression.App -> expr.copy(func = renameExpr(ctx, expr.func), arg = renameExpr(ctx, expr.arg))
        is Expression.Typed -> expr.copy(expr = renameExpr(ctx, expr.expr), type = renameType(ctx, expr.type))
        is Expression.Construction -> expr.copy(
            namespace = renameDtor(ctx, expr.namespace),
            exprs = expr.exprs.map { renameExpr(ctx, it) }
        )
        is Expression.If -> expr.copy(
            condition = renameExpr(ctx, expr.condition),
            thenBranch = renameExpr(ctx, expr.thenBranch),
            elseBranch = renameExpr(ctx, expr.elseBranch)
        )
        is Expression.Let -> {
            val extendedCtx = ctx.copy(values = ctx.values.put(expr.binder, Namespace.local))

            expr.copy(
                scheme = expr.scheme?.let { renameScheme(ctx, it) },
                expr = renameExpr(if (expr.recursive) extendedCtx else ctx, expr.expr),
                body = renameExpr(extendedCtx, expr.body)
            )
        }
        is Expression.Lambda -> expr.copy(
            body = renameExpr(ctx.copy(values = ctx.values.put(expr.binder, Namespace.local)), expr.body)
        )
        is Expression.Var -> expr.copy(
            namespace = if (expr.namespace.isLocal())
                ctx.values[expr.name] ?: throw Exception("unknown variable ${expr.name} at ${expr.span}")
            else
                ctx.aliases[expr.namespace] ?: throw Exception("unknown namespace ${expr.namespace} at ${expr.span}")
        )
        is Expression.Match -> expr.copy(
            expr = renameExpr(ctx, expr.expr),
            cases = expr.cases.map { renameCase(ctx, it) }
        )
    }
}

fun renameDtor(ctx: Context, namespace: Namespace): Namespace {
    val (ns, type) = namespace.splitType()

    val newNamespace = if (ns.isLocal())
        ctx.types[type] ?: throw Exception("unknown type $type at ${type.span}")
    else
        ctx.aliases[ns] ?: throw Exception("unknown namespace $ns at ${ns.span}")

    return newNamespace.nest(type)
}

fun renameCase(ctx: Context, case: Expression.Case): Expression.Case = case.copy(
    namespace = renameDtor(ctx, case.namespace),
    body = renameExpr(
        ctx.copy(values = ctx.values.putAll(case.binders.map { it to Namespace.local }.toMap())),
        case.body
    )
)

private fun renameDataConstructor(ctx: Context, dtor: DataConstructor): DataConstructor {
    return dtor.copy(fields = dtor.fields.map { renameType(ctx, it) })
}

private fun renameType(ctx: Context, ty: Type): Type {
    return when (ty) {
        is Type.ErrorSentinel, is Type.Unknown, is Type.Var -> ty
        is Type.Fun -> ty.copy(arg = renameType(ctx, ty.arg), result = renameType(ctx, ty.result))
        is Type.Constructor -> {
            val newNamespace = if (ty.namespace.isLocal()) {
                ctx.types[ty.name] ?: throw Exception("unknown type at ${ty.span}")
            } else {
                ctx.aliases[ty.namespace] ?: throw Exception("unknown namespace at ${ty.namespace.span}")
            }

            ty.copy(namespace = newNamespace, tyArgs = ty.tyArgs.map { renameType(ctx, it) })
        }
    }
}

private fun removeAlias(import: Import): Import {
    return when (import) {
        is Import.Qualified -> import.copy(alias = import.namespace)
    }
}

fun main() {
    val sf = Parser(
        Lexer(
            """
module Main

import List as L

type List<a> {
  Cons(a, List<a>),
  Nil()
}

let xs: Int = L::List::Cons(1, List::Nil()) ;

let main: Int =  
 let x = match xs {
  L::List::Cons(x, xs) => xs,
  L::List::Nil() => 10
 } in
 
  let y = match xs {
   List::Cons(x, xs) => xs,
   List::Nil() => 10
  } in
 
 add x 4;
    """.trimIndent()
        )
    ).parseSourceFile()

    print(renameModule(sf))
}