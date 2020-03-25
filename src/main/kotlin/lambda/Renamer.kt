package lambda

import lambda.syntax.*
import java.lang.Exception

class Renamer {

    private val ctx: HashMap<Namespace, Namespace> = hashMapOf()
    private var currentNamespace = Namespace.local
    private val currentTypes = hashMapOf(
        Name("String") to Namespace.prim,
        Name("Int") to Namespace.prim,
        Name("Bool") to Namespace.prim,
        Name("Unit") to Namespace.prim
    )
    private val currentValues = hashMapOf(Name("add") to Namespace.prim)

    fun renameModule(module: SourceFile): SourceFile {
        currentNamespace = module.header.namespace
        module.header.imports.forEach {
            when (it) {
                is Import.Qualified -> ctx[it.alias] = it.namespace
            }
        }

        module.typeDeclarations().forEach {
            if (currentTypes.contains(it.name))
                throw Exception("can't define the same type twice at ${it.name.span}")
            currentTypes[it.name] = currentNamespace

            if (ctx.contains(Namespace(it.name)))
                throw Exception("can't define the type ${it.name} because it shadows an import")
            ctx[Namespace(it.name)] = currentNamespace.nest(it.name)
        }

        return SourceFile(
            module.header.copy(imports = module.header.imports.map(::removeAlias)),
            module.declarations.map {
                renameDeclaration(it)
            },
            module.span
        )
    }

    private fun renameDeclaration(declaration: Declaration): Declaration {
        return when (declaration) {
            is Declaration.Value -> {
                declaration.copy(scheme = renameScheme(declaration.scheme), expr = renameExpr(declaration.expr))
            }
            is Declaration.Type -> {
                declaration.copy(dataConstructors = declaration.dataConstructors.map {
                    renameDataConstructor(it)
                })
            }
        }
    }

    private fun renameScheme(scheme: Scheme): Scheme {
        return scheme.copy(ty = renameType(scheme.ty))
    }

    private fun renameExpr(expr: Expression): Expression {
        return when (expr) {
            is Expression.Literal -> expr
            is Expression.App -> expr.copy(func = renameExpr(expr.func), arg = renameExpr(expr.arg))
            is Expression.Typed -> expr.copy(expr = renameExpr(expr.expr), type = renameType(expr.type))
            is Expression.Construction -> expr.copy(
                namespace = ctx[expr.namespace]
                    ?: throw Exception("unknown type ${expr.namespace.asQualifier()} at ${expr.namespace.span}"),
                exprs = expr.exprs.map(::renameExpr))
            is Expression.If -> expr.copy(
                condition = renameExpr(expr.condition),
                thenBranch = renameExpr(expr.thenBranch),
                elseBranch = renameExpr(expr.elseBranch)
            )
            is Expression.Let -> expr.copy(
                scheme = expr.scheme?.let(::renameScheme),
                expr = renameExpr(expr.expr),
                body = renameExpr(expr.body)
            )
            is Expression.Lambda -> TODO()
            is Expression.Var -> TODO()
            is Expression.Match -> TODO()
        }
    }

    private fun renameDataConstructor(dtor: DataConstructor): DataConstructor {
        return dtor.copy(fields = dtor.fields.map { renameType(it) })
    }

    private fun renameType(ty: Type): Type {
        return when (ty) {
            is Type.ErrorSentinel, is Type.Unknown, is Type.Var -> ty
            is Type.Fun -> ty.copy(arg = renameType(ty.arg), result = renameType(ty.result))
            is Type.Constructor -> {
                val newNamespace = if (ty.namespace.isLocal()) {
                    currentTypes[ty.name] ?: throw Exception("unknown type at ${ty.span}")
                } else {
                    ctx[ty.namespace] ?: throw Exception("unknown namespace at ${ty.namespace.span}")
                }

                ty.copy(namespace = newNamespace, tyArgs = ty.tyArgs.map(::renameType))
            }
        }
    }

    private fun removeAlias(import: Import): Import {
        return when (import) {
            is Import.Qualified -> import.copy(alias = import.namespace)
        }
    }

}

fun main() {
    val sf = Parser(
        Lexer(
            """
module Main


type List<a> {
  Cons(a, List<a>),
  Nil()
}

let x : List<Int> = List::Cons(1, List::Nil());



    """.trimIndent()
        )
    ).parseSourceFile()

    print(Renamer().renameModule(sf))
}