package lambda.backend

import lambda.Lexer
import lambda.Parser
import lambda.syntax.DataConstructor
import lambda.syntax.Expression
import lambda.syntax.Name
import java.util.*
import lambda.backend.Expression as IRExpression


fun main() {
    val input = """
        let y = 1 in 
        (\x. add x y) 4
    """.trimIndent()

    val expr = Parser(Lexer(input)).parseExpression()
    val lowering = Lowering(emptyMap())
    println(lowering.lowerExpr(expr, emptyMap()))
}

class Lowering(private val typeTable: Map<Name, List<DataConstructor>>) {
    var freshSupply: Int = 0
    val liftedDeclarations: MutableList<Declaration> = mutableListOf()

    fun lowerExpr(expr: Expression, env: Map<Name, LnName.Index>): IRExpression {
        return when (expr) {
            is Expression.Literal -> IRExpression.Literal(Lit.fromSyntax(expr.lit))
            is Expression.Var -> IRExpression.Var(env[expr.name]?.let {
                LnName.Bound(it)
            } ?: LnName.Free(expr.name))

            is Expression.Lambda -> {
                val (args, body) = expr.foldArguments()
                val topName = freshName(Name("lifted"))
                val freeVars = expr.freeVars().mapNotNull {
                    env[it]?.let { index ->
                        it to LnName.Bound(index)
                    }
                }
                println("$topName: $freeVars")
                IRExpression.Var(LnName.Free(topName)) //TODO
            }
            is Expression.App -> IRExpression.App(lowerExpr(expr.func, env), lowerExpr(expr.arg, env))
            is Expression.Typed -> lowerExpr(expr.expr, env)
            is Expression.Let -> if (expr.recursive) TODO() else {
                val tempEnv = HashMap(env)
                tempEnv.mapValues { it.value.shift() }
                tempEnv[expr.binder] = LnName.Index(0, 0)
                IRExpression.Let(expr.binder, lowerExpr(expr.expr, env), lowerExpr(expr.body, tempEnv))
            }
            is Expression.If -> IRExpression.If(
                lowerExpr(expr.condition, env),
                lowerExpr(expr.elseBranch, env),
                lowerExpr(expr.thenBranch, env)
            )
            is Expression.Construction -> IRExpression.Pack(
                tagForDtor(expr.type, expr.dtor),
                expr.exprs.map { lowerExpr(it, env) })
            is Expression.Match -> IRExpression.Match(
                lowerExpr(expr.expr, env),
                expr.cases.map { lowerCase(it, env) })
        }
    }

    private fun lowerCase(case: Expression.Case, env: Map<Name, LnName.Index>): IRExpression.Case {
        val tempEnv = HashMap(env)
        tempEnv.mapValues { it.value.shift() }
        case.binders.forEachIndexed { index, name -> tempEnv[name] = LnName.Index(0, index) }
        return IRExpression.Case(tagForDtor(case.type, case.dtor), case.binders, lowerExpr(case.body, tempEnv))
    }

    private fun tagForDtor(type: Name, dtor: Name): Int = typeTable.getValue(type).indexOfFirst { it.name == dtor } + 1

    fun freshName(name: Name) = Name("$name${freshSupply++}")
}

