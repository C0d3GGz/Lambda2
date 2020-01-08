package lambda.backend

import lambda.Lexer
import lambda.Parser
import lambda.Span
import lambda.Type
import lambda.syntax.DataConstructor
import lambda.syntax.Expression
import lambda.syntax.Name
import java.util.*
import lambda.backend.Expression as IRExpression


fun main() {
    val input = """
        let y = 1 in
        let z = 2 in 
        (\x. add z y) 4
    """.trimIndent()

    val expr = Parser(Lexer(input)).parseExpression()
    val lowering =
        Lowering(
            mapOf(
                Name("Tuple") to listOf(
                    DataConstructor(
                        Name("T"),
                        listOf(Type.Int, Type.Int),
                        Span.DUMMY
                    )
                )
            )
        )
    println(lowering.lowerExpr(expr, emptyMap()))
    println("---")
    println(lowering.liftedDeclarations)
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
                var (args, body) = expr.foldArguments()
                val topName = freshName(Name("lifted"))
                val capturedVars = expr.freeVars().mapNotNull { name ->
                    env[name]?.let { name to it }
                }
                val newEnv = HashMap<Name, LnName.Index>()
                args = capturedVars.map { it.first } + args
                args.forEachIndexed { index, name ->
                    newEnv[name] = LnName.Index(0, index)
                }
                val loweredBody = lowerExpr(body, newEnv)
                liftedDeclarations += Declaration(topName, args, loweredBody)

                capturedVars.map { it.second }
                    .fold<LnName.Index, IRExpression>(IRExpression.Var(LnName.Free(topName))) { acc, ix ->
                        IRExpression.App(
                            acc,
                            IRExpression.Var(LnName.Bound(ix))
                        )
                    }
            }
            is Expression.App -> IRExpression.App(lowerExpr(expr.func, env), lowerExpr(expr.arg, env))
            is Expression.Typed -> lowerExpr(expr.expr, env)
            is Expression.Let -> if (expr.recursive) {
                if (expr.expr !is Expression.Lambda)
                    throw Exception("only functions may be defined recursively")


                TODO()
            } else {
                val tempEnv = HashMap<Name, LnName.Index>()
                env.mapValuesTo(tempEnv) { it.value.shift() }
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
        val tempEnv = HashMap<Name, LnName.Index>()
        env.mapValuesTo(tempEnv) { it.value.shift() }
        case.binders.forEachIndexed { index, name -> tempEnv[name] = LnName.Index(0, index) }
        return IRExpression.Case(tagForDtor(case.type, case.dtor), case.binders, lowerExpr(case.body, tempEnv))
    }

    private fun tagForDtor(type: Name, dtor: Name): Int = typeTable.getValue(type).indexOfFirst { it.name == dtor } + 1

    fun freshName(name: Name) = Name("\$$name${freshSupply++}")
}

