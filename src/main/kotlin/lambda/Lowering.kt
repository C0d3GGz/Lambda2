package lambda

import lambda.syntax.DataConstructor
import lambda.syntax.Expression
import lambda.syntax.Name
import lambda.syntax.SourceFile

class Lowering() {

    private var table: Map<Name, List<DataConstructor>> = emptyMap()

    private fun lowerExpr(expr: Expression): RTExpression {
        return when (expr) {
            is Expression.Literal -> RTExpression.Literal(expr.lit)
            is Expression.Var -> RTExpression.Var(expr.name)
            is Expression.Lambda -> RTExpression.Lambda(
                expr.binder.value,
                lowerExpr(expr.body.value)
            )
            is Expression.App -> RTExpression.App(
                lowerExpr(expr.func.value),
                lowerExpr(expr.arg.value)
            )
            is Expression.Typed -> lowerExpr(expr.expr.value)
            is Expression.Let -> {
                // let x = 4 in add x 5
                // (\x. add x 5) 4

                RTExpression.App(
                    RTExpression.Lambda(expr.binder.value, lowerExpr(expr.body.value)),
                    lowerExpr(expr.expr.value)
                )
            }
            is Expression.If -> RTExpression.If(
                lowerExpr(expr.condition.value),
                lowerExpr(expr.thenBranch.value),
                lowerExpr(expr.elseBranch.value)
            )
            is Expression.Construction -> RTExpression.Pack(
                tagForDtor(expr.type.value, expr.dtor.value),
                expr.exprs.map { lowerExpr(it.value) }
            )
            is Expression.Match ->
                RTExpression.Match(lowerExpr(expr.expr.value), expr.cases.map(::lowerCase))
        }
    }

    private fun lowerCase(case: Expression.Case): RTExpression.Case {
        val body = lowerExpr(case.body.value)
        val tag = tagForDtor(case.type.value, case.dtor.value)
        return RTExpression.Case(tag, case.binders.map { it.value }, body)
    }

    private fun tagForDtor(type: Name, dtor: Name): Int = table.getValue(type).indexOfFirst { it.name == dtor } + 1

    fun lowerSourceFile(sf: SourceFile): List<Pair<Name, RTExpression>> {
        table = sf.typeDeclarations()
            .map { type -> type.name.value to type.dataConstructors }
            .toMap()

        return sf.valueDeclarations().map { it.name.value to lowerExpr(it.expr.value) }
    }
}

