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
                expr.binder,
                lowerExpr(expr.body)
            )
            is Expression.App -> RTExpression.App(
                lowerExpr(expr.func),
                lowerExpr(expr.arg)
            )
            is Expression.Typed -> lowerExpr(expr.expr)
            is Expression.Let -> {
                // let x = 4 in add x 5
                // (\x. add x 5) 4

                if (expr.recursive)
                    RTExpression.LetRec(expr.binder, lowerExpr(expr.expr), lowerExpr(expr.body))
                else
                    RTExpression.App(
                        RTExpression.Lambda(expr.binder, lowerExpr(expr.body)),
                        lowerExpr(expr.expr)
                    )
            }
            is Expression.If -> RTExpression.If(
                lowerExpr(expr.condition),
                lowerExpr(expr.thenBranch),
                lowerExpr(expr.elseBranch)
            )
            is Expression.Construction -> RTExpression.Pack(
                tagForDtor(expr.type, expr.dtor),
                expr.exprs.map(::lowerExpr)
            )
            is Expression.Match ->
                RTExpression.Match(lowerExpr(expr.expr), expr.cases.map(::lowerCase))
        }
    }

    private fun lowerCase(case: Expression.Case): RTExpression.Case {
        val body = lowerExpr(case.body)
        val tag = tagForDtor(case.type, case.dtor)
        return RTExpression.Case(tag, case.binders, body)
    }

    private fun tagForDtor(type: Name, dtor: Name): Int = table.getValue(type).indexOfFirst { it.name == dtor } + 1

    fun lowerSourceFile(sf: SourceFile): List<Pair<Name, RTExpression>> {
        table = sf.typeDeclarations()
            .map { type -> type.name to type.dataConstructors }
            .toMap()

        return sf.valueDeclarations().map { it.name to lowerExpr(it.expr) }
    }
}

