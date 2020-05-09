package lambda

import lambda.syntax.*

class Lowering() {

    private var interfaces: MutableList<Pair<Namespace, Interface>> = mutableListOf()

    private fun lowerExpr(expr: Expression): RTExpression {
        return when (expr) {
            is Expression.Literal -> RTExpression.Literal(expr.lit)
            is Expression.Hole -> throw Exception("Holes shouldn't reach lowerExpr")
            is Expression.Var -> RTExpression.Var(expr.namespace, expr.name)
            is Expression.Lambda -> expr.binders.foldRight(lowerExpr(expr.body)) { binder, acc ->
                RTExpression.Lambda(binder, acc)
            }
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
                tagForDtor(expr.namespace, expr.dtor),
                expr.exprs.map(::lowerExpr)
            )
            is Expression.Match ->
                RTExpression.Match(lowerExpr(expr.expr), expr.cases.map(::lowerCase))
        }
    }

    private fun lowerCase(case: Expression.Case): RTExpression.Case {
        val body = lowerExpr(case.body)
        val tag = tagForDtor(case.namespace, case.dtor)
        return RTExpression.Case(tag, case.binders, body)
    }

    private fun tagForDtor(namespace: Namespace, dtor: Name): Int {
        val (ns, type) = namespace.splitType()
        return interfaces.first { it.first == ns }.second.types[type]!!.dtors.indexOfFirst { it.name == dtor } + 1
    }

    fun lowerSourceFile(
        sf: SourceFile,
        interfaces: MutableList<Pair<Namespace, Interface>>
    ): List<Pair<Name, RTExpression>> {
        this.interfaces = interfaces

        return sf.valueDeclarations().map { it.name to lowerExpr(it.expr) }
    }
}

