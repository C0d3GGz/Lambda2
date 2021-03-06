package lambda.backend

/*
import asmble.ast.Node.*
import asmble.ast.Node.Func
import asmble.ast.Node.Global
import asmble.ast.Node.Type.*
import asmble.io.AstToBinary
import asmble.io.AstToSExpr
import asmble.io.ByteWriter
import lambda.Lexer
import lambda.Parser
import lambda.Typechecker
import lambda.syntax.Name
import java.io.ByteArrayOutputStream
import java.io.File

fun main() {

    val source = """
        type Option<a> {
            None(),
            Some(a)
        }
        
        type List<a> {
            Nil(),
            Cons(a, List<a>)
        }
       
        let main : Int =
        
            letrec sum = \x. match x {
                List::Nil() => 0,
                List::Cons(h,t) => add h (sum t) 
            } in 
        
            let list = List::Cons(12, List::Cons(10, List::Cons(20, List::Nil() ))) in  
            sum list;
        
    """

    val sf = Parser(Lexer(source)).parseSourceFile()
    Typechecker().inferSourceFile(sf)
    val ir = Lowering.lowerProg(sf)

    ir.forEach { println(it) }

    val module = Codegen().compileProgram(ir)
    val expr = AstToSExpr().fromModule(module)
    println(expr)

    val buffer = ByteArrayOutputStream()
    AstToBinary().fromModule(ByteWriter.OutputStream(buffer), module)

    File("out.wasm").writeBytes(buffer.toByteArray())
}

class Locals(vararg params: Value) {
    private var list: MutableList<Value> = params.toMutableList()
    private var nParams: Int = params.size

    fun register(type: Value): Int = list.size.also { list.add(type) }

    // All the locals the user registered
    fun getLocals(): List<Value> = list.drop(nParams)
}

class Codegen() {

    private val globalFuncs = mutableListOf<Pair<String, Func>>()

    private val tableFuncs = mutableListOf<String>()

    private val globals = mutableListOf<Pair<String, Global>>()

    private val types = mutableListOf<Pair<String, Type.Func>>()

    private fun registerGlobalFunc(name: String, func: Func, isTable: Boolean): Int {
        val index = globalFuncs.indexOfFirst { it.first == name }

        return if (index < 0)
            globalFuncs.size.also {
                globalFuncs.add(name to func)
                if (isTable) tableFuncs.add(name)
            }
        else
            index.also {
                globalFuncs[it] = name to func
                if (isTable && tableFuncs.indexOf(name) < 0) tableFuncs.add(name)
            }
    }

    private fun registerGlobals(name: String, global: Global): Int =
        globals.size.also { globals.add(name to global) }

    private fun registerType(name: String, type: Type.Func): Int =
        types.size.also { types.add(name to type) }


    fun arityFor(name: Name): Int {
        val (_, func) = globalFuncs.first { it.first == name.value }
        return func.type.params.size
    }

    fun funcIndexFor(name: String): Int = globalFuncs.indexOfFirst { it.first == name }.also {
        if (it < 0) throw Exception("unknown global function $name")
    }

    fun tableNameFor(name: Name): String = "${name.value}\$table"

    fun tableIndexFor(name: Name): Int = tableFuncs.indexOfFirst { it == tableNameFor(name) }.also {
        if (it < 0) throw Exception("unknown table function ${tableNameFor(name)}")
    }

    fun makePrimitiveBinary(name: String, instr: Instr): Int {
        val binaryF = makeFunc2(
            name,
            Value.I32, Value.I32, Value.I32
        ) { _, x, y ->
            listOf(
                Instr.GetLocal(x),
                Instr.GetLocal(y),
                instr
            )
        }

        makeFunc1(tableNameFor(Name(name)), Value.I32, Value.I32, true) { _, argPointer ->
            (0 until 2).flatMap { i ->
                listOf<Instr>(
                    Instr.GetLocal(argPointer),
                    Instr.I32Load(2, i * 4L)
                )
            } + Instr.Call(binaryF)
        }

        return binaryF
    }

    fun makeFunc1(
        name: String,
        param: Value,
        res: Value?,
        isTable: Boolean = false,
        body: (locals: Locals, paramIndex: Int) -> List<Instr>
    ): Int {
        val locals = Locals(param)
        val instrs = body(locals, 0)
        val func = Func(
            type = Type.Func(listOf(param), ret = res),
            locals = locals.getLocals(),
            instructions = instrs
        )
        return registerGlobalFunc(name, func, isTable)
    }

    fun makeFunc2(
        name: String,
        param1: Value,
        param2: Value,
        res: Value?,
        isTable: Boolean = false,
        body: (Locals, Int, Int) -> List<Instr>
    ): Int {
        val locals = Locals(param1, param2)
        val instrs = body(locals, 0, 1)
        val func = Func(
            type = Type.Func(listOf(param1, param2), ret = res),
            locals = locals.getLocals(),
            instructions = instrs
        )
        return registerGlobalFunc(name, func, isTable)
    }

    fun makeFuncN(
        name: String,
        args: List<Value>,
        res: Value?,
        isTable: Boolean = false,
        body: (Locals, List<Int>) -> List<Instr>
    ): Int {
        val locals = Locals(*args.toTypedArray())
        val instrs = body(locals, args.indices.toList())
        val func = Func(
            type = Type.Func(args, ret = res),
            locals = locals.getLocals(),
            instructions = instrs
        )
        return registerGlobalFunc(name, func, isTable)
    }

    fun compileCase(
        locals: Locals,
        case: Expression.Case,
        tag: Int,
        scrutinee: Int,
        continuation: List<Instr>
    ): List<Instr> {
        val binders = case.binders.map {
            locals.register(Value.I32)
        }


        return listOf<Instr>(
            Instr.I32Const(case.tag),
            Instr.GetLocal(tag),
            Instr.I32Eq,

            Instr.If(Value.I32)
        ) +
                binders.mapIndexed { index, binder ->
                    listOf<Instr>(
                        Instr.GetLocal(scrutinee),
                        Instr.I32Load(2, 4 + index * 4L),
                        Instr.SetLocal(binder)
                    )
                }.flatten() +
                compileExpr(locals, case.body.instantiate(binders.map {
                    Expression.GetLocal(it)
                })) +

                Instr.Else +
                continuation +

                Instr.End
    }

    fun compileLiteral(lit: Lit): List<Instr> {
        return when (lit) {
            is Lit.Int -> listOf(Instr.I32Const(lit.int))
            is Lit.Bool -> listOf(Instr.I32Const(if (lit.bool) 1 else 0))
            is Lit.String -> TODO("string literals unsupported")
        }
    }


    fun compileExpr(locals: Locals, expr: Expression): List<Instr> {
        return when (expr) {
            is Expression.Literal -> compileLiteral(expr.lit)
            is Expression.Var -> {
                if (expr.name !is LnName.Free) throw Exception("can't deal with bound names in compileExpr")

                listOf(
                    Instr.I32Const(arityFor(expr.name.name)),
                    Instr.I32Const(tableIndexFor(expr.name.name)),
                    Instr.Call(funcIndexFor("make_closure"))
                )
            }
            is Expression.App -> {
                compileExpr(locals, expr.func) +
                        compileExpr(locals, expr.arg) +
                        Instr.Call(funcIndexFor("apply_closure"))
            }
            is Expression.Let -> {
                val binder = locals.register(Value.I32)
                compileExpr(locals, expr.expr) +
                        Instr.SetLocal(binder) +
                        compileExpr(locals, expr.body.instantiate(listOf(Expression.GetLocal(binder))))
            }
            is Expression.If -> {
                compileExpr(locals, expr.condition) +
                        Instr.If(Value.I32) +
                        compileExpr(locals, expr.thenBranch) +
                        Instr.Else +
                        compileExpr(locals, expr.elseBranch) +
                        Instr.End
            }
            is Expression.Pack -> {
                // [2byte tag | 2byte arity | (4byte x arity) fields]
                val arity = expr.exprs.size
                val packPointer = locals.register(Value.I32)

                listOf<Instr>(
                    Instr.I32Const(4 + 4 * arity),
                    Instr.Call(funcIndexFor("allocate")),
                    Instr.TeeLocal(packPointer),
                    Instr.I32Const(expr.tag),
                    Instr.I32Store16(1, 0),
                    Instr.GetLocal(packPointer),
                    Instr.I32Const(arity),
                    Instr.I32Store16(1, 2)
                ) + expr.exprs.withIndex().flatMap { (i, e) ->
                    listOf(Instr.GetLocal(packPointer)) +
                            compileExpr(locals, e) +
                            Instr.I32Store(2, 4L + 4 * i)
                } + Instr.GetLocal(packPointer)
            }
            is Expression.Match -> {
                val scrutinee = locals.register(Value.I32)
                val tag = locals.register(Value.I32)
                compileExpr(locals, expr.expr) +
                        listOf<Instr>(
                            Instr.TeeLocal(scrutinee),
                            Instr.I32Load16S(1, 0),
                            Instr.SetLocal(tag)
                        ) +
                        expr.cases.foldRight(listOf<Instr>(Instr.Unreachable)) { case, acc ->
                            compileCase(locals, case, tag, scrutinee, acc)
                        }
            }
            is Expression.GetLocal -> listOf(Instr.GetLocal(expr.index))
        }
    }

    fun compileProgram(decls: List<Declaration>): Module {
        val watermark = registerGlobals(
            "watermark",
            Global(
                Type.Global(Value.I32, true),
                listOf(Instr.I32Const(0))
            )
        )

        val allocate = makeFunc1(
            "allocate",
            Value.I32, Value.I32
        ) { locals, bytes ->
            val res = locals.register(Value.I32)

            listOf(
                Instr.GetGlobal(watermark),
                Instr.TeeLocal(res), // Set- and GetLocal
                Instr.GetLocal(bytes),
                Instr.I32Add,
                Instr.SetGlobal(watermark),
                Instr.GetLocal(res)
            )
        }

        val makeClosure = makeFunc2(
            "make_closure",
            Value.I32, Value.I32,
            Value.I32
        ) { locals, arity, code_pointer ->
            val closure = locals.register(Value.I32)

            listOf(

                // size of argument memory
                Instr.GetLocal(arity),
                Instr.I32Const(4), // 4 byte per argument (32 bit)
                Instr.I32Mul,

                // size for static components of closure
                Instr.I32Const(8), // arity 2, applied 2, code_pointer 4
                Instr.I32Add,

                Instr.Call(allocate),
                Instr.SetLocal(closure),

                // initialize arity
                Instr.GetLocal(closure),
                Instr.GetLocal(arity),
                Instr.I32Store16(1, 0),

                // initialize applied arguments (to zero is redundant)

                Instr.GetLocal(closure),
                Instr.I32Const(4), // jump over arity and applied
                Instr.I32Add,

                Instr.GetLocal(code_pointer),
                Instr.I32Store(2, 0),

                Instr.GetLocal(closure)

            )

        }

        val applyClosure = makeFunc2(
            "apply_closure",
            Value.I32,
            Value.I32,
            Value.I32
        ) { locals, closure, arg ->
            val applied_value = locals.register(Value.I32)
            val applied_location = locals.register(Value.I32)


            listOf(

//          apply_closure(closure, arg){
//              closure.args[applied] = arg
//              if closure.applied+1 < closure.arity then
//                  closure.applied++
//                  closure
//              else
//                  closure.code_pointer(&closure.arguments)
//          }

                //jump to applied location
                Instr.GetLocal(closure),
                Instr.I32Const(2),
                Instr.I32Add,

                // read applied value
                Instr.TeeLocal(applied_location),
                Instr.I32Load16S(1, 0), //loads applied value onto stack
                Instr.SetLocal(applied_value),

                // jumping to start of argument vector
                Instr.GetLocal(closure),
                Instr.I32Const(8),
                Instr.I32Add,

                // jumping over applied arguments
                Instr.GetLocal(applied_value),
                Instr.I32Const(4),
                Instr.I32Mul,
                Instr.I32Add,

                // write argument into argument vector
                Instr.GetLocal(arg),
                Instr.I32Store(2, 0),

                //applied+1
                Instr.GetLocal(applied_value),
                Instr.I32Const(1),
                Instr.I32Add,

                //closure.arity
                Instr.GetLocal(closure),
                Instr.I32Load16S(1, 0),

                //if argument list isn't complete yet
                Instr.I32LtS,

                Instr.If(Value.I32),

                //store applied++ to closure.applied, return closure
                Instr.GetLocal(applied_location),
                Instr.GetLocal(applied_value),
                Instr.I32Const(1),
                Instr.I32Add,
                Instr.I32Store16(1, 0),
                Instr.GetLocal(closure),

                Instr.Else,
                // jumping to start of argument vector
                Instr.GetLocal(closure),
                Instr.I32Const(8),
                Instr.I32Add,

                // jumping to code_pointer
                Instr.GetLocal(closure),
                Instr.I32Const(4),
                Instr.I32Add,
                Instr.I32Load(2, 0),

                Instr.CallIndirect(
                    registerType(
                        "int_to_int",
                        Type.Func(listOf(Value.I32), Value.I32)
                    ), false
                ),

                //end if
                Instr.End

            )
        }

        makePrimitiveBinary("add", Instr.I32Add)
        makePrimitiveBinary("sub", Instr.I32Sub)
        makePrimitiveBinary("mul", Instr.I32Mul)
        makePrimitiveBinary("int_eq", Instr.I32Eq)
        makePrimitiveBinary("int_gt", Instr.I32GtS)
        makePrimitiveBinary("int_gte", Instr.I32GeS)




        fun compileDeclaration(decl: Declaration) {
            val arity = decl.args.size
            println("compiling decl: ${decl.name.value} @ $arity")

            val uncurried =
                makeFuncN(decl.name.value, decl.args.map { Value.I32 }, Value.I32, false) { locals, params ->
                    compileExpr(locals, decl.body.instantiate(params.map { Expression.GetLocal(it) }))
                }

            makeFunc1(tableNameFor(decl.name), Value.I32, Value.I32, true) { _, argPointer ->
                (0 until arity).flatMap { i ->
                    listOf<Instr>(
                        Instr.GetLocal(argPointer),
                        Instr.I32Load(2, i * 4L)
                    )
                } + Instr.Call(uncurried)
            }
        }

        fun registerDeclaration(decl: Declaration) {
            val dummyFunc = Func(
                type = Type.Func(decl.args.map { Value.I32 }, ret = Value.I32),
                locals = emptyList(),
                instructions = listOf(Instr.Unreachable)
            )

            registerGlobalFunc(decl.name.value, dummyFunc, false)
            registerGlobalFunc(tableNameFor(decl.name), dummyFunc, true)
        }

        decls.forEach { registerDeclaration(it) }
        decls.forEach { compileDeclaration(it) }

        return Module(
            funcs = globalFuncs.map { it.second },
            exports = globalFuncs.mapIndexed { i, (name, _) ->
                Export(name, ExternalKind.FUNCTION, i)
            },
            globals = globals.map { it.second },
            memories = listOf(Memory(ResizableLimits(1, null))),
            tables = listOf(Table(ElemType.ANYFUNC, ResizableLimits(tableFuncs.size, null))),
            elems = listOf(Elem(0, listOf(Instr.I32Const(0)), tableFuncs.map {
                globalFuncs.indexOfFirst { (name, _) -> name == it }.apply {
                    if (this == -1) throw Exception("unknown table fun $it")
                }
            }))
        )
    }
}*/
