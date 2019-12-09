package lambda.backend

import asmble.ast.Node.*
import asmble.ast.Node.Func
import asmble.ast.Node.Global
import asmble.ast.Node.Type.*
import asmble.io.AstToBinary
import asmble.io.AstToSExpr
import asmble.io.ByteWriter
import java.io.ByteArrayOutputStream
import java.io.File
import kotlin.Exception

fun main() {
    val module = Codegen().compileModule()
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

    private fun registerGlobalFunc(name: String, func: Func, isTable: Boolean): Int =
        globalFuncs.size.also {
            globalFuncs.add(name to func)
            if (isTable) tableFuncs.add(name)
        }

    private fun registerGlobals(name: String, global: Global): Int =
        globals.size.also { globals.add(name to global) }

    private fun registerType(name: String, type: Type.Func): Int =
        types.size.also { types.add(name to type) }


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

    fun compileModule(): Module {
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

        val add = makeFunc2(
            "add",
            Value.I32, Value.I32, Value.I32
        ) { _, x, y ->
            listOf(
                Instr.GetLocal(x),
                Instr.GetLocal(y),
                Instr.I32Add
            )
        }

        val add3 = makeFunc1("add3", Value.I32, Value.I32) { locals, x ->
            val y = locals.register(Value.I32)
            listOf(
                Instr.I32Const(3),
                Instr.SetLocal(y),
                Instr.GetLocal(x),
                Instr.GetLocal(y),
                Instr.Call(add)
            )
        }

        val duble = makeFunc2("duble\$inner", Value.I32, Value.I32, Value.I32, false) { _, x, y ->
            listOf(
                Instr.GetLocal(x),
                Instr.GetLocal(y),
                Instr.I32Add,
                Instr.I32Const(2),
                Instr.I32Mul
            )
        }

        val dubleWrapper = makeFunc1("duble", Value.I32, Value.I32, true) { _, arg_location ->
            listOf(
                Instr.GetLocal(arg_location),
                Instr.I32Load(2, 0),

                Instr.GetLocal(arg_location),
                Instr.I32Const(4),
                Instr.I32Add,
                Instr.I32Load(2, 0),

                Instr.Call(duble)
            )
        }

        val fn0 = Func(
            type = Type.Func(params = emptyList(), ret = Value.I32),
            locals = emptyList(),
            instructions = listOf(

                Instr.I32Const(globalFuncs.find { it.first == "duble\$inner" }!!.second.type.params.size), //arity
                Instr.I32Const(tableFuncs.indexOf("duble")), //code pointer
                Instr.Call(makeClosure),

                Instr.I32Const(21),
                Instr.Call(applyClosure),

                Instr.I32Const(22),
                Instr.Call(applyClosure)

            )
        )

        val main = registerGlobalFunc("main", fn0, false)

        return Module(
            funcs = globalFuncs.map { it.second },
            exports = globalFuncs.mapIndexed { i, (name, _) ->
                Export(name, ExternalKind.FUNCTION, i)
            },
            globals = globals.map { it.second },
            memories = listOf(Memory(ResizableLimits(1, null))),
            tables = listOf(Table(ElemType.ANYFUNC, ResizableLimits(globalFuncs.size, null))),
            elems = listOf(Elem(0, listOf(Instr.I32Const(0)), tableFuncs.map {
                globalFuncs.indexOfFirst { (name, _) -> name == it }.apply {
                    if (this == -1) throw Exception("unknown table fun $it")
                }
            }))
        )
    }
}