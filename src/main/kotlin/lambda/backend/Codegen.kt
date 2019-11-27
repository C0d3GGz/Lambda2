package lambda.backend

import asmble.ast.Node
import asmble.io.AstToBinary
import asmble.io.AstToSExpr
import asmble.io.ByteWriter
import java.io.ByteArrayOutputStream
import java.io.File

fun main() {
    val module = Codegen().compileModule()
    val expr = AstToSExpr().fromModule(module)
    println(expr)

    val buffer = ByteArrayOutputStream()
    AstToBinary().fromModule(ByteWriter.OutputStream(buffer), module)

    File("out.wasm").writeBytes(buffer.toByteArray())
}

class Locals(vararg params: Node.Type.Value) {
    private var list: MutableList<Node.Type.Value> = params.toMutableList()
    private var nParams: Int = params.size

    fun register(type: Node.Type.Value): Int = list.size.also { list.add(type) }

    // All the locals the user registered
    fun getLocals(): List<Node.Type.Value> = list.drop(nParams)
}

class Codegen() {

    private val globalFuncs = mutableListOf<Pair<String, Node.Func>>()

    private val globals = mutableListOf<Pair<String, Node.Global>>()

    private fun registerGlobalFunc(name: String, func: Node.Func): Int =
        globalFuncs.size.also { globalFuncs.add(name to func) }

    private fun registerGlobals(name: String, global: Node.Global): Int =
        globals.size.also { globals.add(name to global) }

    fun makeFunc1(
        name: String,
        param: Node.Type.Value,
        res: Node.Type.Value?,
        body: (locals: Locals, paramIndex: Int) -> List<Node.Instr>
    ): Int {
        val locals = Locals(param)
        val instrs = body(locals, 0)
        val func = Node.Func(
            type = Node.Type.Func(listOf(param), ret = res),
            locals = locals.getLocals(),
            instructions = instrs
        )
        return registerGlobalFunc(name, func)
    }

    fun makeFunc2(
        name: String,
        param1: Node.Type.Value,
        param2: Node.Type.Value,
        res: Node.Type.Value?,
        body: (Locals, Int, Int) -> List<Node.Instr>
    ): Int {
        val locals = Locals(param1, param2)
        val instrs = body(locals, 0, 1)
        val func = Node.Func(
            type = Node.Type.Func(listOf(param1, param2), ret = res),
            locals = locals.getLocals(),
            instructions = instrs
        )
        return registerGlobalFunc(name, func)
    }

    fun compileModule(): Node.Module {
        val watermark = registerGlobals(
            "watermark",
            Node.Global(
                Node.Type.Global(Node.Type.Value.I32, true),
                listOf(Node.Instr.I32Const(0))
            )
        )

        val allocate = makeFunc1(
            "allocate",
            Node.Type.Value.I32, Node.Type.Value.I32
        ) { locals, bytes ->
            val res = locals.register(Node.Type.Value.I32)

            listOf(
                Node.Instr.GetGlobal(watermark),
                Node.Instr.TeeLocal(res), // Set- and GetLocal
                Node.Instr.GetLocal(bytes),
                Node.Instr.I32Add,
                Node.Instr.SetGlobal(watermark),
                Node.Instr.GetLocal(res)
            )
        }

        val makeClosure = makeFunc2(
            "make_closure",
            Node.Type.Value.I32, Node.Type.Value.I32,
            Node.Type.Value.I32
        ) { locals, arity, code_pointer ->
            val closure = locals.register(Node.Type.Value.I32)

            listOf(

                // size of argument memory
                Node.Instr.GetLocal(arity),
                Node.Instr.I32Const(4), // 4 byte per argument (32 bit)
                Node.Instr.I32Mul,

                // size for static components of closure
                Node.Instr.I32Const(8), // arity 2, applied 2, code_pointer 4
                Node.Instr.I32Add,

                Node.Instr.Call(allocate),
                Node.Instr.SetLocal(closure),

                // initialize arity
                Node.Instr.GetLocal(closure),
                Node.Instr.GetLocal(arity),
                Node.Instr.I32Store16(1, 0),

                // initialize applied arguments (to zero is redundant)

                Node.Instr.GetLocal(closure),
                Node.Instr.I32Const(4), // jump over arity and applied
                Node.Instr.I32Add,

                Node.Instr.GetLocal(code_pointer),
                Node.Instr.I32Store(2, 0),

                Node.Instr.GetLocal(closure)

            )

        }

        val add = makeFunc2(
            "add",
            Node.Type.Value.I32, Node.Type.Value.I32, Node.Type.Value.I32
        ) { _, x, y ->
            listOf(
                Node.Instr.GetLocal(x),
                Node.Instr.GetLocal(y),
                Node.Instr.I32Add
            )
        }

        val add3 = makeFunc1("add3", Node.Type.Value.I32, Node.Type.Value.I32) { locals, x ->
            val y = locals.register(Node.Type.Value.I32)
            listOf(
                Node.Instr.I32Const(3),
                Node.Instr.SetLocal(y),
                Node.Instr.GetLocal(x),
                Node.Instr.GetLocal(y),
                Node.Instr.Call(add)
            )
        }

        val fn0 = Node.Func(
            type = Node.Type.Func(params = emptyList(), ret = Node.Type.Value.I32),
            locals = emptyList(),
            instructions = listOf(
                Node.Instr.I32Const(20),
                Node.Instr.Call(allocate),
                Node.Instr.Drop,
                Node.Instr.GetGlobal(watermark)
            )
        )

        val main = registerGlobalFunc("main", fn0)

        return Node.Module(
            funcs = globalFuncs.map { it.second },
            exports = globalFuncs.mapIndexed { i, (name, _) ->
                Node.Export(name, Node.ExternalKind.FUNCTION, i)
            },
            globals = globals.map { it.second },
            memories = listOf(Node.Type.Memory(Node.ResizableLimits(1, null)))
        )
    }
}