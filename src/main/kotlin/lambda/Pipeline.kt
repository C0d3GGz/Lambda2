package lambda

import lambda.renamer.renameModule
import lambda.syntax.ModuleHeader
import lambda.syntax.Name
import lambda.syntax.Namespace
import lambda.syntax.SourceFile
import java.io.File


class Graph(val vertices: List<String>, edges: List<Pair<Int, Int>>) {

    val numVertices = vertices.size
    val adjacency = List(numVertices) { BooleanArray(numVertices) }

    init {
        for (edge in edges) adjacency[edge.first][edge.second] = true
    }

    fun hasDependency(r: Int, todo: List<Int>): Boolean {
        for (c in todo) if (adjacency[r][c]) return true
        return false
    }

    fun topoSort(): List<String> {
        val result = mutableListOf<String>()
        val todo = MutableList<Int>(numVertices) { it }
        outer@ while (todo.isNotEmpty()) {
            for ((i, r) in todo.withIndex()) {
                if (!hasDependency(r, todo)) {
                    todo.removeAt(i)
                    result.add(vertices[r])
                    continue@outer
                }
            }
            throw Exception("Graph has cycles")
        }
        return result
    }
}

enum class Target {
    Wasm, Interpreter
}

data class Config(val target: Target, val main: Namespace) {
    companion object {
        val default = Config(Target.Interpreter, Namespace(listOf(Name("Main"))))
    }
}

class Pipeline {

    fun compileModules(files: List<File>, config: Config = Config.default): MutableList<Pair<Namespace, Interface>> {
        val headers = files.map(::parseModuleHeader)
        val vertices = headers.map { it.namespace.toString() }
        val vertexIndex: (Namespace) -> Int = { ns ->
            val i = vertices.indexOf(ns.toString())
            if (i < 0) throw Exception("unknown module $ns")
            i
        }
        val moduleGraph = Graph(vertices, headers.flatMap { (name, imports) ->
            val i = vertexIndex(name)
            imports.map { i to vertexIndex(it.namespace) }
        })


        val ordering = moduleGraph.topoSort()
        val sfs = files.map { Parser(Lexer(it.readText())).parseSourceFile() }
        val interfaces = mutableListOf<Pair<Namespace, Interface>>()
        val sfs2 = mutableListOf<SourceFile>()

        ordering.forEach { o ->
            val sf = sfs.find { it.header.namespace.toString() == o }!!
            val renamedSf = renameModule(sf)
            sfs2 += renamedSf
            val newInterface = Typechecker().inferSourceFile(interfaces, renamedSf)

            interfaces += renamedSf.header.namespace to newInterface
        }

        val rts = sfs2.map { it.header.namespace to Lowering().lowerSourceFile(it, interfaces) }
        var result: RTExpression? = null

        rts.fold(initialContext()) { acc, (ns, exprs) ->
            val (resCtx, expr) = evalExprs(acc, ns, exprs)

            if (ns == config.main)
                result = expr

            resCtx
        }

        println(result)

        return interfaces
    }

    private fun parseModuleHeader(file: File): ModuleHeader = Parser(Lexer(file.readText())).parseModuleHeader()
}

/*fun runFile(file: File) {
    try {
        val sf = Parser(Lexer(file.readText())).parseSourceFile()
        println(sf.header)

        try {
            println("Typechecking: sf") // TODO pretty for SourceFile
            Typechecker().inferSourceFile(sf).forEach { println("${it.key.value} : ${it.value.pretty()}") }
        } catch (e: Exception) {
            e.printStackTrace()
        }

        try {
            val loweredExprs = Lowering().lowerSourceFile(sf)
            // loweredExprs.forEach { println("Evaluating ${it.first.value} = ${it.second.pretty()}") }
            val result = evalExprs(loweredExprs)
            println(result.pretty())
        } catch (e: Exception) {
            e.printStackTrace()
        }
    } catch (e: Exception) {
        e.printStackTrace()
    }
}*/

fun main(args: Array<String>) {
    Pipeline()
        .compileModules(
            listOf(File("std/list.l2"), File("gol.l2"), File("std/option.l2")),
            Config(Target.Interpreter, Namespace(listOf(Name("GameOfLife"))))
        )

    /*val filePath = File(args.getOrElse(0) { "std/list.l2" })
    runFile(filePath)
    val watchService = FileSystems.getDefault().newWatchService()
    val path = Paths.get(".")
    path.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY)
    while (true) {
        val key = watchService.take() ?: break
        val shouldRebuild = key.pollEvents().any {
            val p = it.context() as Path
            p.toString().endsWith(".l2")
        }
        if (shouldRebuild) {
            runFile(filePath)
        }
        key.reset()
    }*/
}