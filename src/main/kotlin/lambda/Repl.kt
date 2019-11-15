package lambda

import java.io.File
import java.nio.file.*

fun runFile(file: File) {
    try {
        val sf = Parser(Lexer(file.readText())).parseSourceFile()

        try {
            println("Typechecking: sf") // TODO pretty for SourceFile
            Typechecker().inferSourceFile(sf, file).forEach { println("${it.key.value} : ${it.value.pretty()}") }

            val loweredExprs = Lowering().lowerSourceFile(sf)
            // loweredExprs.forEach { println("Evaluating ${it.first.value} = ${it.second.pretty()}") }
            val result = evalExprs(loweredExprs)
            println(result.pretty())
        } catch (e: Exception) {
            // e.printStackTrace()
        }
    } catch (e: Exception) {
        e.printStackTrace()
    }
}

fun main(args: Array<String>) {
    val filePath = File(args.getOrElse(0) { "examples.l2" })
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
    }
}