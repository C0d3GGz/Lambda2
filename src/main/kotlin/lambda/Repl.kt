package lambda

import java.io.File
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds

fun runFile(file: File) {
    try {
        val sf = Parser(Lexer(file.readText())).parseSourceFile()

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
}

fun main(args: Array<String>) {
    val filePath = args.getOrElse(0) {"examples.l2"}
    runFile(File(filePath))
    val watchService = FileSystems.getDefault().newWatchService()
    val path = Paths.get(".")
    path.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY)
    while (true) {
        val key = watchService.take() ?: break
        for (event in key.pollEvents()) {
            val changed = event.context() as Path
            if (changed.endsWith("examples.lam2")) {
                runFile(changed.toFile())
            }
        }
        key.reset()
    }
}