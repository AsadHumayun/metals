import scala.io.Codec
import java.io.FileWriter
import scala.meta.io.AbsolutePath
import java.nio.file.Paths
import play.twirl.compiler.TwirlCompiler
import scala.meta.inputs.Input
import scala.meta.internal.metals.TwirlAdjustments
import org.eclipse.lsp4j.{Range => LspRange, Position}

import java.io.File

// Sample code from a simple view in WebJars

val path =
  Paths
    .get(System.getProperty("user.dir"))
    .getParent
    .resolve("webjars")
    .resolve("app")
    .resolve("views")
    .resolve("index.scala.html")
    .toAbsolutePath

// simulates a twirl template that was (ideally) taken from buffers.get(...)
val codeFromBuffer = scala.io.Source.fromFile(path.toFile).mkString

// where to write the compiled vFile contents to (for debugging)
val outPath =
  Paths
    .get(System.getProperty("user.dir"))
    .resolve("index.compiled.template.scala")
    .toFile

val twirlFile = Input.VirtualFile(path.toUri.toString, codeFromBuffer)
val (vFile, /*twirl->scala*/ _, adjustments) =
  TwirlAdjustments(twirlFile, "2.13.0")

// Write compiled file output for debugging/checking src positions between scala<->twirl
val writer = new FileWriter(outPath)
writer.write(vFile.value)
writer.close()

val scalaRange = new LspRange(new Position(20, 43), new Position(20, 43 + 55))

adjustments.adjustRange(scalaRange)
adjustments.adjustPos(new Position(12, 40))
// somewhere this has gone wrong.

TwirlCompiler
  .compileVirtual(
    content = vFile.value,
    source = path.toFile,
    sourceDirectory = path.getParent.toFile,
    resultType = "play.twirl.api.HtmlFormat.Appendable",
    formatterType = "play.twirl.api.Html",
    // additionalImports = playImports(
    // 	TwirlCompiler.defaultImports(scalaVersion),
    // 	playVersion,
    // ),
    // constructorAnnotations = playDI(playVersion),
    codec = Codec(
      scala.util.Properties.sourceEncoding
    ),
    // scalaVersion = Some(scalaVersion),
    inclusiveDot = true,
  )
  .mapPosition(301)
