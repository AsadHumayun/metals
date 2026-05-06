import play.twirl.compiler.TwirlCompiler

import java.io.File

// Sample code from a simple view in WebJars
val code =  """
@this(webJarsUtil: org.webjars.play.WebJarsUtil, main: main)

@(webjarsOrError: Either[Iterable[WebJar], String])

@main(title = "WebJars - Web Libraries in Jars") {
    <!-- Scripts -->
    <script defer src="@routes.Assets.versioned("javascripts/index.js")"></script>
    @webJarsUtil.locate("jquery.typewatch", "jquery.typewatch.js").script()

    <!-- Content -->
    <div class="home-bg">
        <!-- Hero -->
        @sections.hero()

        <!-- Popular WebJars -->
        @sections.popular(webjarsOrError)
    </div>

    <!-- Modals -->
    @partials.fileListModal()

    @partials.newWebJarModal()
}
"""

val target = new File("/Users/w/Documents/git/webjars/app/views/index.scala.html")
val targetDir = new File("/Users/w/Documents/git/webjars/app/views/")

TwirlCompiler.compileVirtual(
  content = code,
  target,
  targetDir,
  "resultType",
  "formatTypeIThink"
)._content
