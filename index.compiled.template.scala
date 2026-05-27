package html

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml

/**/
class index$$TwirlInclusiveDot /*1.6*/ (
    webJarsUtil: org.webjars.play.WebJarsUtil,
    main: main,
) extends _root_.play.twirl.api.BaseScalaTemplate[
      play.twirl.api.Html,
      _root_.play.twirl.api.Format[play.twirl.api.Html],
    ](play.twirl.api.HtmlFormat.Appendable)
    with _root_.play.twirl.api.Template1[Either[
      Iterable[WebJar],
      String,
    ], play.twirl.api.Html] {

  /**/
  def apply /*3.2*/ (
      webjarsOrError: Either[Iterable[WebJar], String]
  ): play.twirl.api.Html = {
    _display_ {
      {

        Seq[Any](
          format.raw /*4.1*/ ("""
"""),
          _display_(
            /*5.2*/ main(title = "WebJars - Web Libraries in Jars") /*5.49*/ {
              _display_(
                Seq[Any](
                  format.raw /*5.51*/ ("""
    """),
                  format.raw /*6.5*/ ("""<!-- Scripts -->
    <script defer src=""""),
                  _display_(
                    /*7.25*/ routes /*7.31*/ .Assets.versioned(
                      "javascripts/index.js"
                    )
                  ),
                  format.raw /*7.72*/ (""""></script>
    """),
                  _display_(
                    /*8.6*/ webJarsUtil /*8.17*/
                      .locate("jquery.typewatch", "jquery.typewatch.js")
                      .script()
                  ),
                  format.raw /*8.76*/ ("""

    """),
                  format.raw /*10.5*/ ("""<!-- Content -->
    <div class="home-bg">
        <!-- Hero -->
        """),
                  _display_( /*13.10*/ sections /*13.18*/ .hero()),
                  format.raw /*13.25*/ ("""

        """),
                  format.raw /*15.9*/ ("""<!-- Popular WebJars -->
        """),
                  _display_(
                    /*16.10*/ sections /*16.18*/ .popular(webjarsOrError)
                  ),
                  format.raw /*16.42*/ ("""
    """),
                  format.raw /*17.5*/ ("""</div>

    <!-- Modals -->
    """),
                  _display_( /*20.6*/ partials /*20.14*/ .fileListModal()),
                  format.raw /*20.30*/ ("""

    """),
                  _display_( /*22.6*/ partials /*22.14*/ .newWebJarModal()),
                  format.raw /*22.31*/ ("""
"""),
                )
              )
            }
          ),
          format.raw /*23.2*/ ("""
"""),
        )
      }
    }
  }

  def render(
      webjarsOrError: Either[Iterable[WebJar], String]
  ): play.twirl.api.Html = apply(webjarsOrError)

  def f: ((Either[Iterable[WebJar], String]) => play.twirl.api.Html) =
    (webjarsOrError) => apply(webjarsOrError)

  def ref: this.type = this

}

/*
                  -- GENERATED --
                  SOURCE: /Users/w/Documents/git/webjars/app/views/index.scala.html
                  HASH: 79ee8903903deb4b4bd635bf06ad98bbc7c58e95
                  MATRIX: 301->5|633->63|760->114|787->116|842->163|881->165|912->170|979->211|993->217|1054->258|1096->275|1115->286|1194->345|1227->351|1328->425|1345->433|1373->440|1410->450|1471->484|1488->492|1533->516|1565->521|1624->554|1641->562|1678->578|1711->585|1728->593|1766->610|1798->612
                  LINES: 12->1|15->3|20->4|21->5|21->5|21->5|22->6|23->7|23->7|23->7|24->8|24->8|24->8|26->10|29->13|29->13|29->13|31->15|32->16|32->16|32->16|33->17|36->20|36->20|36->20|38->22|38->22|38->22|39->23
                  -- GENERATED --
 */
