package scala.meta.internal.metals

import scala.meta.internal.pc.SemanticTokens

import org.eclipse.lsp4j.SemanticTokenModifiers
import org.eclipse.lsp4j.SemanticTokenTypes
import play.twirl.parser.TreeNodes._

/**
 *  Provides semantic tokens of Twirl files
 *  according to the LSP specification.
 */
object TwirlSemanticTokensProvider {
  case class Position(
      line: Int,
      column: Int,
  )

  /**
   * A constructed semantic token that contains no delta encoding.
   */
  case class SourceTwirlSemanticToken(
      length: Int,
      tokenType: Int,
      tokenModifiers: Int,
      line: Int,
      column: Int,
  ) {
    def getSrcPos: Position = Position(line = line, column = column)

    def deltaEncode(
        prevToken: DeltaEncodedTwirlSemanticToken
    ): DeltaEncodedTwirlSemanticToken = {
      scribe.info( // TODO: remove debug statement
        s"deltaLine=[${prevToken.deltaLine}];deltaStart=[${prevToken.deltaStart}]"
      )

      val deltaLine = line - prevToken.deltaLine

      // relative to 0 or the previous token’s start if they are on the same line
      val deltaStart =
        if (deltaLine == 0)
          column - prevToken.deltaStart
        else
          column

      DeltaEncodedTwirlSemanticToken(
        deltaLine = deltaLine,
        deltaStart = deltaStart,
        length = length,
        tokenType = tokenType,
        tokenModifier = tokenModifiers,
      )
    }
  }

  /**
   * A case class designed to make it easier to make it easier to construct and convert semantic
   * tokens to/from their raw representation that is expected by LSP.
   *
   * @note
   *   **This class should not be instantiated directly for creating delta tokens.** The `encodeDelta(...)` method should be
   *   used to convert a `SourceTwirlSemanticTokens` to a `DeltaEncodedTwirlSemanticTokens`.
   */
  case class DeltaEncodedTwirlSemanticToken(
      deltaLine: Int,
      deltaStart: Int,
      length: Int,
      tokenType: Int,
      tokenModifier: Int,
  ) {
    def toList: List[Integer] =
      List(deltaLine, deltaStart, length, tokenType, tokenModifier).map(f => new Integer(f))

    /**
     * This method will return a `Position` using the delta values defined on the class. It should be
     * made clear that this will not be the source position.
     */
    def toDeltaPos: Position = Position(deltaLine, deltaStart)
  }

  /**
   * The idea is that this class will hold the state for the recursive logic when it comes to
   * "jumping" between the ASTs.
   *
   * @param prevToken
   *   The previous `SourceTwirlSemanticToken`
   * @param tokens
   *   All of the collected semantic tokens thus far
   */
  case class State(
      prevToken: SourceTwirlSemanticToken,
      tokens: Seq[SourceTwirlSemanticToken],
  ) {
    def getPrevPos: Position = prevToken.getSrcPos
  }

  object Emitter {
    def resolveTokens(
        state: State,
        pos: Position,
        str: String,
        tokenType: String,
        tokenModifier: String,
    ): State = {
      val thisToken = SourceTwirlSemanticToken(
        line = pos.line,
        column = pos.column,
        length = str.length,
        tokenType = SemanticTokens.getTypeId(tokenType),
        tokenModifiers = SemanticTokens.getModifierId(tokenModifier),
      )

      State(
        prevToken = thisToken,
        tokens = state.tokens.appended(thisToken),
      )
    }

    def emitScala(
        state: State,
        pos: Position,
        str: String,
    ): State = {
      scribe.info(s"Scala emitted: [$str].")
      // TODO: Call the current semantic tokens impl for Scala.
      Emitter.resolveTokens(
        state,
        pos,
        str,
        tokenType = SemanticTokenTypes.Method,
        tokenModifier = SemanticTokenModifiers.Async,
      )
    }

    def emitComment(
        state: State,
        pos: Position,
        str: String,
    ): State =
      Emitter.resolveTokens(
        state,
        pos,
        str,
        tokenType = SemanticTokenTypes.Enum,
        tokenModifier = SemanticTokenModifiers.Abstract,
      )

    /**
     * Used for template imports.
     * @example
     *   {{{
     * @import
     *   java.net.URLEncoder imports=[ArrayBuffer(Simple(import java.net.URLEncoder))]
     *   }}}
     * @note
     *   This will internally call `emitScala(...)` on the specified tokens.
     */
    def emitImports(
        state: State,
        pos: Position,
        str: String,
    ): State = emitScala(state, pos, str)

    /**
     * This is the details that are provided in the `@this(...)` expression in Twirl.
     * @note
     *   This will internally call `emitScala(...)` on the specified tokens.
     * @example
     *   {{{
     * @this(main: main)
     * constructor=[Some(Constructor(None,(main: main)))]
     *   }}}
     */
    def emitConstructor(
        state: State,
        pos: Position,
        str: String,
    ): State =
      /**
       * TODO: This is an unused method.
       */
      Emitter.resolveTokens(
        state,
        pos,
        str,
        tokenType = SemanticTokenTypes.Comment,
        tokenModifier = SemanticTokenModifiers.Documentation,
      )

    /** Emits template header params. */
    def emitParams(
        state: State,
        pos: PosString,
        str: String,
    ): State =
      // Should this just be using emitScala?
      Emitter.resolveTokens(
        state,
        pos = Position(
          line = pos.pos.line,
          column = pos.pos.column,
        ),
        str,
        tokenType = SemanticTokenTypes.Comment,
        tokenModifier = SemanticTokenModifiers.Documentation,
      )
  }

  /**
   * This method's purpose is to traverse and identify the Twirl template's metadata.
   *
   * @param state
   * @param template
   * @param pos
   * @return
   */
  def matchTemplate(
      state: State,
      template: BaseTemplate,
      pos: Position,
  ): State = {

    /**
     * Matches common template metadata. This applies to all templates that we might receive and
     * therefore have to match against & process tokens for.
     *
     * Matches and processes `[state, imports, sub, nodes]` of a template.
     *
     * @param state
     *   The state to use.
     * @param imports
     *   The imports that are used in this template.
     * @param members
     *   The members of this template.
     * @param sub
     *   The subtemplates of this template.
     * @param nodes
     *   The nodes of this tree.
     * @return
     *   State -- the new State for this template.
     */
    def matchCommonTemplateMeta(
        state: State,
        imports: collection.Seq[Simple],
        members: collection.Seq[LocalMember],
        sub: collection.Seq[SubTemplate],
        nodes: collection.Seq[TemplateTree],
    ): State = {
      val importedStates = imports.foldLeft(state) { (state, import_) =>
        Emitter.emitScala(
          state = state,
          Position(import_.pos.line, import_.pos.column),
          import_.code,
        )
      }
      val membersState = members.foldLeft(importedStates) { (state, member) =>
        Emitter.emitScala(
          state = state,
          pos = Position(
            line = member.pos.line,
            column = member.pos.column,
          ),
          str = member.code.code,
        )
      }
      val subTemplateState = sub.foldLeft(membersState) { (state, sub) =>
        matchTemplate(
          state = state,
          template = sub,
          pos = Position(
            line = sub.pos.line,
            column = sub.pos.column,
          ),
        )
      }

      nodes.foldLeft(subTemplateState)((state, node) =>
        matchNode(node = node, state = state)
      )
    }

    /**
     * A case class representing the data that will be stored once a comment is detected and
     * extracted from some input `text`.
     *
     * @see
     *   [[Traverser.getCommentNodes]]
     */
    case class CommentSrcPos(
        pos: Position,
        str: String,
    ) {

      /**
       * @TODO:
       *   Figure out as to whether this actually works...
       *
       * @param lineOffset
       * @return
       */
      def getSourceToken(lineOffset: Int): SourceTwirlSemanticToken =
        SourceTwirlSemanticToken(
          length = this.str.length,
          tokenType = SemanticTokens.getTypeId(SemanticTokenTypes.Comment),
          tokenModifiers = SemanticTokens.getModifierId(
            SemanticTokenModifiers.Documentation
          ),
          line = this.pos.line + lineOffset,
          column = this.pos.column,
        )
    }
    def getCommentNodes(text: String): List[CommentSrcPos] = {
      case class BeginRegionMarker(
          pos: Position,
          rawSrcPos: Int,
      )
      sealed trait ScannerModes
      case object Text extends ScannerModes
      case object BlockComment extends ScannerModes
      case object TwirlComment extends ScannerModes
      case object Ignore extends ScannerModes
      var rawSrcPos = 0
      var pos: Position = Position(1, 0)
      var beginRegion: Option[BeginRegionMarker] = None
      var mode: ScannerModes = Text
      var comments: List[CommentSrcPos] = List()

      def moveCursorForwardByOne(): Unit = {
        rawSrcPos = rawSrcPos + 1
        pos = Position(
          line = pos.line,
          column = pos.column + 1,
        )
      }

      while (rawSrcPos < text.length) {
        val char = text.charAt(rawSrcPos).toLower.toString
        char match {
          case x if x == "\n" =>
            // newline, increment pos.line and set pos.col to 0
            pos = Position(
              line = pos.line + 1,
              column = 0,
            )
          case _: String => Nil
        }
        mode match {
          case Text =>
            char match {
              // received normal text; continue matching until start of
              // either a line comment, block comment, or twirl comment
              case x if x == "/" => // start with block comment
                text.charAt(rawSrcPos + 1).toLower.toString match {
                  case y if y == "*" =>
                    // We are now inside a block comment.
                    beginRegion = Some(
                      BeginRegionMarker(pos = pos, rawSrcPos = rawSrcPos)
                    )
                    mode = BlockComment
                    moveCursorForwardByOne()
                  case y if y == "/" =>
                    // We are now inside a // line comment
                    beginRegion = None
                    mode = Text
                    val endIndex = text.indexOf("\n", rawSrcPos + 2)
                    endIndex match {
                      case x if x == -1 =>
                        // end of block comment extends until end of text, consume whole text.
                        comments = comments.appended(
                          CommentSrcPos(
                            pos = pos,
                            str = text.substring(rawSrcPos, text.length),
                          )
                        )
                        rawSrcPos = text.length
                      case _: Int =>
                        comments = comments.appended(
                          CommentSrcPos(
                            pos = pos,
                            str = text.substring(rawSrcPos, endIndex),
                          )
                        )
                        rawSrcPos = endIndex + 1
                    }
                  case _: String => moveCursorForwardByOne()
                }
              case x if x == "@" => // check if we are in an @* comment block... and set mode.
                text.charAt(rawSrcPos + 1).toLower.toString match {
                  case y if y == "*" =>
                    // We are now inside a twirl block comment.
                    beginRegion = Some(
                      BeginRegionMarker(pos = pos, rawSrcPos = rawSrcPos)
                    )
                    mode = TwirlComment
                    moveCursorForwardByOne()
                  case _: String => moveCursorForwardByOne()
                }
              case _: String => moveCursorForwardByOne()
            }
          case TwirlComment =>
            text.charAt(rawSrcPos).toLower.toString match {
              case x if x == "*" =>
                text.charAt(rawSrcPos + 1).toLower.toString match {
                  case y if y == "@" =>
                    mode = Text
                    comments = comments.appended(
                      CommentSrcPos(
                        pos = beginRegion.get.pos,
                        str = text.substring(
                          beginRegion.get.rawSrcPos,
                          rawSrcPos + 2,
                        ),
                      )
                    )
                    beginRegion = None
                  case _: String => moveCursorForwardByOne()
                }
              case _: String => moveCursorForwardByOne()
            }
          case BlockComment =>
            text.charAt(rawSrcPos).toLower.toString match {
              case x if x == "*" =>
                text.charAt(rawSrcPos + 1).toLower.toString match {
                  case y if y == "/" =>
                    mode = Text
                    comments = comments.appended(
                      CommentSrcPos(
                        pos = beginRegion.get.pos,
                        str = text.substring(
                          beginRegion.get.rawSrcPos,
                          rawSrcPos + 2,
                        ),
                      )
                    )
                    beginRegion = None
                  case _: String => moveCursorForwardByOne()
                }
              case _: String => moveCursorForwardByOne()
            }
          case Ignore => moveCursorForwardByOne()
        }
      }
      comments
    }

    template match {
      case BlockTemplate(imports, members, sub, nodes) =>
        matchCommonTemplateMeta(state, imports, members, sub, nodes)
      case SubTemplate(
            declaration,
            name,
            params,
            imports,
            members,
            sub,
            nodes,
          ) =>
        val namePos = Position(line = name.pos.line, column = name.pos.column)
        val declaredState = declaration match {
          case Left(isVarOrDef) =>
            isVarOrDef match {
              case true => // var
                Emitter.resolveTokens(
                  state = state,
                  pos = namePos,
                  str = name.str,
                  tokenType = SemanticTokenTypes.Variable,
                  tokenModifier =
                    "0", // TODO: a workaround to give no token modifier
                )
              case _: Boolean => // def
                Emitter.resolveTokens(
                  state = state,
                  pos = namePos,
                  str = name.str,
                  tokenType = SemanticTokenTypes.Function,
                  tokenModifier = "0",
                )
            }
          case Right(isLazyVal) =>
            isLazyVal match {
              case true => // lazy val
                Emitter.resolveTokens(
                  state = state,
                  pos = namePos,
                  str = name.str,
                  tokenType = SemanticTokenTypes.Variable,
                  tokenModifier = SemanticTokenModifiers.Readonly,
                )
              case _: Boolean => // eager val
                Emitter.resolveTokens(
                  state = state,
                  pos = namePos,
                  str = name.str,
                  tokenType = SemanticTokenTypes.Variable,
                  tokenModifier = SemanticTokenModifiers.Definition,
                )
            }
        }
        val scalaEmittedState = Emitter.emitScala(
          state = declaredState,
          pos = Position(
            line = params.pos.line,
            column = params.pos.column,
          ),
          str = params.str,
        )
        matchCommonTemplateMeta(
          state = scalaEmittedState,
          imports = imports,
          members = members,
          sub = sub,
          nodes = nodes,
        )
      case Template(
            constructor,
            comment,
            params,
            topImports,
            imports,
            members,
            sub,
            nodes,
          ) =>
        val constructorState = constructor match {
          case Some(constructor) =>
            Emitter.resolveTokens(
              state = state,
              pos = pos,
              str = constructor.params.str,
              tokenType = SemanticTokenTypes.Parameter,
              tokenModifier = SemanticTokenModifiers.Declaration,
            )
          case None => state
        }
        val commentState = comment match {
          case Some(value) =>
            Emitter.resolveTokens(
              state = constructorState,
              pos =
                Position(line = value.pos.line, column = value.pos.column),
              str = value.msg,
              tokenType = SemanticTokenTypes.Comment,
              tokenModifier = SemanticTokenModifiers.Documentation,
            )
          case None => constructorState
        }
        val paramsState = Emitter.emitScala(
          state = commentState,
          pos = Position(line = params.pos.line, column = params.pos.column),
          str = params.str,
        )
        val topImportsStates = topImports.foldLeft(paramsState) {
          (state__, top) =>
            Emitter.resolveTokens(
              state = state__,
              pos = Position(line = top.pos.line, column = top.pos.column),
              str = top.code,
              tokenType = SemanticTokenTypes.Namespace,
              tokenModifier = SemanticTokenModifiers.Modification,
            )
        }
        val constructorEmittedState = constructor match {
          case Some(constructor) =>
            scribe.info(s"CONSTRUCTOR STR=[${constructor.params.str}]")

            val paramsText = constructor.params.str
            val commentNodes = getCommentNodes(paramsText).map { comment =>
              scribe.info(
                s"[commentNodes#map]: Detected comment [${comment.str}] inside constructor."
              )
              comment.getSourceToken(topImportsStates.prevToken.line)
            }

            State(
              prevToken = SourceTwirlSemanticToken(
                length = params.str.length,
                tokenType = SemanticTokens.getTypeId(
                  SemanticTokenTypes.Parameter
                ),
                tokenModifiers = 0,
                line = params.pos.line,
                column = params.pos.column,
              ),
              tokens = topImportsStates.tokens ++ commentNodes,
            )

          // TODO: need to apply semantic tokens for constructor comments...
          // constructor.comment match
          // case Some(value) => ???
          // case None => ???
          case None => topImportsStates
        }

        matchCommonTemplateMeta(
          state = constructorEmittedState,
          imports = imports,
          members = members,
          sub = sub,
          nodes = nodes,
        )
    }
  }

  def matchNode(node: TemplateTree, state: State): State = {
    def traverseReassignment(
        state: State,
        ref: Either[SubTemplate, Var],
    ): State = {
      ref match {
        case Left(template) =>
          matchTemplate(
            state = state,
            template = template,
            pos =
              Position(line = template.pos.line, column = template.pos.column),
          )
        case Right(var_) =>
          // Just emit everything as Scala and then let Metals provide
          // the semantic tokens for this - I could go and do it myself
          // but there is not really much of a point in doing that if Metals
          // can just go ahead and do it for us anyway
          Emitter.emitScala(
            state = state,
            pos = Position(
              line = var_.pos.line,
              column = var_.pos.column,
            ),
            str = var_.code.toString,
          )
      }
    }
    def traverseScalaExp(state: State, scalaExp: ScalaExp): State = {
      scalaExp.parts.foldLeft(state)(traverseScalaExpPart)
    }
    def traverseBlock(state: State, block: Block): State = {
      // TODO: Return the result of matchTemplate here directly.
      val tokens =
        matchTemplate(
          state,
          block.contents,
          Position(block.pos.line, block.pos.column),
        ).tokens
      State(
        prevToken = state.prevToken,
        tokens,
      )
    }

    def traverseScalaExpPart(state: State, part: ScalaExpPart): State = {
      part match {
        case simple @ Simple(code) =>
          Emitter.emitScala(
            state = state,
            pos = Position(simple.pos.line, simple.pos.column),
            str = code,
          )
        case block @ Block(whitespace, args, _) =>
          traverseBlock(state, block)
      }
    }

    node match {
      case comment @ Comment(msg) =>
        Emitter.emitComment(
          state,
          pos = Position(comment.pos.line, comment.pos.column),
          str = msg,
        )
      case Plain(text) => state // do not emit tokens, let tmLanguage remain
      case Display(exp) => traverseScalaExp(state, exp)
      case Reassignment(ref) => traverseReassignment(state, ref)
      // for scalaExp, foldLeft onto it
      case scalaExp @ ScalaExp(parts) =>
        traverseScalaExp(state = state, scalaExp = scalaExp)
      case _: TemplateTree => ??? // TODO: Should be able to remove this case - should be unreachable...?
    }
  }

  /**
    * This is the core method that will handle the main tree traversal of the
    * Twirl Abstract Syntax Tree (AST). This function will traverse the tree,
    * sort its nodes, delta encode them, and return them in accordance with
    * the LSP specification.
    *
    * @param template   The template to traverse, obtained by parsing the Twirl
    *                   file using `TwirlParser`.
    * @return           The flattened, delta-encoded source tokens, ready to be
    *                   provided to the IDE.
    */
  def provide(template: Template): List[Integer] =
    matchTemplate(
      state = State(
        prevToken = SourceTwirlSemanticToken(0, 0, 0, 0, 0),
        tokens = Seq(),
      ),
      template = template,
      pos = Position(1, 0),
    ).tokens
      .sortBy(token => (token.line, token.column))
      .foldLeft(List(DeltaEncodedTwirlSemanticToken(0, 0, 0, 0, 0))) {
        (prev, curr) =>
          prev.appended(curr.deltaEncode(prev.last))
      }
      .flatMap(token => token.toList)
}
