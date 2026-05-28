package scala.meta.internal.metals

import play.twirl.parser.TreeNodes.*

import scala.concurrent.ExecutionContextExecutorService
import scala.meta.pc.CancelToken

/**
 *  Provides semantic tokens of Twirl files
 *  according to the LSP specification.
 */
object TwirlSemanticTokensProvider {
  // TODO: Future work suggestion: companion object that will convert
  //       this to/from a character index in the file to a pair.
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
        prevToken: SourceTwirlSemanticToken
    ): DeltaEncodedTwirlSemanticToken = {
      scribe.info( // TODO: remove debug statement
        s"deltaLine=[${prevToken.line}];deltaStart=[${prevToken.column}]"
      )

      val deltaLine = line - prevToken.line
      // relative to 0 or the previous token’s start if they are on the same line
      val deltaStart =
        if (deltaLine == 0) column - prevToken.column
        else column

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
      List(deltaLine, deltaStart, length, tokenType, tokenModifier).map(f =>
        Integer.valueOf(f)
      )

    /**
     * This method will return a `Position` using the delta values defined on the class. It should be
     * made clear that this will not be the source position.
     */
    def toDeltaPos: Position = Position(deltaLine, deltaStart)
  }

  /**
   * @param twirlOffset
   * @param length
   */
  case class ScalaFragmentIdentifier(
      // TODO: for now we wont build new vFile as we go, for simplicity as PoC.
      // scalaOffset: Int, TODO:
      twirlOffset: Position, // TODO: make to int later.
      length: Int,
  )

  /**
   * Contains information about the ScalaFragment, including its source code.
   */
  private type ScalaFragment = (ScalaFragmentIdentifier, String)

  /**
   * The idea is that this class will hold the state for the recursive logic when it comes to
   * "jumping" between the ASTs.
   */
  case class State(
      // maybe these should all probs be options
      topImports: Map[ScalaFragmentIdentifier, String],
      imports: Map[ScalaFragmentIdentifier, String],
      name: Option[ScalaFragment],
      constructor: Option[ScalaFragment],
      comment: Option[ScalaFragment],
      params: Option[ScalaFragment],
      defs: Option[ScalaFragment],
      scalaFragments: Map[ScalaFragmentIdentifier, String],
      // used for making empty defs with the purpose of getting "proper"
      // semantic tokens for
      subtemplatesParams: Seq[ScalaFragment],
  )

  /**
   * This method will traverse the parsed template's AST.
   *
   * @param state       The state to use (contains accumulated fragments and template info)
   * @param template    The parsed template AST
   * @return            The populated state containing accumulated, collected nodes of interest
   *                    from the AST.
   */
  def matchTemplate(
      state: State,
      template: BaseTemplate,
  )(implicit
      ec: ExecutionContextExecutorService,
      rc: ReportContext,
      ct: CancelToken,
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
        state.copy(
          imports = state.imports + (ScalaFragmentIdentifier(
            Position(import_.pos.line, import_.pos.column)
          ) -> import_.code)
        )
      }
      val membersState = members.foldLeft(importedStates) { (state, member) =>
        state.copy(
          scalaFragments = state.scalaFragments + (ScalaFragmentIdentifier(
            Position(member.pos.line, member.pos.column),
            member.code.code,
          ))
        )
      }
      val subTemplateState = sub.foldLeft(membersState) { (state, sub) =>
        matchTemplate(state, template = sub)
      }

      nodes.foldLeft(subTemplateState)((state, node) => matchNode(node, state))
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
        // val namePos = Position(line = name.pos.line, column = name.pos.column)
        def saveState: State = state.copy(scalaFragments =
          state.scalaFragments + (
            ScalaFragmentIdentifier(
              Position(name.pos.line, name.pos.column),
              name.str.length,
            ) -> name.str
          )
        )
        val declaredState = declaration match {
          case Left(isVarOrDef) =>
            isVarOrDef match {
              case true => // var
                // TODO: Potential future work here for emitting different tokens for def/val, lazy/eager vals(?)
                saveState
              case _: Boolean => // def
                saveState
            }
          case Right(isLazyVal) =>
            isLazyVal match {
              case true => // lazy val
                saveState
              case _: Boolean => // eager val
                saveState
            }
        }
        val paramsCollected = declaredState.copy(
          subtemplatesParams = declaredState.subtemplatesParams.appended(
            (
              ScalaFragmentIdentifier(
                Position(params.pos.line, params.pos.column),
                params.str.length,
              ),
              params.str,
            )
          )
        )
        matchCommonTemplateMeta(
          state = paramsCollected,
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
            state.copy(
              constructor = Some(
                (
                  ScalaFragmentIdentifier(
                    Position(
                      constructor.params.pos.line,
                      constructor.params.pos.column,
                    ),
                    constructor.params.str.length,
                  ),
                  constructor.params.str,
                )
              )
            )
          case None => state
        }
        val commentState = comment match {
          case Some(value) => constructorState
//            emitter.resolveTokens(
//              state = constructorState,
//              pos = Position(line = value.pos.line, column = value.pos.column),
//              str = value.msg,
//              tokenType = SemanticTokenTypes.Comment,
//              tokenModifier = SemanticTokenModifiers.Documentation,
//            )
          case None => constructorState
        }
        val paramsState = commentState.copy(
          params = (
            ScalaFragmentIdentifier(
              Position(params.pos.line, params.pos.column),
              params.str.length,
            ),
            params.str,
          )
        )
        val topImportsStates = topImports.foldLeft(paramsState) {
          (state__, top) =>
            state__.copy(
              topImports = state__.topImports + (
                // TODO: Potential future work or refactor - add a companion object for this case class to auto construct it
                ScalaFragmentIdentifier(
                  Position(top.pos.line, top.pos.column),
                  top.code.length,
                ) -> top.code
              )
            )
//            emitter.resolveTokens(
//              state = state__,
//              pos = Position(line = top.pos.line, column = top.pos.column),
//              str = top.code,
//              tokenType = SemanticTokenTypes.Namespace,
//              tokenModifier = SemanticTokenModifiers.Modification,
//            )
        }

        matchCommonTemplateMeta(
          state = topImportsStates,
          imports = imports,
          members = members,
          sub = sub,
          nodes = nodes,
        )
    }
  }

  /**
   * Matches a node of the Twirl AST & accumulates its contents in `state`.
   *
   * @param node       The node to match from the AST
   * @param state      The state that will accumulate node contents. Later, this can be
   *                   enumerated upon and a virtual file can be constructed from the contents
   *                   accumulated into a state.
   * @return           A new state, reflecting the newly matched node entry.
   */
  def matchNode(node: TemplateTree, state: State)(implicit
      ec: ExecutionContextExecutorService,
      rc: ReportContext,
      ct: CancelToken,
  ): State = {
    def traverseReassignment(
        state: State,
        ref: Either[SubTemplate, Var],
    ): State = {
      ref match {
        case Left(template) =>
          matchTemplate(
            state = state,
            template = template,
          )
        case Right(var_) =>
          // TODO: Again, an area for potential future work
          state.copy(
            scalaFragments = state.scalaFragments + (
              ScalaFragmentIdentifier(
                Position(var_.pos.line, var_.pos.column),
                var_.code.code.length,
              ) -> var_.code.code
            )
          )
      }
    }
    def traverseScalaExp(state: State, scalaExp: ScalaExp): State = {
      scalaExp.parts.foldLeft(state)(traverseScalaExpPart)
    }
    def traverseBlock(state: State, block: Block): State = {
      matchTemplate(
        state,
        block.contents,
      )
    }

    def traverseScalaExpPart(state: State, part: ScalaExpPart): State = {
      part match {
        case simple @ Simple(code) =>
          state.copy(
            scalaFragments = state.scalaFragments + (
              ScalaFragmentIdentifier(
                Position(simple.pos.line, simple.pos.column),
                code.length,
              ) -> code
            )
          )
        case block @ Block(whitespace, args, _) =>
          traverseBlock(state, block)
      }
    }

    node match {
      case comment @ Comment(msg) =>
        state // do not append any comments onto the output scala
      case Plain(text) => state // do not emit tokens, let tmLanguage remain
      case Display(exp) => traverseScalaExp(state, exp)
      case Reassignment(ref) => traverseReassignment(state, ref)
      case scalaExp @ ScalaExp(parts) =>
        traverseScalaExp(state, scalaExp)
      case _: TemplateTree =>
        ??? // TODO: Should be able to remove this case - should be unreachable...?
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
  def provide(
      template: Template
  )(implicit
      ec: ExecutionContextExecutorService,
      rc: ReportContext,
      ct: CancelToken,
  ) = {
    matchTemplate(
      State(
        topImports = Map.empty[ScalaFragmentIdentifier, String],
        imports = Map.empty[ScalaFragmentIdentifier, String],
        // TODO:  Go through everything else here and make sure that they allow for
        //        usage of the options here (using None/Some(x) where necessary).
        name = None,
        constructor = None,
        comment = None,
        params = None,
        defs = None,
        scalaFragments = Map.empty[ScalaFragmentIdentifier, String],
        subtemplatesParams = Seq.empty,
      ),
      template,
    )
  }
//    matchTemplate(
//      state = State(
//        prevToken = SourceTwirlSemanticToken(0, 0, 0, 0, 0),
//        tokens = Seq(),
//      ),
//      template = template,
//      pos = Position(1, 0),
//      emitter = new Emitter(compiler, path),
//    ).tokens
//      .sortBy(token => (token.line, token.column))
//      .foldLeft(
//        (
//          SourceTwirlSemanticToken(0, 0, 0, 0, 0),
//          List.empty[DeltaEncodedTwirlSemanticToken],
//        )
//      ) { case ((prev, acc), curr) =>
//        val encoded = curr.deltaEncode(prev)
//        (curr, acc.appended(encoded))
//      }
//      ._2
//      .flatMap(token => token.toList)
}
