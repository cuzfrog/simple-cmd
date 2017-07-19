package com.github.cuzfrog.scmd.macros

import scala.meta._
import scala.collection.immutable


private[scmd] class ScmdDefMacro {

  /** Override this for testing. */
  protected val isTestMode: Boolean = false

  final def expand(name: Type.Name, stats: immutable.Seq[Stat]): Defn.Class = {

    /**
      * A RawArg is macro time instance of arg definition.
      * A TermArg is macro time term of arg Node.
      *
      * This step collects arg defs from source code, checking syntax,
      * then turn them into Node terms.
      */
    val argDefs = RawArg.collectRawArg(stats).map(TermArg.raw2termArg)

    /**
      * ArgTree represents structure of user defined args.
      * A TermArgTree is a premature ArgTree consisting of Terms, Which contains the topological information.
      *
      * This step build an TermArgTree by the order of user-defined args in source code,
      * then turn it into a single macro-reify-ready Term.
      */
    val argTree = TreeBuilder.buildArgTreeByIdx(argDefs).defnTerm

    /** For testing. */
    val privateMod = if(isTestMode) mod"private[scmd]" else mod"private[this]"

    val headers = List(
      q"import com.github.cuzfrog.scmd._"
    )
    val addMethods = List(
      q"$privateMod def argTree: com.github.cuzfrog.scmd.parse.ArgTree = $argTree",
      q"""def parse(args: Array[String]) = { args.foreach(println) }"""
    )
    val moreStats = headers ++ stats ++ addMethods

    //abort("dev...")

    q"""
          class $name { ..$moreStats }
        """
  }
}

