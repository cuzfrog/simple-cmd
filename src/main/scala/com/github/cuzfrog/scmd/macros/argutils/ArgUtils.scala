package com.github.cuzfrog.scmd.macros.argutils

import scala.collection.immutable
import scala.meta.Stat

/*
 * Intellij code inspection is too slow, split implementation into objects.
 */

private[macros] object ArgUtils {
  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(stats: immutable.Seq[Stat]): immutable.Seq[Stat] =
    ConvertParsedImpl.convertParsed(stats)

  /** Scala meta generated fields need explicit types to inform IDE. */
  def addExplicitType(stat: Stat): Stat = AddExplicitTypeImpl.addExplicitType(stat)

}
