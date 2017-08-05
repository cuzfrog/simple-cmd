package com.github.cuzfrog.scmd.internal

import scala.annotation.StaticAnnotation
import scala.collection.immutable
import scala.meta._

/** Reduce boilerplate of equality overriding in Node. */
private[scmd] class EqualityOverridingMacro extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Defn.Class(mods, name, tparams, ctor, template) =>
        EqualityOverridingImpl.expand(mods, name, tparams, ctor, template)
      case Term.Block(
      Seq(Defn.Class(mods, name, tparams, ctor, template),
      companion: Defn.Object)) =>
        Term.Block(
          immutable.Seq(
            EqualityOverridingImpl.expand(mods, name, tparams, ctor, template),
            companion)
        )
      case _ =>
        abort(s"@EqualityOverridingMacro must annotate a class.")
    }
  }
}

private object EqualityOverridingImpl {
  final def expand(mods: immutable.Seq[Mod],
                   name: Type.Name,
                   tparams: immutable.Seq[Type.Param],
                   ctor: Ctor.Primary,
                   template: Template): Defn.Class = {
    val Ctor.Primary(_, _, paramss) = ctor
    require(
      paramss.flatten.collectFirst {
        case p@Term.Param(_, Term.Name("entity"), _, _) => p
      }.nonEmpty,
      s"No param named entity found in class ${name.value}."
    )

    val nameWithTpe = if(tparams.nonEmpty) t"$name[_]" else name

    val moreStats = immutable.Seq(
      q"override def hashCode(): Int = entity.hashCode * 3 + 17",
      q"""override def equals(obj: scala.Any): Boolean = {
            if (!this.canEqual(obj)) return false
            obj.asInstanceOf[$nameWithTpe].entity == this.entity
          }""",
      q"override def canEqual(that: Any): Boolean = that.isInstanceOf[$nameWithTpe]"
    )

    val updatedTemplate =
      template.copy(stats = Option(moreStats).map(_ ++ template.stats.getOrElse(Nil)))
    Defn.Class(mods, name, tparams, ctor, updatedTemplate)
  }
}