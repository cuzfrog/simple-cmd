package com.github.cuzfrog.scmd.internal

import scala.annotation.StaticAnnotation
import scala.meta._

import scala.collection.immutable

/** Reduce boilerplate in Arguments creation with traits. */
private[scmd] class ArgCreationMacro extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"private def $name[T](..$params) = $_" if name.value.startsWith("create") =>
        val argValueCtor = {
          val argValueOpt = params collectFirst {
            case param"_value: Option[T]" => t"SingleValue"
            case param"_value: Seq[T]" => t"VariableValue"
          }
          Ctor.Ref.Name(
            argValueOpt.getOrElse(abort(s"No _value param field found in method:$name")).value
          )
        }

        val argCtor = {
          val argType = name.value match {
            case "createParam" => t"Parameter"
            case "createOpt" => t"OptionArg"
            case bad => abort(s"Method $bad not supported.")
          }
          Ctor.Ref.Name(argType.value)
        }

        def buildStat(argCtor: Ctor.Ref, argValueCtor: Ctor.Ref) = {
          val params = {
            val optParams = if(name.value.contains("Opt")) List(q"abbr") else Nil
            val commonParams = List(q"name", q"description", q"isMandatory")
            (commonParams ++ optParams).map(p => Term.Arg.Named(p, p))
          }
          val valueTpe = if (argValueCtor.syntax.contains("Single")) t"Option" else t"Seq"
          val stats = List(
            q"override def value: $valueTpe[T] = _value",
            q"override def default: $valueTpe[T] = _default"
          )

          q"""
            if (isMandatory && _default.nonEmpty) {
              new $argCtor[T](..$params)
                with $argValueCtor[T] with Mandatory with WithDefault{..$stats}
            } else if(isMandatory && _default.isEmpty) {
              new $argCtor[T](..$params)
                with $argValueCtor[T] with Mandatory{..$stats}
            } else if(!isMandatory && _default.nonEmpty) {
              new $argCtor[T](..$params)
                with $argValueCtor[T] with WithDefault{..$stats}
            }else {
              new $argCtor[T](..$params) with $argValueCtor[T]{..$stats}
            }"""
        }

        q"private def $name[T](..$params) = {${buildStat(argCtor, argValueCtor)}}"
      case _ =>
        abort("@ArgCreationMacro can only be used on arguments creation def.")
    }
  }
}
