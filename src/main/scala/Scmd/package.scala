import com.github.cuzfrog.scmd.macros.MacroUtil
import com.github.cuzfrog.scmd.{ScmdRouteDSL, ScmdTreeDefDSL, ScmdValidationApi, ScmdValueConverter, ScmdValueImplicitConversion}

import scala.annotation.StaticAnnotation
import scala.meta._

/** Helper for client importing. */
package object Scmd extends ScmdValidationApi {

  final val scmdTreeDefDSL: ScmdTreeDefDSL.type = ScmdTreeDefDSL
  final val scmdRouteDSL: ScmdRouteDSL.type = ScmdRouteDSL
  final val scmdValueImplicitConversion: ScmdValueImplicitConversion.type = ScmdValueImplicitConversion
  final val scmdValueConverter: ScmdValueConverter.type = ScmdValueConverter
  //final val scmdSafeValueConverter: ScmdSafeValueConverter.type = ScmdSafeValueConverter

  // --------------------- Macro Annotations ----------------------
  final class ScmdDef extends StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
      MacroUtil('Def, defn)
    }
  }
  final class ScmdValid extends StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
      MacroUtil('Valid, defn)
    }
  }

  type ArgRoute = com.github.cuzfrog.scmd.ArgRoute
}

