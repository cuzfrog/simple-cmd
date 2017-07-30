import com.github.cuzfrog.scmd.macros.MacroUtil
import com.github.cuzfrog.scmd.{ScmdApi, ScmdRouteDSL, ScmdTreeDefDSL, ScmdValueConverter, ScmdValueImplicitConversion}

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * ScmdApi stub to import scmd.
  */
object scmd extends ScmdApi {

  final val scmdTreeDefDSL: ScmdTreeDefDSL.type = ScmdTreeDefDSL
  final val scmdRouteDSL: ScmdRouteDSL.type = ScmdRouteDSL
  final val scmdValueImplicitConversion: ScmdValueImplicitConversion.type = ScmdValueImplicitConversion
  final val scmdValueConverter: ScmdValueConverter.type = ScmdValueConverter

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
}
