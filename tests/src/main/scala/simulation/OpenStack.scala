package simulation

import Scmd._
import com.github.cuzfrog.scmd.{OptionArg, SingleValue}

private object OpenStack {
  @ScmdDef
  private class OpenStackDef(args: Seq[String]) extends ScmdDefStub{
    val nova = cmdDef(description = "nova command entry")
    val neutron = cmdDef(description = "neutron command entry")
    val cinder = cmdDef(description = "cinder command entry")

    val list = cmdDef(description = "print service/project list")
    val service1111 = cmdDef(description = "chose service to print")
    val project = cmdDef(description = "chose project to print")

    val verbose = optDef[Boolean](abbr = "V", description = "print verbose info.")
    val inTable = optDef[Boolean](description = "print in table.").withDefault(true)

    import scmdTreeDefDSL._

    argTreeDef(
      verbose,
      inTable,
      version, //built-in
      nova(
        list(service1111 ?, project), //cmds under list are optional.
        //equivalent to list(service ?, project ?)
      ),
      neutron(
        list(service1111, project) ?,
        //cmd list is optional. but once list is entered, one of its sub-cmds is required.
      ),
      cinder(
        list(service1111, project), //every level of cmds is required.
      )
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = (new OpenStackDef(args))
    println("-----------App info------------")
    println(conf.appInfoString)
    println("-----------Arg tree------------")
    println(conf.argTreeString)

    val parsed: OpenStackDef = conf.parsed
    println("---------Parsed node sequence:----------")
    println(conf.parsedSeqString)
    println("---------Parsed values:----------")
    import scmdValueConverter._
    println("Is Verbose:" + parsed.verbose.value)
    println("---------Usage:----------")
    println(conf.usageString)
  }
}
