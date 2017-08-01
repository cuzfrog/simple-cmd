package simulation

import Scmd._

private object OpenStack {
  private class ArgDef{
    val nova = cmdDef(description = "nova command entry")
    val neutron = cmdDef(description = "neutron command entry")
    val cinder = cmdDef(description = "cinder command entry")

    val list = cmdDef(description = "print service/project list")
    val service = cmdDef(description = "chose service to print")
    val project = cmdDef(description = "chose project to print")
  }
}
