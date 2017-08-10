# Scmd - The simple cmd-line arguments parser.

This project is under development now.

### Nomenclature
* Argument arg - general phrase of all below.
* Command cmd - a predefine phrase that must be fully matched on the command-line
 to tell the app what to do.
* Parameter param - argument without a name on the command-line, which directly matches the value.
 This is equivalent to "argument" in many other libraries.
* Option opt - (optional) argument that has a name with hyphens precede: `-f`, `--source-file`
* Properties prop(s) - an argument with a flag as its name,
 and values of key/value pairs: `-Dkey=value`
* PriorArg prior - an argument with alias that has priority to be matched. `-help`, `--help`

### Motivation:
If you ask google "scala command line arguments parser github", google gives you a whole page of
answers. Some libraries said: "Life is too short to parse command-line arguments". I think it might 
be "Life is too short to swing from one command-line argument parser to another". I tried many of 
these parsers, some are complicated with hard-to-read README,
 some kind of lack features that fit into some cases, some do not generate usage info.
It turns out that parsing command-line arguments is not an easy work.
 There are so many tricky things, residing in the common features below:
 
| Fetures                                                              | example | 
|----------------------------------------------------------------------|---------|
| Sub commands and argument hierarchy                 | `openstack nova list service` | 
| Boolean option folding                                                |  `-xyz`       |
| Option Value folding                                                 |`-fValue`       |
| Option Value evaluation                                            |`-f=Value`       |
| Tail option                                |`cp -r SRC DST` equivalent to `cp SRC DST -r` |
| Mutually exclusion                                        | `--start ❘ --stop`       |
| Mutually dependency                                   | `[-a -b]` or `[-a [-b]]`       |
| Validation                                     | `cp SRC DST` SRC must exist  |
| Type safety                                     | `cp SRC DST` SRC must be file  |
| Argument optionality                                     | `SRC [DST]`       |
| Variable Argument                                        | `SRC... DST`       |
| Properties/value with flag                                   | `-Dkey=value`       | 
| Contextual help                                            | preciser help info       | 
| Command route             | no manually writing `if` ..  `else` or `match case` to route command      | 

### Goals I'm trying to achieve:

* **Simplicity** - Clients would be able to use it with little effort.
* **Versatility** - It has to be powerful enough to tackle those tricky things.
* **Beauty** - It should provide fluent coding style;
 It should able to generate formatted usage console info.
* **Strictness** - As a library, it should be well structured, documented and self-encapsulated.

### Minimal example:
First, define the arguments in def-class:
```scala
import java.io.File
import Scmd._
@ScmdDef
private class CpDef(args: Seq[String]) extends ScmdDefStub{
    val SRC = paramDefVariable[File]().mandatory
    val DEST = paramDef[File]().mandatory
    val recursive = optDef[Boolean](abbr = "R")
}
```
This definition matches cmd-line argument combinations below:
```text
$cp file1 file2 ... dest       //backtracking: dest always matched.
$cp file1 dest -R              
$cp -recursive file1 dest      //option can be put at anywhere.
...
```

Then use them:
```scala
object CpApp extends App{
    val conf = (new CpDef(args)).parsed
    import scmdValueConverter._
    conf.SRC.value // Seq[File]
    conf.DEST.value // File
    conf.recursive.value // Boolean
}
```
### Document:

* Define arguments
* Build up argument structure
* Use parsed values
* Routing

#### Define arguments:

1. name is val's name.
```scala
val nova = cmdDef(description = "nova command entry") //the command's name is nova
val Nova = cmdDef() //another cmd  (name is case-sensitive)
val remotePort = optDef[Int]() //matches `--remote-port` and `--remotePort`
```
the `description` cannot be omitted.

2. param/opt/props are typed:
```scala
val port = optDef[Int](abbr = "p", description = "manually specify tcp port to use") //type is Int
```
[Supported types](src/main/scala/com/github/cuzfrog/scmd/runtime/ArgTypeEvidence.scala)
Put custom evidence into def-class to support more types.

3. mandatory argument:
```scala
val srcFile = paramDef[Path](description = "file to send.").mandatory
```
`mandatory` gives `srcFile` a `Mandatory` mixin, this makes later usage more pleasant.

4. argument with default value:
```scala
val name = optDef[String]().withDefault("default name")
```
Mandatory argument cannot have default value. 
Boolean args have default value of `false`, which could be set to `true` manually.
Boolean args cannot(should not) be mandatory.

5. variable argument(repetition):
```scala
val SRC = paramDefVariable[File]().mandatory
val num = optDefVariable[Long](abbr = "N")
```
`-N=1 --num 2 --num=3 -N4 -N 5` is legal, `num` will have the value of `Seq(1,2,3,4,5)`

6. properties(argument with flag):
```scala
val properties = propDef[Int](flag = "D",description = "this is a property arg.")
```
`-Dkey1=1 -Dkey2=3` gives `properties` the value `Seq("key1"->1,"key2"->3)`
Props are global argument, that means they can be matched from anywhere on the command-line arguments.

7. prior argument:
```scala
val help = priorDef(alias = Seq("-help", "--help")) //fully matches `-help` and `--help`
```
Prior args are scoped to cmd: 
`git --help` prints the help info for `git`, `git tag --help` prints for `tag`

Prior args are picked immediately.
`cp --help SRC DST` prints the help info, `SRC` and `DST` are ignored.

#### Built up arguments structure.

Argument definition is of the shape of a tree, params/opts are scoped to their cmds.

1. Inferred from declaration.
The definition order matters in `@ScmdDef` annotated Class.
```scala
val sharedParam = paramDef[String](description = "this should be shared by cmds below.")
val topLevelOpt = optDef[Int]()
val nova = cmdDef(description = "nova command entry")
val param1 = paramDef[File]()
val opt1 = optDef[Int]()
val neutron = cmdDef(description = "neutron command entry")
val opt2 = optDef[Int]()
```
This will build the structure:
```text
openstack
    +-topLevelOpt
    +-nova
          +-sharedParam
          +-param1
          +-opt1
    +-neutron
          +-sharedParam
          +-opt2
```

2. Using tree building DSL:
```scala
import scmdTreeDefDSL._
argTreeDef( //app entry
  verbose, //opt
  version, //built-in prior, gets ignored
  nova(
    list(service ?, project), //cmds under list are optional.
    //equivalent to list(service ?, project ?)
  ),
  neutron(
    list(service, project) ?, //trailing comma is supported through macros.
    //cmd list is optional. but once list is entered, one of its sub-cmds is required.
  ),
  cinder(
    list(service, project), //every level of cmds is required.
  )
)
```
This will build the structure:
```text
openstack
    +-verbose
    +-nova
          +-list
              +-service
              +-project
    +-neutron
          +-list
              +-service
              +-project
    +-cinder
          +-list
              +-service
              +-project
```
Notice, `service`, `project` and `list` are reused in the DSL. They have only one definition each.

#### Use parsed values.

Scmd provides 3 styles of getting evaluated arguments: 

1. Implicit conversion:
```scala
import scmdValueImplicitConversion._
val src:Seq[File] = conf.SRC 
val dst:File = conf.DST //mandatory arg will be converted into value directly.
val port:Option[Int] = conf.remotePort //optional arg will be converted into an Option
```
If an arg has default value, it will fall back to default value when not specified by user.

2. Converter(Recommended):
```scala
import scmdValueConverter._
val src:Seq[File] = conf.SRC.value 
val dst:File = conf.DST.value
val port:Option[Int] = conf.remotePort.value
```

3. Safe converter:
```scala
import scmdSafeValueConverter._
val src:Seq[File] = conf.SRC.valueSeq 
val dst:File = conf.DST.value
val port:Option[Int] = conf.remotePort.valueOpt
```

#### Routing.

Manual routing: `if(conf.cmd.met){...} else {...}`

Scmd provides a DSL similar to that of 
[akka-http](http://doc.akka.io/docs/akka-http/current/scala/http/introduction.html#routing-dsl-for-http-servers)
 to route through commands:
```scala
def buildRoute(argDef: ArgDef): ArgRoute = {
    import scmdRouteDSL._
    import argDef._
    import scmdValueConverter._
    app.onConditions(
        boolOpt.expectTrue,
        properties.expectByKey("key1")(_.forall(_ > 6))
    ).run {
        println(intOpt.value)
        //do something
    } ~
    app.run {
        //fall back behavior.
    }
}
```
`~` links routes together that if the first route's conditions are not met,
 then the second route is tried, until the statements inside `run` are called,
 the route continues to try through. If one does not want the whole route to end
 after one of the `run` id done, use `runThrough` instead.
 
#### Misc.

1. built-in priors:
`help` and `version` are built-in prior arguments. When they are matched against top cmd(app itself),
usage info and version info will be printed respectively. The alias of them will be matched only,
i.e. `-help`, `--help`

define them in route against top cmd will override the default behavior.
```scala
import scmdRouteDSL._
app.runOnPrior(help){
  //different behavior
}.run{...}
```

### About

#### Contribution:
Contribution is generally welcomed.

#### Author:
Cause Chung (cuzfrog@139.com)/(cuzfrog@gmail.com)
