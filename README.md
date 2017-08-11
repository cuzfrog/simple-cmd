# Scmd - The simple cmd-line arguments parser.

This project is under development now.

## Nomenclature
* Argument _arg_ - general phrase of all below.
* Command _cmd_ - a predefined phrase that must be fully matched on the command-line
 to tell the app what to do.
* Parameter _param_ - argument without a name on the command-line, which directly matches the value.
 This is equivalent to "argument" in many other libraries.
* Option _opt_ - (optional) argument that has a name with hyphens precede: `-f`, `--source-file`
* Properties _prop(s)_ - an argument with a flag as its name,
 and values of key/value pairs: `-Dkey=value`
* PriorArg _prior_ - an argument with alias that has priority to be matched. `-help`, `--help`

## Motivation:
If you ask google "scala command line arguments parser github", google gives you a whole page of
answers. Some libraries said: "Life is too short to parse command-line arguments". I think it might 
be "Life is too short to swing from one command-line argument parser to another". I tried many of 
these parsers, some are complicated with hard-to-read README,
 some kind of lack features that fit into some cases, some do not generate usage info.
It turns out that parsing command-line arguments is not an easy work.
 Here's some common features:
 
| Fetures                                                              | example | 
|----------------------------------------------------------------------|---------|
| Sub commands and argument hierarchy                 | `openstack nova list service` | 
| Boolean option folding                                                |  `-xyz`       |
| Option Value folding                                                 |`-fValue`       |
| Option Value evaluation                                            |`-f=Value`       |
| Trailing option                                |`cp -r SRC DST` equivalent to `cp SRC DST -r` |
| Mutual exclusion                                        | `--start ❘ --stop`       |
| Mutual dependency                                   | `[-a -b]` or `[-a [-b]]`       |
| Validation                                     | `cp SRC DST` SRC must exist  |
| Typed argument                                     | `cp SRC DST` SRC must be file  |
| Argument optionality                                     | `SRC [DST]`       |
| Variable argument                                        | `SRC... DST`       |
| Properties                                                 | `-Dkey=value`       | 
| Contextual help                                            | preciser help info       | 
| Routing             | no manually writing `if` ..  `else` or `match case` to route command      | 

 There are so many tricky things to be done to bring these features into reality. And Scmd did so.

### Goals I'm trying to achieve:

* **Simplicity** - Clients would be able to use it with little effort.
* **Versatility** - It has to be powerful enough to tackle those tricky things.
* **Beauty** - It should provide fluent coding style;
 It should able to generate formatted usage console info.
* **Strictness** - As a library, it should be well structured, documented and self-encapsulated.

![openstack-help](/usage-pic/openstack-help.png) ![cp-help](/usage-pic/cp-help.png) 

## Minimal example:
First, define the arguments in def-class:
```scala
import java.io.File
import Scmd._
@ScmdDef
private class CpDef(args: Seq[String]) extends ScmdDefStub{ //app name 'cp' is inferred from CpDef
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
## Document:

* [Project setup](#project-setup)
* [Define arguments](#define-arguments)
* [Build up argument structure](#built-up-arguments-structure)
* [Validation](#validation)
* [Use parsed values](#use-parsed-values)
* [Routing](#routing)
* [Misc](#misc)

### Project setup:

Scmd depends on [scalameta](http://scalameta.org/) at compile time.
```scala
val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in(Compile, console) ~= (_ filterNot (_ contains "paradise")),
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0" % Provided
)
val yourProject = project
  .settings(macroAnnotationSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.cuzfrog" %% "scmd" % "version"
    )
  )
```
For simplicity, Scmd is not divided into multiple projects. 
If one seriously demands smaller runtime jar size, please fire an issue or manually exclude
package `com.github.cuzfrog.scmd.macros`.

### Define arguments:

1. name is from val's name.
```scala
val nova = cmdDef(description = "nova command entry") //the command's name is nova
val Nova = cmdDef() //another cmd  (cmd name is case-sensitive)
val remotePort = optDef[Int]() //matches `--remote-port` and `--remotePort`
//val RemotePort = optDef[Int]()  //won't compile, name conflicts are checked at compile time.
```
The `description` cannot be omitted.

2. param/opt/props are typed:
```scala
val port = optDef[Int](abbr = "p", description = "manually specify tcp port to use") //type is Int
```
See [Supported types](src/main/scala/com/github/cuzfrog/scmd/runtime/ArgTypeEvidence.scala).
Put custom evidence into def-class to support more types.

3. mandatory argument:
```scala
val srcFile = paramDef[Path](description = "file to send.").mandatory
```

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

### Built up arguments structure.

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
  nova(
    list(service ?, project), //cmds under list are optional.
    //equivalent to list(service ?, project ?)
  ),
  neutron(
    alive | dead, // --alive | --dead, mutual exclusion
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
          +-alive
          +-dead
          +-list
              +-service
              +-project
    +-cinder
          +-list
              +-service
              +-project
```
Notice, `service`, `project` and `list` are reused in the DSL. They have only one definition each.

### Validation.
This refers to argument basic(low-level) validation. 
Mutual limitation is defined above, and its validation is implicit.

```scala
@ScmdValid
class CatValidation(argDef: CatDef) {
  validation(argDef.files) { files =>
    files.foreach { f =>
      if (!f.toFile.exists()) throw new IllegalArgumentException(s"$f not exists.")
    }
  }
}
val conf = (new CatDef(args)).withValidation(new CatValidation(_)).parsed
```
Arguments will be checked by validation statements when they are evaluated(parsed from cmd-line args).

### Use parsed values.

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

### Routing.

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
(new ArgDef(args)).runWithRoute(buildRoute)
```
`~` links routes together that if the first route's conditions are not met,
 then the second route is tried, until the statements inside `run` are called,
 the route continues to try through. Once a `run` is done, the whole route ends.
  If one does not want the whole route to end
 after one of the `run` finishes, use `runThrough` instead.
 
### Misc.

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

2. limitations:

* Reusing arg-def in tree-building-DSL: arg cannot duplicate through lineage.
Duplication through lineage makes it possibly ambiguous for an argument's scope.
This makes features, like _trailing option_, very hard to implement.

## About

### Thanks:
This project is inspired by [mow.cli](https://github.com/jawher/mow.cli). 
Ansi formatting is modified from [backuity/clist](https://github.com/backuity/clist).

### Developer:
See: [Internal explanation](INTERNAL_EXPLANATION.md).

Contribution is welcomed.

### Author:
Cause Chung (cuzfrog@139.com)/(cuzfrog@gmail.com)
