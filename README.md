# Scmd
The simple cmd-line arguments parser.

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
 There are so many tricky things residing in the common features below:
 
| Fetures                                                              | example | 
|----------------------------------------------------------------------|---------|
| Sub commands and argument hierarchy                                   |        | 
| Boolean option folding                                                |  `-xyz`       |
| Option Value folding                                                 |`-fValue`       |
| Option Value evaluation                                            |`-f=Value`       |
| Mutually exclusion                                        | `--start ❘ --stop`       |
| Mutually dependency                                   | `[-a -b]` or `[-a [-b]]`       |
| Validation                                     | `cp SRC DST` SRC must be file/exist  |
| Argument optionality                                     | `SRC [DST]`       |
| Variable Argument                                      | `SRC... DST`       |
| Properties/flag                                            | `-Dkey=value`       | 
| Contextual help                                            | preciser help info       | 

### Goals I'm trying to achieve:

* **Simplicity** - Clients would be able to use it with little effort.
* **Versatility** - It has to be powerful enough to tackle those tricky things.
* **Beauty** - It should provide fluent coding style;
 It should able to generate formatted usage console info.
* **Strictness** - As a library, it should be well structured, documented and self-encapsulated.

### Minimal example:
First define the arguments:
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