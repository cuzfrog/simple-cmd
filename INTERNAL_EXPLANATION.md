# Scmd internal explanation.

Scmd heavily utilizes 4 tools:

* macros/scalameta - to reduce boilerplate and enhance api.
* recursive calls - frequently used for arguments tree, route tree and backtracking 
* trait mixin - to provide a type-enhance api
* typeclass - to extend api, to simplify and decouple internal structures.

### Package structure:
```text
+-scmd               //shared or public classes
    +-internal       //internal macros and utils
    +-macros         //macros for argument definition
    +-runtime        //parsing algorithm, runtime utils
```

### Work flow:
1. extract argument definition information from def-class.

2. add `ScmdRuntime` and other necessary methods to def-class.

`ScmdRuntime` is stateful OOP-class, while encapsulated by def-class.
 It provides all apis to interact with arg-parsing.

3. (optional) add validation to `ScmdRuntime` by changing its states.

4. when `parse` or `runWithRoute` is called, command-line args are then parsed by `ScmdRuntime`.
A newly created def-class with evaluated arguments is to be returned by `parse` or consumed by
`runWithRoute`.

Mutual limitation validation is carried out just after the 
parsing(command-line args being paired with defined args). 
Low level validation(arg validation and type conversion) is carried out during the evaluation.