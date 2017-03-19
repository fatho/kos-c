# kos-c

This project contains a compiler for a C/C++/C#-like language targeting the kOS scripting language used in the Kerbal Operating System mod (https://ksp-kos.github.io/KOS/index.html) for Kerbal Space Program.

The inspiration for this project came from a certain incident where one of my drones literally crashed due to a type error after roughly 30 minutes of flight time.

Thus, in order to prevent such frustrating incidents in the future, `kos-c` was born.

# Language Features

The language exposes all features that are also available in kOS scripts (unless I forgot some), and even a little more.

## Module System

The language has a simple module system where every module is in its own file with the extension `.kc`.
Collections of modules can be grouped into libraries by simply putting them in a common base directory.
Every code file starts with a module header such as `module Foo::Bar::Baz;`, indicating its name.
The name of a module must match its file system location inside the library folder it is part of.
In the previous example, the module `Foo::Bar::Baz` must therefore be defined in a file `<library>/Foo/Bar/Baz.kc`.

Other modules can be imported via an `import` declaration in one of following variants:

  - `import Foo::Bar;` is a qualified import, i.e. any entity `X` inside the imported module can only be accessed via its fully qualified name `Foo::Bar::X`.
  - `import Foo::Bar as Baz;` is also a qualified import, but the module is locally renamed to `Baz`. An entity `X` contained in the imported module can be accessed via the fully qualified name `Baz::X`.
  - `import Foo::Bar unqualified;` imports all public entities of the imported module in the local scope.

Note that even though `import` statements can appear anywhere on the top level, they are always brought into scope before all other checks.

The language also provides two visibility levels `private` and `public` that can be used to annotate declarations, but they are not checked so far.

## Type System

The language has a static type system with support for delegates (function pointers) and simple generic types.
There are the following kinds of types:

  - Simple types, such as `Scalar` or `Boolean`,
  - generic types such as a list of scalars `List<Scalar>` or a dictionary `Lexicon<String, Scalar>` and
  - function types such as `Scalar(Boolean,Boolean)` which denotes a function returning a scalar and taking two boolean arguments.

In fact, because kOS also supports optional arguments, there is an extended function type `RetType(Arg1, Arg2, ...)[Opt1,Opt2,...]` where the types of optional arguments are denoted in square brackets.

## Function Declarations

Functions can be declared similar as they would be in C# or related languages.
The following function adds two numbers.
```
Scalar Add(Scalar x, Scalar y) {
  return x + y;
}
```

Functions can also be generic, as demonstrated in the following example.
```
A FirstOrSecond<A>(Boolean b, A fst, A snd) {
  if (b) {
    return fst;
  } else {
    return snd;
  }
}
```
Depending on the argument `b`, it returns either the first or the second argument of type `A`,
where `A` is the name of the generic type parameter.

One can also define anonymous functions inside expressions using the following syntax: `(Foo x, Bar y) -> ReturnType { ... }`.

**DISCLAIMER** The parser seems to be a bit buggy in some cases related to anonymous functions.

## Global Variables

Sometimes it can be helpful to declare global variables:

```
Scalar globalVal = 10;
```

Every declaration of a variable must also come with an initializer. By adding additional modifiers, we can make global variables read- or write-only:

```
Scalar thisIsAConstant get = 2;
Scalar thisIsWriteOnly set = 1;
```

While the concept of write-only variables is probably not that useful when they are user-defined, there are is at least one builtin variable in kOS that can only be written to (`SHIP:CONTROL:NEUTRALIZE`).

## Records

KOS-C allows the definition of user defined records which are simple accumulations of variables.

```
record Vec2 {
  Scalar X;
  Scalar Y;
}
```

Likewise to functions, records can also be generic.

```
record Pair<A, B> {
  A First;
  B Second;
}
```

Values of records can be created using a record initializer.
For example, a value of the record `Vec2` can be created with `Vec2 { X = 1, Y = 2 }`.

Every record comes with a predefined function to make a shallow copy.

```
Vec2 r1 = Vec2 { X = 1, Y = 2 };
Vec2 r2 = r1.Copy();
r2.X = 2; // r1.X stays 1
```

Because kOS does not support user-defined data types, records are translated into lists when compiled into a kOS script. Record fields are index in the order they are declared.

## Interoperability

In order to make use of things that are already built into kOS or defined in kOS scripts that are not worth porting,
the language provides a special `builtin` directive for specifying the interface of such entities.
In fact, the whole kOS standard library is imported through this `builtin` mechanism.

All builtins are declared in the `KOS::` namespace in the [kosc-prelude](kosc-prelude/) library.
Note that by default the scope is empty, so in order to even use basic types like `Void`, `Scalar`, etc., one should `include KOS::Builtin unqualified;` in every module.

### Structures

Everything in `kOS` is a structure. In order to make use of a specific structure in `kos-c`, it can be brought into scope as follows

```
builtin structure SomeStructure : SuperStructure {
  Scalar Foo;
  Scalar Bar get;
  Scalar Baz set;
}
```

These declarations look a lot like records, but differ in a few aspects.
First of all, structures can have a "super"-structure they derive from.
For example, in kOS every `Body` is also an `Orbitable`.

Furthermore, while records can only contain definitions of variables, structures can also contain 
function declarations and an indexing operator.

As an example consider how the kOS list type could be imported as a builtin structure.

```
builtin structure List<A> : Enumerable<A> {
  A [Scalar index];
  Void Add(A item);?
  ...
}
```

The first line of the structure body is the declaration of the indexing operator.
It allows using standard array indexing syntax on structures that define it.
When we have a list `lst`, we can access the fifth element like so: `lst[4]`.

## Statements

TODO...

## Expressions

TODO....

# Examples

Even though the language documentation is still incomplete, the prelude library can give some insight into how the language can be used.
For how the builtin kOS features are included, see [kosc-prelude/KOS](kosc-prelude/KOS).
For an example of how to launch a rocket into LKO, see [kosc-prelude/Rocket](kosc-prelude/Rocket) and [Examples/Main.kc](Examples/Main.kc).

# Roadmap

- implement visibility system
- better error messages (with source location)
- automatically include bare minimum builtins?
- also rename local variables
- provide option for disabling renaming

# Setup

The compiler is written in Haskell, but since the project is compatible with [Stack](https://docs.haskellstack.org/en/stable/README/), it should be fairly easy to compile.
Assuming that stack has been installed according to the linked documentation, all that's necessary is to run the following commands in the root of the repository:

- `stack setup` in order to install the correct version of the Haskell compiler GHC. This only needs to be done once. GHC is installed into stack's local directory so it will not affect any other system wide installs.
- `stack install` installs all dependencies, compiles the KOS-C compiler and copies the `koscc` executable into the user's binary directory. On linux, that is `~/.local/bin`, on Windows `%APPDATA%/local/bin`.

If an automatic installation into the `bin` directory is not desired, the last step can be replaced by `stack build`.
The KOS-C compiler can then be executed using `stack exec koscc -- <args>` from inside the repository's root folder. Not that the double dash `--` is necessary to separate the compiler arguments from stack's arguments.

# Usage

The module system is completely handled by KOS-C, therefore the compiler always produces just a single file.
It should be invoked like `koscc -Lpath/to/kos-prelude -Lpath/to/other/library -o Output.ks Main.kc` where `Main.kc` is the KOS-C file containing the entry point of the program.
The compiler then transitively resolves all imports and generates a single file (in this example `Output.ks`) containing required declarations from all imported modules.
The main KOS-C file is required to contain a function `Void Main()`. This serves as the root for the code generation process. In the generated script file, the main function will be executed as soon as the script is run.

The code generator only outputs declarations that are actually reachable from the entry point. Therefore, one can import big libraries without risking to hit the storage limit of the kOS processors.
As an additional measure, all global declarations are renamed to shorter identifiers.

If renaming is not desired, one can provide a string literal in function and variable declarations that will be used instead of generating a fresh name, as in the following exammple.

```
Void CustomName() "SomeName" {
}

Scalar CustomVar "SomeVar" = 2;
```
