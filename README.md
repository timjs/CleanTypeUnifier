# CleanTypeUnifier

A type unifier for Clean, in Clean.

Copyright &copy; 2016 Camil Staps. Licensed under MIT (see the LICENSE file).

This library hooks in to the [clean-compiler][cocl], so you will have to
download and build it (preferably the `itask` branch).

## Usage

Typically you will need only a few types and functions:

 * `Type` - everything in Clean has a type, this is a formalisation of that concept.
 * `TypeVar` - types can have variables, which are expressed as strings.
 * `unify :: a a -> Maybe ([TVAssignment], [TVAssignment])` - attempts to unify its two arguments (usually you will use `Type`s). The result is `Nothing` if the two cannot be unified, and two lists of assignments of `TypeVar`s to `Type`s. The first list holds the assignmens of variables in the first argument, the second list of variables in the second argument. A `TVAssignment` is a `(TypeVar, Type)`.
 * `print :: a -> [String]` is defined for most of the types in the library; it is a pretty printer that returns a list of strings (for efficiency reasons) that can be `concat`enated.

In addition to this, there is an instance of `toType` for the `SymbolType` from the `syntax` module from the [clean-compiler][cocl], which can be used if you need the types of functions from existing Clean modules. You will need to call `wantModule` from `frontend/parse`, then extract the `SymbolType`s from the `ParsedModule`.

[cocl]: https://svn.cs.ru.nl/repos/clean-compiler/
