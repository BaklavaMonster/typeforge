# TypeForge

The repo contains my take on the challenge that was put to me in the context of the BLP interview. The instructions are minimal and the types served as the statement of intent. I made some minor changes to them, and did my best to not corrupt the spirit of the exercise. Ultimately, this is a snapshot of what I'm currently thinking is the best way to balance function and spirit. 

## Type changes

### PrimitiveType

* Added `PrimitiveType.StringLiteral of string`. This covers the results of `KeyOf`.

### TypeExpr

* `TypeExpr.Flatten` now only consumes a tuple of `TypeExpr`. I did not understand why `TypeBindings` were required, not what purpose they serve.
* `TypeExpr.Exclude` was removed altogether. I could not match a definition to it that I found satisfying.

I'm happy to discuss these items further and even implement them once I understand the function they serve

### TypeValue
The broader question is to what extent can `TypeValue` be unresolved ? The definition given certainly implies it, but then I cannot see a usecase where having this could be useful in anyway. Due to this I made the following rationalizations:

* `TypeValue.Lookup` has been discarded, as I don't see why we cannot evaluate a reference to another `TypeExpr`.
* `TypeValue.Var` has also been discarded, as they can IMHO only be defined within an unresolved `TypeValue.Lambda`
* `TypeValue.Lambda` bas been **conserved**. The rational behind this is that they embed a TypeExpr, and as such we can possibly delay their evaluation. The other case we would encounter lambdas is within `Apply`, and there we can bind the variable to a value, so they get effectively evaluated.

## Schema
I confess that I know next to nothing in SQL apart from SELECT and TOP. As such to make better use of my time, we build a JsonSchema which I can use a bit better. The only contentious part here is the choice to mark `TypeValue.Arrow` as simple objects... Maybe we should fail here as json cannot (at least trivially and safely) represent inhabited functions.



# Original Instructions
## Introduction

This document describes a case for the position of (senior) software developer in the field of DSLs and meta-programming. The test is meant for you to show off your foundational knowledge and ability to solve complex problems in the field of programming language design and meta-programming. What the test is not meant to show off is knowledge of the minutiae of technologies, libraries, or frameworks that can be Googled/StackOverflowed/ChatGPT'ed/MySpace'ed/...

Write code, show us how you think, have some fun, and it will all be great!


### Tasks

Create a new F# project in an editor of your choice.

Given the following definition of the type system:

```f#
  type TypeParameter = { Name: string; Kind: Kind }

  and Kind =
    | Star
    | Arrow of Kind * Kind

  and TypeIdentifier = { Name: string }

  and TypeVar =
    { Name: string }

  and TypeExpr =
    | Primitive of PrimitiveType
    | Var of TypeVar
    | Lookup of TypeIdentifier
    | Apply of TypeExpr * TypeExpr
    | Lambda of TypeParameter * TypeExpr
    | Arrow of TypeExpr * TypeExpr
    | Record of Map<string, TypeExpr>
    | Tuple of List<TypeExpr>
    | Union of Map<string, TypeExpr>
    | List of TypeExpr
    | Set of TypeExpr
    | Map of TypeExpr * TypeExpr
    | KeyOf of TypeExpr
    | Sum of List<TypeExpr>
    | Flatten of FlattenArgs
    | Exclude of TypeExpr * TypeExpr
    | Rotate of TypeExpr

  and FlattenArgs =
    { Left: TypeBinding
      Right: TypeBinding }

  and TypeBinding =
    { Identifier: TypeIdentifier
      Type: TypeExpr }

  and TypeValue =
    | Primitive of PrimitiveType
    | Var of TypeVar
    | Lookup of TypeIdentifier
    | Lambda of TypeParameter * TypeExpr
    | Arrow of TypeValue * TypeValue
    | Record of Map<string, TypeValue>
    | Tuple of List<TypeValue>
    | Union of Map<string, TypeValue>
    | Sum of List<TypeValue>
    | List of TypeValue
    | Set of TypeValue
    | Map of TypeValue * TypeValue

  and PrimitiveType =
    | Unit
    | Guid
    | Int
    | Decimal
    | Bool
    | String
```

- adjust it as you feel is appropriate
- define `typeEval` as a function that evaluates type expressions into type values
- define `kindCheck` as a function that checks that a type expression has valid kinds
- define a `toSQLSchema` that, given a `TypeValue`, maps it to a (Postgres) SQL type definition
- write the necessary unit tests (note: might also be a series of functions in `main`, usage of any testing framework is not required)
