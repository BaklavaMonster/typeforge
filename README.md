# Introduction

This document describes a case for the position of (senior) software developer in the field of DSLs and meta-programming. The test is meant for you to show off your foundational knowledge and ability to solve complex problems in the field of programming language design and meta-programming. What the test is not meant to show off is knowledge of the minutiae of technologies, libraries, or frameworks that can be Googled/StackOverflowed/ChatGPT'ed/MySpace'ed/...

Write code, show us how you think, have some fun, and it will all be great!


## Tasks

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
