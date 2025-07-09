module TypeForge.Ast

type TypeParameter = { Name: string; Kind: Kind }

and Kind =
    | Star
    | Arrow of Kind * Kind

and TypeIdentifier = { Name: string }

and TypeVar = { Name: string }

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
    | Flatten of TypeExpr * TypeExpr // See README.md
    // | Exclude of TypeExpr * TypeExpr,  See README.md
    | Rotate of TypeExpr

and FlattenArgs =
    { Left: TypeBinding
      Right: TypeBinding }

and TypeBinding =
    { Identifier: TypeIdentifier
      Type: TypeExpr }

and TypeValue =
    | Primitive of PrimitiveType
    | Lambda of TypeParameter * TypeExpr
    | Arrow of TypeValue * TypeValue
    | Record of Map<string, TypeValue>
    | Tuple of List<TypeValue>
    | Union of Map<string, TypeValue>
    | Sum of List<TypeValue>
    | List of TypeValue
    | Set of TypeValue
    | Map of TypeValue * TypeValue
    // See README.md
    // | Var of TypeVar
    // | Lookup of TypeIdentifier

and PrimitiveType =
    | Unit
    | Guid
    | Int
    | Decimal
    | Bool
    | String
    | StringLiteral of string // // See README.md

type Environment<'a> = {
    map : Map<string,'a>
}
with 
    static member get (env:Environment<'a>) k = 
        env.map.TryFind k
        |> FSharpPlus.Option.toResultWith (sprintf "Could not find key %s in environment" k)

    static member put (env:Environment<'a>) kvp =
        match env.map.ContainsKey (fst kvp) with
        | false -> 
            let newMap = env.map.Add kvp 
            {env with map = newMap} |> Ok 
        | true -> sprintf "Key %s is already defined in environment" (fst kvp) |> Error
