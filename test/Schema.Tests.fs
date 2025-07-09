module TypeForge.Tests.Schema

open NUnit.Framework
open TypeForge.Ast
open TypeForge.Schema

open Json.Schema
open System.Text.Json.Nodes

[<Test>]
let ``Prints a schema, uses 3rd party library to check`` () =
    let typeValue = 
        Record (
            [
                "a tuple", Tuple [Primitive Int; Primitive (StringLiteral "myStringLiteral")]
                "a list of discriminated unions", List (Union (["myPrimitive", Primitive Int; "myOTherPrimitive", Primitive (StringLiteral "myStringLiteral")]|> Map.ofList))
                "just a primitive", Primitive Int
                "an arrow", Arrow (Primitive Int, Tuple [Primitive Int; Primitive (StringLiteral "Yet another string literal")])  
            ]
            |> Map.ofList
        )
    
    let schema = toJsonSchema typeValue

    let serialized = schema.ToString 2

    printf "%s" serialized

    // throws if schema is invalid
    Assert.DoesNotThrow( fun () -> JsonSchema.FromText(serialized) |> ignore)