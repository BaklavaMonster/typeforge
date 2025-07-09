module TypeForge.Schema

open FSharp.Data
open TypeForge.Ast

let serialize (schema: JsonValue) : string = schema.ToString()

let rec toJsonSchema (typeVal: TypeValue) : JsonValue =
    match typeVal with
    // | Var _ 
    // | Lookup _
    | Lambda _ -> failwith "We cannot serialize types that are not fully resolved, therefor we cannot provide a schema for them, sorry"

    | Primitive p ->
        match p with
        | Unit -> JsonValue.Record [| "type", JsonValue.String "null" |]
        | Guid -> JsonValue.Record [| "type", JsonValue.String "string"; "format", JsonValue.String "uuid" |]
        | Int -> JsonValue.Record [| "type", JsonValue.String "integer" |]
        | Decimal -> JsonValue.Record [| "type", JsonValue.String "number" |]
        | Bool -> JsonValue.Record [| "type", JsonValue.String "boolean" |]
        | String -> JsonValue.Record [| "type", JsonValue.String "string" |]
        | StringLiteral value ->
            JsonValue.Record [| "type", JsonValue.String "string"; "const", JsonValue.String value |]
            
    | Arrow(_, _) -> 
        // Arrow represent function, these are not serializable. We could fail here, but choose not to
        JsonValue.Record [| "type", JsonValue.String "object" |] // 
    | Record fields ->
        let properties =
            fields
            |> Map.toList
            |> List.map (fun (name, tv) -> name, toJsonSchema tv)
            |> Array.ofList

        let required =
            fields
            |> Map.toList
            |> List.map fst
            |> List.map JsonValue.String
            |> Array.ofList

        JsonValue.Record
            [| "type", JsonValue.String "object"
               "properties", JsonValue.Record properties
               "required", JsonValue.Array required |]
    | Tuple types ->
        let items =
            types |> List.mapi (fun i tv -> string i, toJsonSchema tv) |> Array.ofList

        JsonValue.Record
            [| "type", JsonValue.String "array"
               "items", JsonValue.Record items
               "minItems", JsonValue.Number(decimal types.Length)
               "maxItems", JsonValue.Number(decimal types.Length) |]

    | Union variants ->
        let variantSchemas =
            variants
            |> Map.toList
            |> List.map (fun (tag, tv) ->
                JsonValue.Record
                    [| "type", JsonValue.String "object"
                       "properties",
                       JsonValue.Record
                           [| "tag", JsonValue.Record [| "const", JsonValue.String tag |]
                              "value", toJsonSchema tv |]
                       "required", JsonValue.Array [| JsonValue.String "tag"; JsonValue.String "value" |] |])
            |> Array.ofList

        JsonValue.Record [| "oneOf", JsonValue.Array variantSchemas |]
    | Sum types ->
        let typeSchemas = types |> List.map toJsonSchema |> Array.ofList
        JsonValue.Record [| "oneOf", JsonValue.Array typeSchemas |]
    | List tv -> JsonValue.Record [| "type", JsonValue.String "array"; "items", toJsonSchema tv |]
    | Set tv ->
        JsonValue.Record
            [| "type", JsonValue.String "array"
               "items", toJsonSchema tv
               "uniqueItems", JsonValue.Boolean true |]
    | Map(key, value) ->
        JsonValue.Record
            [| "type", JsonValue.String "object"
               "additionalProperties", toJsonSchema value |]
