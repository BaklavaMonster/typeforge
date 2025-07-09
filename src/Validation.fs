module TypeForge.Validation

open TypeForge.Ast

type KindEnv = Environment<Kind>

let rec kindCheck (env: KindEnv) (expr: TypeExpr) : Result<Kind, string> =
    match expr with
    | TypeExpr.Primitive _ -> Ok Star
    | TypeExpr.Var { Name = name } -> 
        match KindEnv.get env name with 
        | Ok kind -> Ok kind
        | Error _ -> Kind.Arrow (Star,Star) |> Ok

    | TypeExpr.Lookup { Name = name } -> 
        match KindEnv.get env name with 
        | Ok kind -> Ok kind
        | Error _ -> Kind.Arrow (Star,Star) |> Ok
        
    | TypeExpr.List te -> kindCheck env te
    | TypeExpr.Set te -> kindCheck env te

    | TypeExpr.Lambda(tp, body) ->
        (tp.Name, tp.Kind)
        |> KindEnv.put env
        |> Result.bind (fun mEnv -> kindCheck mEnv body)
        |> Result.map (fun bodyKind -> Kind.Arrow(tp.Kind, bodyKind))

    | TypeExpr.Apply(fn, arg) ->
        match kindCheck env fn, kindCheck env arg with
        | Ok(Kind.Arrow(paramKind, bodyKind)), Ok argKind when paramKind = argKind -> Ok bodyKind
        | Ok fnKind, Ok argKind (* when paramKind <> argKind *) -> Error "Lambda param cannot consume argument"
        | Error e1, Error e2 -> Error(e1 + e2)
        | Error e, _
        | _, Error e -> Error e

    | TypeExpr.Flatten(te1, te2) ->
        match kindCheck env te1, kindCheck env te2 with
        | Ok Star, Ok Star -> Ok Star
        | Ok _, Ok _ -> Error "Flatten expects both its operands to be *"
        | Error e1, Error e2 -> Error(e1 + e2)
        | Error e, _
        | _, Error e -> Error e

    | TypeExpr.Arrow(l, r) -> 
        match kindCheck env l, kindCheck env r with
        | Ok Star, Ok Star -> Ok Star
        | Ok _, Ok _ -> Error "Arrow expects both its input and output to be *"
        | Error e1, Error e2 -> Error(e1 + e2)
        | Error e, _
        | _, Error e -> Error e

    | TypeExpr.KeyOf te ->
        match kindCheck env te with
        | Ok Star -> Ok Star
        | Ok _ -> Error "KeyOf expects both its operands to be *"
        | Error e -> Error e

    | TypeExpr.Map(k, v) ->
        match kindCheck env k, kindCheck env v with
        | Ok Star, Ok Star -> Ok Star
        | Ok _, Ok _ -> Error "Map expects both its key and value to be of type *"
        | Error e1, Error e2 -> Error(e1 + e2)
        | Error e, _
        | _, Error e -> Error e

    | TypeExpr.Sum list ->
        list
        |> Seq.filter (kindCheck env >> (<>) (Ok Star))
        |> Seq.length
        |> fun len ->
            if len = 0 then
                (Ok Star)
            else
                sprintf "%i elements of Sum have a different type than *, ie are unresolved" len |> Error

    | TypeExpr.Tuple list ->
        list
        |> Seq.filter (kindCheck env >> (<>) (Ok Star))
        |> Seq.length
        |> fun len ->
            if len = 0 then
                (Ok Star)
            else
                sprintf "%i elements of Tuple have a different type than *, ie are unresolved" len |> Error

    | TypeExpr.Rotate te ->
        match kindCheck env te with
        | Ok Star -> Ok Star
        | Ok _ -> Error "Rotate expect its operand (a tuple) to collapse to *"
        | Error e -> Error e

    | TypeExpr.Record map ->
        map.Values
        |> seq
        |> Seq.filter (kindCheck env >> (<>) (Ok Star))
        |> Seq.length
        |> fun len ->
            if len = 0 then
                (Ok Star)
            else
                sprintf "%i elements of Record have a different type than *" len |> Error

    | TypeExpr.Union map ->
        map.Values
        |> seq
        |> Seq.filter (kindCheck env >> (<>) (Ok Star))
        |> Seq.length
        |> fun len ->
            if len = 0 then
                (Ok Star)
            else
                sprintf "%i elements of Union have a different type than *" len |> Error

    | TypeExpr.Exclude _ -> Error "Unimplemented"

