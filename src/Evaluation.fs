module TypeForge.Evaluation

open TypeForge.Ast
open FsToolkit.ErrorHandling

type Binding =
    | Instance of TypeExpr
    | Declared

type EvalEnv = Environment<TypeExpr>

let rec typeEval (env: EvalEnv) (expr: TypeExpr) : Result<TypeValue, string> =
    match expr with
    | TypeExpr.Primitive pt -> TypeValue.Primitive pt |> Ok

    | TypeExpr.Var v ->
        // Var references a type variable.
        // if it is resolved (ie has been pushed to the env before) we continue 
        // the evaluation. If it's not the case we just return it as Var.
        match EvalEnv.get env v.Name with 
        | Ok te ->  typeEval env te // we resolved the type variable so we continue
        //| Ok Declared -> Var v |> Ok // the variable has been declared but not instantiated. This happens when the lambda is not the argument of an application
        | Error e -> "Variable is not previously declared nor instantiated" + e |> Error // this is the case 

    | TypeExpr.Lookup lkp ->
        match EvalEnv.get env lkp.Name with
        | Ok te ->  typeEval env te 
        | Error e -> sprintf "we cannot propagate unresolved lookups -> %s" e |> Error

    | TypeExpr.Lambda (param, expr) -> // as in "lambda abstraction"
        // if we found ourselves in this case it means that this lambda lives
        // outside of an Apply (see below). This is alright, as we can just 
        // set the type parameter as declared

        TypeValue.Lambda (param, expr) |> Ok
  

    | TypeExpr.Apply (fn, arg) -> // as in "lambda application"
        // Apply and Lambda need to be handled together as we are binding the lambda's
        // parameter to the Apply's argument. As such you cannot throw and catch 
        // efficiently
        match fn with
        | TypeExpr.Lambda(tp, te) -> 
            match EvalEnv.put env (tp.Name, arg) with
            | Ok mEnv -> typeEval mEnv te
            | Error e -> failwith e

        | _ -> Error "Apply TypeExpr expects a lambda TypeExpr as its first arg"

    // Function type
    | TypeExpr.Arrow (t1, t2) ->
        Result.map2 (fun x y -> x, y) (typeEval env t1) (typeEval env t2)
        |> Result.map Arrow

    // Product types
    | TypeExpr.Tuple tpl ->
        tpl 
        |> List.map (typeEval env) 
        |> List.sequenceResultA
        |> Result.map Tuple
        |> Result.mapError (List.reduce (+))


    | TypeExpr.Record m ->
        let moveKeyInRslt =
            function
            | s, Ok te -> Ok(s, te)
            | _, Error(e) -> Error(e)

        m
        |> Map.toList
        |> List.map (fun (k, v) -> (k, typeEval env v))
        |> List.map moveKeyInRslt
        |> List.sequenceResultA
        |> Result.map (Map.ofList >> Record)
        |> Result.mapError (List.reduce (+))


    | TypeExpr.Map(k, v) -> 
        Result.map2 (fun x y -> (x,y)) (typeEval env k) (typeEval env v)
        |> Result.map Map

    // CoProduct
    | TypeExpr.Sum list ->
        list
        |> List.map (typeEval env)
        |> List.sequenceResultA
        |> Result.mapError (List.reduce (+))
        |> Result.map Sum

    | TypeExpr.Union map -> 
        let moveTagInRslt =
            function
            | s, Ok te -> Ok(s, te)
            | _, Error e -> Error e

        map
        |> Map.toList
        |> List.map (fun (k, v) -> (k, typeEval env v))
        |> List.map moveTagInRslt
        |> List.sequenceResultA
        |> Result.map (Map.ofList >> Record)
        |> Result.mapError (List.reduce (+))


    | TypeExpr.List te -> 
        typeEval env te 
        |> Result.map List

    | TypeExpr.Set te -> 
        typeEval env te |> 
        Result.map Set

    | TypeExpr.Flatten (te1, te2) ->
        let moveKeyInRslt =
            function
            | s, Ok te -> Ok(s, te)
            | _, Error e -> Error e

        let resolveToRecord =
            function
            | TypeExpr.Record m -> m
            | TypeExpr.Lookup { Name = name } ->
                match EvalEnv.get env name with
                | Ok  (TypeExpr.Record m) -> m
                | Ok other  -> failwithf "Found other type: %s" (other.GetType().FullName)
                | Error e -> failwith e
            | _ -> failwith "Flatten case is neither a Record nor a lookup yielding a Record"

        let te1 = te1 |> resolveToRecord
        let te2 = te2 |> resolveToRecord 
        let mergedMap = Map.fold (fun acc key value -> Map.add key value acc) te1 te2

        mergedMap
        |> Map.toList
        |> List.map (fun (k, v) -> k, typeEval env v)
        |> List.map moveKeyInRslt
        |> List.sequenceResultA
        |> Result.map (Map.ofList >> Record)
        |> Result.mapError (List.reduce (+))


    | TypeExpr.Rotate te ->
        // We assume that only Tuple can be rotated as it is the only one that embeds order
        match te with
        | TypeExpr.Tuple tpl ->
            match tpl with
            | [] -> List.empty
            | [ v ] -> v |> typeEval env |> List.singleton
            | head :: tail ->
                tail
                |> Array.ofList
                |> (fun right -> Array.append right [| head |])
                |> List.ofArray
                |> List.map (typeEval env)
            |> List.sequenceResultA
            |> Result.map Tuple
            |> Result.mapError (List.reduce (+))

        | other -> sprintf "Expected Rotate Tuple Record, but found Rotate %s" (other.GetType().FullName) |> Error

    | TypeExpr.KeyOf te ->
        match te with
        | TypeExpr.Record map ->
            map
            |> Map.toList
            |> List.map (fun (k,v) -> TypeExpr.Primitive (StringLiteral k))
            |> TypeExpr.Sum
            |> typeEval env

        | other -> sprintf "Expected KeyOf Record, but found KeyOf %s" (other.GetType().FullName) |> Error
