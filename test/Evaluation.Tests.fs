module TypeForge.Tests.Evaluation

open NUnit.Framework
open TypeForge.Ast
open TypeForge.Evaluation


let env: Environment<TypeExpr> = { map = Map.empty }

let check env t v =
    match typeEval env t with
    | Ok k when k = v -> ()
    | x -> failwithf "Found %A, expected: %A" x v

let checkFails env t =
    match typeEval env t with
    | Error _ -> ()
    | Ok other -> failwithf "Found Ok(%A), expected Error()" (Ok other)

module Primitive =
    let t = TypeExpr.Primitive Int
    let v = TypeValue.Primitive Int

    [<Test>]
    let ``primitive p maps into primitive p`` () = check env t v

module Var =
    // This line essentially simulates what the apply case would add to the eval context.
    let mEnv =
        EvalEnv.put env ("T", TypeExpr.Primitive Int)
        |> Result.defaultWith (fun _ -> failwith "")

    [<Test>]
    let ``var v maps into var v when context resolves TypeVar `` () =
        let t = TypeExpr.Var { Name = "T" }
        let v = TypeValue.Primitive Int
        check mEnv t v

    [<Test>]
    let ``var v maps into var v when context does not contain name`` () =
        let t = TypeExpr.Var { Name = "LadyGaga" }
        let v = TypeValue.Var { Name = "LadyGaga" }
        check mEnv t v

module Lookup =
    // Things start to look interesting, as we have indirection
    // We therefor need to  enrich our environment with what would be a
    // previously defined type :
    // F# equivalent:
    //     type PreviousType = int
    //     type NewType = PreviousType
    let mEnv =
        EvalEnv.put env ("ExpressionName", TypeExpr.Primitive Int)
        |> Result.defaultWith (fun _ -> failwith "")

    [<Test>]
    let ``Lookup in env yields proper TypeValue`` () =
        let t = TypeExpr.Lookup { Name = "ExpressionName" }
        let v = TypeValue.Primitive Int
        check mEnv t v

    [<Test>]
    let ``Lookup in env fails to yield proper value`` () =
        let t = TypeExpr.Lookup { Name = "WrongExpressionName" }
        checkFails env t

module Arrow =
    // Not as fun as arrow but a better place than Apply and Lambda
    // these just represent function type from TypeExpr --> TypeExpr
    // they can represent pretty much anything on both sides of the arrow.
    // As such they are pretty trivial
    [<Test>]
    let ``Arrow in TypeExpr maps to Arrow in TypeValue`` () =
        let t = TypeExpr.Arrow(TypeExpr.Primitive Bool, TypeExpr.Var { Name = "VarName" })

        let v =
            TypeValue.Arrow(TypeValue.Primitive Bool, TypeValue.Var { Name = "VarName" })

        check env t v

module Record =
    // fairly simple as well as we are just representing records
    // F#
    //   type MyRecord = {
    //     age : int
    //     isBiped : bool
    //     name : string
    //     hairColour : HairColour
    //   }
    [<Test>]
    let ``Record TypeExpr with a single indirection resolves to ValueExpr`` () =
        let mEnv =
            EvalEnv.put env ("HairColour", TypeExpr.Primitive String)
            |> Result.defaultWith (fun _ -> failwith "")

        let t =
            TypeExpr.Record(
                [ "age", TypeExpr.Primitive Int
                  "isBiped", TypeExpr.Primitive Bool
                  "name", TypeExpr.Primitive String
                  "hairColour", TypeExpr.Lookup { Name = "HairColour" } ]
                |> Map.ofList
            )

        let v =
            TypeValue.Record(
                [ "age", TypeValue.Primitive Int
                  "isBiped", TypeValue.Primitive Bool
                  "name", TypeValue.Primitive String
                  "hairColour", TypeValue.Primitive String ]
                |> Map.ofList
            )

        check mEnv t v

module Tuple =
    // Same deal as above but unnamed
    [<Test>]
    let ``Tuple TypeExpr with a single indirection resolves to ValueExpr`` () =
        let mEnv =
            EvalEnv.put env ("HairColour", TypeExpr.Primitive String)
            |> Result.defaultWith (fun _ -> failwith "")

        let t =
            TypeExpr.Tuple
                [ TypeExpr.Primitive Int
                  TypeExpr.Primitive Bool
                  TypeExpr.Primitive String
                  TypeExpr.Lookup { Name = "HairColour" } ]

        let v =
            TypeValue.Tuple
                [ TypeValue.Primitive Int
                  TypeValue.Primitive Bool
                  TypeValue.Primitive String
                  TypeValue.Primitive String ]

        check mEnv t v

module SetAndList =
    // pretty trivial
    [<Test>]
    let ``List TypeExpr resolves to List ValueType`` () =
        let t =
            TypeExpr.List(
                TypeExpr.Tuple
                    [ TypeExpr.Primitive Int
                      TypeExpr.Primitive Bool
                      TypeExpr.Primitive String
                      TypeExpr.Primitive String ]
            )

        let v =
            TypeValue.List(
                TypeValue.Tuple
                    [ TypeValue.Primitive Int
                      TypeValue.Primitive Bool
                      TypeValue.Primitive String
                      TypeValue.Primitive String ]
            )

        check env t v

    [<Test>]
    let ``Set TypeExpr resolves to Set TypeValue`` () =
        let t =
            TypeExpr.Set(
                TypeExpr.Tuple
                    [ TypeExpr.Primitive Int
                      TypeExpr.Primitive Bool
                      TypeExpr.Primitive String
                      TypeExpr.Primitive String ]
            )

        let v =
            TypeValue.Set(
                TypeValue.Tuple
                    [ TypeValue.Primitive Int
                      TypeValue.Primitive Bool
                      TypeValue.Primitive String
                      TypeValue.Primitive String ]
            )

        check env t v

module Sum =
    // Sum is the algebraic data type, equivalent to DU.
    // but that stuff does not really matter here :)
    [<Test>]
    let ``Sum TypeExpr resolves to Sum TypeValue`` () =
        let t =
            TypeExpr.Sum
                [ TypeExpr.Primitive Int
                  TypeExpr.Primitive Bool
                  TypeExpr.Primitive String
                  TypeExpr.Primitive String ]

        let v =
            TypeValue.Sum
                [ TypeValue.Primitive Int
                  TypeValue.Primitive Bool
                  TypeValue.Primitive String
                  TypeValue.Primitive String ]

        check env t v

module Rotate =
    [<Test>]
    let ``Rotate TypeExpr rotates and resolves to Tuple TypeValue`` () =
        let t =
            TypeExpr.Rotate(
                TypeExpr.Tuple [ TypeExpr.Primitive Int; TypeExpr.Primitive Bool; TypeExpr.Primitive String ]
            )

        let v = 
            TypeValue.Tuple
                [ TypeValue.Primitive Bool
                  TypeValue.Primitive String
                  TypeValue.Primitive Int ]

        check env t v

module LambdaAndApply =
    [<Test>]
    let ``Lambda TypeExpr fails when outside an Apply`` () =
        let t =
            TypeExpr.Sum
                [ TypeExpr.Primitive Int
                  TypeExpr.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) ]
        let v = 
            TypeValue.Sum
                 [ TypeValue.Primitive Int
                   TypeValue.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) ]

        check env t v

    [<Test>]
    let ``Apply TypeExpr on identity Lambda TypeExpr resolved to expected Primitive Type Value`` () =
        let idLambda =
            TypeExpr.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) // an identity lambda :)

        let te = TypeExpr.Primitive String
        let t = TypeExpr.Apply(idLambda, te)
        let v = TypeValue.Primitive String
        check env t v

    [<Test>]
    let ``Apply TypeExpr with no Lambda TypeExpr fails`` () =
        let te = TypeExpr.Primitive String
        let t = TypeExpr.Apply(te, te)
        checkFails env t


    [<Test>]
    let ``Check a rather complex case`` () =
        let lambda =
            TypeExpr.Lambda (
                {Name = "T"; Kind = Kind.Star},
                TypeExpr.Tuple [ 
                    TypeExpr.Var {Name = "T"};
                    TypeExpr.Var {Name = "T"};
                    TypeExpr.Var {Name = "T"}
                ]
            )
        let apply =
            TypeExpr.Apply (
                lambda,
                TypeExpr.Set (TypeExpr.Primitive Int)
            )

        let expectedOutput = 
            TypeValue.Tuple [ 
                TypeValue.Set (TypeValue.Primitive Int);
                TypeValue.Set (TypeValue.Primitive Int);
                TypeValue.Set (TypeValue.Primitive Int)
            ]

        check env apply expectedOutput

module KeyOf =
    let l =
        [ "name1", TypeExpr.Primitive Int
          "name2", TypeExpr.Primitive String
          "name3", TypeExpr.Primitive Bool ]

    let t = l |> Map.ofList |> TypeExpr.Record |> TypeExpr.KeyOf

    let v =
        [ TypeValue.Primitive(StringLiteral "name1")
          TypeValue.Primitive(StringLiteral "name2")
          TypeValue.Primitive(StringLiteral "name3") ]
        |> TypeValue.Sum

    [<Test>]
    let ``KeyOf yields a Sum with proper size and proper StringLiteral`` () = check env t v
