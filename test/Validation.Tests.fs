module TypeForge.Tests.Validation

open NUnit.Framework
open TypeForge.Ast
open TypeForge.Validation


type KindEnv = Environment<Kind>
let env: Environment<Kind> = { map = Map.empty }

let putForce env kind =
    // Use when you know that the env is empty. Avoids pattern matching in tests... urgh.
    match KindEnv.put env kind with
    | Ok env -> env
    | Error e -> failwith e

let check env (t: TypeExpr) (finalKind: Kind) =
    match kindCheck env t with
    | Ok k when k = finalKind -> ()
    | x -> failwithf "Found %A, expected: %A" x finalKind

let checkFails env t =
    match kindCheck env t with
    | Error _ -> ()
    | Ok other -> failwithf "Found Ok(%A), expected Error()" (Ok other)

module Primitive =
    let t = TypeExpr.Primitive Int
    let v = Star

    [<Test>]
    let ``Primitive has Star`` () = check env t v

module VarAndLookup =
    let mEnv = putForce env ("T", Kind.Arrow(Star, Star))

    let tv = TypeExpr.Var { Name = "T" }
    let tl = TypeExpr.Lookup { Name = "T" }

    let v = Kind.Arrow(Star, Star)

    [<Test>]
    let ``Var fetches proper Kind`` () = check mEnv tv v

    [<Test>]
    let ``Lookup fetches proper Kind`` () = check mEnv tl v

module ListAndSet =
    let tList =
        TypeExpr.List(TypeExpr.Primitive Int)
    let tSet =
        TypeExpr.List(TypeExpr.Primitive Bool)

    let v = Kind.Star

    [<Test>]
    let ``List of * returns *`` () = check env tList v

    [<Test>]
    let ``Set of * returns *`` () = check env tSet v

module ArrowAndMap =
    let left =
        TypeExpr.List(TypeExpr.Primitive Int)
    let right =
        TypeExpr.List(TypeExpr.Primitive Bool)

    let t = TypeExpr.Arrow (left,right)

    let v = Star

    [<Test>]
    let ``Arrow of * -> * returns *`` () = check env t v

module LambdaAndApply =
    //we build a lambda that returns a 3-uple of the argument
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
    [<Test>]
    let ``Application of Lambda on arg returns *`` () = 
        check env apply Star
    [<Test>]
    let ``Unresolved Lambda (in the wild) returns * -> *`` () = 
        check env lambda (Kind.Arrow (Star,Star))

    [<Test>]
    let ``Check a nested case, 2 lambdas and their application returns *`` () =
        let t = // on T
            TypeExpr.Apply (
                TypeExpr.Lambda (
                    {Name = "T"; Kind = Kind.Star},
                    TypeExpr.Apply (
                        TypeExpr.Lambda (
                            {Name = "U"; Kind = Kind.Star},
                            TypeExpr.Tuple [ 
                                TypeExpr.Var {Name = "T"};
                                TypeExpr.Var {Name = "U"};
                            ]
                        ),
                    TypeExpr.Primitive Bool
                    )
                ),
                TypeExpr.Primitive String
            )

        check env t Star

// module Arrow =
//     // Not as fun as arrow but a better place than Apply and Lambda
//     // these just represent function type from TypeExpr --> TypeExpr
//     // they can represent pretty much anything on both sides of the arrow.
//     // As such they are pretty trivial
//     [<Test>]
//     let ``Arrow in TypeExpr maps to Arrow in TypeValue`` () =
//         let t = TypeExpr.Arrow(TypeExpr.Primitive Bool, TypeExpr.Var { Name = "VarName" })

//         let v =
//             TypeValue.Arrow(TypeValue.Primitive Bool, TypeValue.Var { Name = "VarName" })

//         check env t v

// module Record =
//     // fairly simple as well as we are just representing records
//     // F#
//     //   type MyRecord = {
//     //     age : int
//     //     isBiped : bool
//     //     name : string
//     //     hairColour : HairColour
//     //   }
//     [<Test>]
//     let ``Record TypeExpr with a single indirection resolves to ValueExpr`` () =
//         let mEnv =
//             EvalEnv.storeTypeIdentifier env ("HairColour", TypeExpr.Primitive String)
//             |> Result.defaultWith (fun _ -> failwith "")

//         let t =
//             TypeExpr.Record(
//                 [ "age", TypeExpr.Primitive Int
//                   "isBiped", TypeExpr.Primitive Bool
//                   "name", TypeExpr.Primitive String
//                   "hairColour", TypeExpr.Lookup { Name = "HairColour" } ]
//                 |> Map.ofList
//             )

//         let v =
//             TypeValue.Record(
//                 [ "age", TypeValue.Primitive Int
//                   "isBiped", TypeValue.Primitive Bool
//                   "name", TypeValue.Primitive String
//                   "hairColour", TypeValue.Primitive String ]
//                 |> Map.ofList
//             )

//         check mEnv t v

// module Tuple =
//     // Same deal as above but unnamed
//     [<Test>]
//     let ``Tuple TypeExpr with a single indirection resolves to ValueExpr`` () =
//         let mEnv =
//             EvalEnv.storeTypeIdentifier env ("HairColour", TypeExpr.Primitive String)
//             |> Result.defaultWith (fun _ -> failwith "")

//         let t =
//             TypeExpr.Tuple
//                 [ TypeExpr.Primitive Int
//                   TypeExpr.Primitive Bool
//                   TypeExpr.Primitive String
//                   TypeExpr.Lookup { Name = "HairColour" } ]

//         let v =
//             TypeValue.Tuple
//                 [ TypeValue.Primitive Int
//                   TypeValue.Primitive Bool
//                   TypeValue.Primitive String
//                   TypeValue.Primitive String ]

//         check mEnv t v

// module SetAndList =
//     // pretty trivial
//     [<Test>]
//     let ``List TypeExpr resolves to List ValueType`` () =
//         let t =
//             TypeExpr.List(
//                 TypeExpr.Tuple
//                     [ TypeExpr.Primitive Int
//                       TypeExpr.Primitive Bool
//                       TypeExpr.Primitive String
//                       TypeExpr.Primitive String ]
//             )

//         let v =
//             TypeValue.List(
//                 TypeValue.Tuple
//                     [ TypeValue.Primitive Int
//                       TypeValue.Primitive Bool
//                       TypeValue.Primitive String
//                       TypeValue.Primitive String ]
//             )

//         check env t v

//     [<Test>]
//     let ``Set TypeExpr resolves to Set TypeValue`` () =
//         let t =
//             TypeExpr.Set(
//                 TypeExpr.Tuple
//                     [ TypeExpr.Primitive Int
//                       TypeExpr.Primitive Bool
//                       TypeExpr.Primitive String
//                       TypeExpr.Primitive String ]
//             )

//         let v =
//             TypeValue.Set(
//                 TypeValue.Tuple
//                     [ TypeValue.Primitive Int
//                       TypeValue.Primitive Bool
//                       TypeValue.Primitive String
//                       TypeValue.Primitive String ]
//             )

//         check env t v

// module Sum =
//     // Sum is the algebraic data type, equivalent to DU.
//     // but that stuff does not really matter here :)
//     [<Test>]
//     let ``Sum TypeExpr resolves to Sum TypeValue`` () =
//         let t =
//             TypeExpr.Sum
//                 [ TypeExpr.Primitive Int
//                   TypeExpr.Primitive Bool
//                   TypeExpr.Primitive String
//                   TypeExpr.Primitive String ]

//         let v =
//             TypeValue.Sum
//                 [ TypeValue.Primitive Int
//                   TypeValue.Primitive Bool
//                   TypeValue.Primitive String
//                   TypeValue.Primitive String ]

//         check env t v

// module Rotate =
//     [<Test>]
//     let ``Rotate TypeExpr rotates and resolves to Tuple TypeValue`` () =
//         let t =
//             TypeExpr.Rotate(
//                 TypeExpr.Tuple [ TypeExpr.Primitive Int; TypeExpr.Primitive Bool; TypeExpr.Primitive String ]
//             )

//         let v =
//             TypeValue.Tuple
//                 [ TypeValue.Primitive Bool
//                   TypeValue.Primitive String
//                   TypeValue.Primitive Int ]

//         check env t v

// module LambdaAndApply =
//     [<Test>]
//     let ``Lambda TypeExpr fails when outside an Apply`` () =
//         let t =
//             let lambda =
//                 TypeExpr.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) // an identity lambda :)

//             TypeExpr.Sum [ TypeExpr.Primitive Int; lambda ]

//         checkFails env t

//     [<Test>]
//     let ``Apply TypeExpr on identity Lambda TypeExpr resolved to expected Primitive Type Value`` () =
//         let idLambda =
//             TypeExpr.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) // an identity lambda :)

//         let te = TypeExpr.Primitive String
//         let t = TypeExpr.Apply(idLambda, te)
//         let v = TypeValue.Primitive String
//         check env t v

//     [<Test>]
//     let ``Apply TypeExpr with no Lambda TypeExpr fails`` () =
//         let te = TypeExpr.Primitive String
//         let t = TypeExpr.Apply(te, te)
//         let v = TypeValue.Primitive String
//         checkFails env t
