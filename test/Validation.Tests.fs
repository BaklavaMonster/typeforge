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
