module TypeForge.Tests.Evaluation

open NUnit.Framework
open TypeForge.Ast
open TypeForge.Evaluation
open FsToolkit.ErrorHandling


let env: Environment<TypeExpr> = { map = Map.empty }

let check env t v =
    match typeEval env t with
    | Ok k when k = v -> ()
    | x -> failwithf "\n Found:    %A\n expected: %A" x v

let checkFails env t =
    match typeEval env t with
    | Error _ -> ()
    | Ok other -> failwithf "Found Ok(%A), expected Error()" (Ok other)

module Primitive =
    let t = TypeExpr.Primitive Int
    let v = TypeValue.Primitive Int

    [<Test>]
    let ``Prmitive TypeExpr maps into Primitive TypeValue`` () = check env t v

module Var =
    // This line essentially simulates what the apply case would add to the eval context.
    let mEnv =
        EvalEnv.put env ("T", TypeExpr.Primitive Int)
        |> Result.defaultWith (fun _ -> failwith "")

    let t = TypeExpr.Var { Name = "T" }
    let v = TypeValue.Primitive Int

    [<Test>]
    let ``Var TypeExpr maps into TypeValue when env contians TypeVar `` () = check mEnv t v

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
    let ``Lookup in env fails and yields Error`` () =
        let t = TypeExpr.Lookup { Name = "WrongExpressionName" }
        checkFails env t

module Arrow =
    // These just represent function type from TypeExpr --> TypeExpr
    // they can represent pretty much anything on both sides of the arrow.
    // As such they are pretty trivial
    let t = TypeExpr.Arrow(TypeExpr.Primitive Int, TypeExpr.Primitive Bool)

    let v = TypeValue.Arrow(TypeValue.Primitive Int, TypeValue.Primitive Bool)
    [<Test>]
    let ``Arrow in TypeExpr maps to Arrow in TypeValue`` () = check env t v

module Record =
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

    [<Test>]
    let ``Record TypeExpr with a single indirection resolves to ValueExpr`` () =  check mEnv t v

module Tuple =
    // Same deal as above but unnamed
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
              
    [<Test>]
    let ``Tuple TypeExpr with a single indirection resolves to ValueExpr`` () =
        check mEnv t v

module SetAndList =
    // pretty trivial

    let tplTe = 
        TypeExpr.Tuple
            [ TypeExpr.Primitive Int
              TypeExpr.Primitive Bool
              TypeExpr.Primitive String
              TypeExpr.Primitive String ]

    let tplTv = 
        TypeValue.Tuple
            [ TypeValue.Primitive Int
              TypeValue.Primitive Bool
              TypeValue.Primitive String
              TypeValue.Primitive String ]

    [<Test>]
    let ``List TypeExpr resolves to List ValueType`` () =
        let t = TypeExpr.List tplTe
        let v =  TypeValue.List tplTv 
        check env t v

    [<Test>]
    let ``Set TypeExpr resolves to Set TypeValue`` () =
        let t = TypeExpr.Set tplTe
        let v =  TypeValue.Set tplTv 

        check env t v

module Sum =
    // Sum is the algebraic data type, equivalent to DU.
    // but that stuff does not really matter here :)
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
    [<Test>]
    let ``Sum TypeExpr resolves to Sum TypeValue`` () = check env t v

module Rotate =
    let t =
        TypeExpr.Rotate(
            TypeExpr.Tuple [ TypeExpr.Primitive Int; TypeExpr.Primitive Bool; TypeExpr.Primitive String ]
        )

    let v = 
        TypeValue.Tuple
            [ TypeValue.Primitive Bool
              TypeValue.Primitive String
              TypeValue.Primitive Int ]

    [<Test>]
    let ``Rotate TypeExpr rotates and resolves to Tuple TypeValue`` () = check env t v

module LambdaAndApply =
    let t =
        TypeExpr.Sum
            [ TypeExpr.Primitive Int
              TypeExpr.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) ]
    let v = 
        TypeValue.Sum
             [ TypeValue.Primitive Int
               TypeValue.Lambda({ Name = "T"; Kind = Star }, TypeExpr.Var { Name = "T" }) ]

    [<Test>]
    let ``Lambda TypeExpr is passed on when outside of Apply`` () =
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
    let ``Apply TypeExpr with no Lambda TypeExpr fails`` ()=
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

    [<Test>]
    let ``Check a nested case, 2 lambdas and their application, or currying on 2 arguments`` () =
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

        let expectedOutput = 
            TypeValue.Tuple [ 
                TypeValue.Primitive String
                TypeValue.Primitive Bool
                
            ]

        check env t expectedOutput

    [<Test>]
    let ``Lambda as argument of Lambda -> we can inject a type contructor in a type constructor (an identity, but still)`` () =
        let identity = 
            TypeExpr.Lambda ( // identity lambda
                { Name = "T"; Kind = Kind.Arrow (Star,Star) },
                TypeExpr.Var {Name = "T"};
            )

        let lambdaArg =
            TypeExpr.Lambda (
                { Name = "V"; Kind = Star },
                TypeExpr.List (TypeExpr.Var {Name = "V"});
            ) 

        let apply =
            TypeExpr.Apply (
                identity,
                lambdaArg
            )

        let o =  TypeValue.Lambda (
                    { Name = "V"; Kind = Star },
                    TypeExpr.List (TypeExpr.Var {Name = "V"});
                )

        check env apply o

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
