module TypeForge.Workbench

open TypeForge.Ast
let t = 0

let typeEval (e: TypeExpr) : TypeValue =
    match e with
    | TypeExpr.Primitive p -> Primitive p
    | _ -> failwith "case not covered, yet..."
