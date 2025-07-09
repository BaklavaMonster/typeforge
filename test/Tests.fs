module TypeForge.Tests

open NUnit.Framework
open TypeForge.Ast 

[<Test>]
let ``zero equals zero`` () = 
    Assert.That(0.0, Is.Zero)

[<Test>]
let ``primitive p maps into primitive p`` () =
    let tei = TypeExpr.Primitive PrimitiveType.Int
    let vei = TypeValue.Primitive PrimitiveType.Int
    Assert.That(vei, Is.EqualTo(Workbench.typeEval tei))
