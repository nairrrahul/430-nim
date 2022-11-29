import strformat
import json

# Typedefs
type
  ExprC = ref object of RootObj
type
  NumC = ref object of ExprC
    n: int
  StrC = ref object of ExprC
    s: string
  IdC = ref object of ExprC
    s: string
  IfC = ref object of ExprC
    c: ExprC
    t: ExprC
    f: ExprC
  LamC = ref object of ExprC
    a: seq[IdC]
    b: ExprC
  AppC = ref object of ExprC
    a: seq[ExprC]
    b: ExprC

# Stringify overrides
method `$`*(e: ExprC): string {.base.} = quit "must override `$` for ExprC subclasses"
method `$`*(e: NumC): string = "NumC(" & $e.n & ")"
method `$`*(e: StrC): string = "StrC(" & $e.s & ")"
method `$`*(e: IdC): string = "IdC(" & $e.s & ")"
method `$`*(e: IfC): string = "IfC(" & $e.c & ", " & $e.t & ", " & $e.f & ")"
# Equality uses stringify
method `==`*(a, b: NumC | StrC | IdC | IfC | LamC | AppC): bool = $a == $b

# Top-level functions


# Helper functions


# Test cases
when isMainModule:
  # equality and inequality of AST types
  doAssert NumC(n: 42) == NumC(n: 42)
  doAssert NumC(n: 42) != NumC(n: 7)
  doAssert StrC(s: "foo") == StrC(s: "foo")
  doAssert StrC(s: "foo") != StrC(s: "bar")
  doAssert IdC(s: "x") == IdC(s: "x")
  doAssert IdC(s: "x") != IdC(s: "y")
  doAssert IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar")) == IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar"))
  doAssert IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar")) != IfC(c: StrC(s: "true"), t: IdC(s: "foo"), f: IdC(s: "bar"))