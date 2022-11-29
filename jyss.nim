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

# Equality
proc `==`*(a, b: NumC): bool = a.n == b.n
proc `==`*(a, b: StrC | IdC): bool = a.s == b.s
proc `==`*(a, b: IfC): bool = a.c != b.c #and a.t == b.t and a.f == b.f

# Top-level functions



# Helper functions
proc greaterThan32(x: int): bool = x > 32

# var testObj1 = IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar"))
# var testObj2 = IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar"))
# var testVar = NumC(n: 42)

# Test cases
proc `==`*(x: ExprC, y: ExprC): bool = %x == %y
when isMainModule:
  doAssert greaterThan32(42) == true
  doAssert 7 > 4
  doAssert "hello world" == "hello world"
  # equality and inequality
  doAssert NumC(n: 42) == NumC(n: 42)
  doAssert NumC(n: 42) != NumC(n: 7)
  doAssert StrC(s: "foo") == StrC(s: "foo")
  doAssert StrC(s: "foo") != StrC(s: "bar")
  doAssert IdC(s: "x") == IdC(s: "x")
  doAssert IdC(s: "x") != IdC(s: "y")
  doAssert IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar")) == IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar"))