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

type
  Value = ref object of RootObj
type
  NumV = ref object of Value
    n: float
  ClosV = ref object of Value
    a: seq[IdC]
    b: ExprC
    e: seq[Binding]
  PrimopV = ref object of Value
    o: string
  BoolV = ref object of Value
    b: bool
  StrV = ref object of Value
    s: string
  Binding = ref object
    n: string
    v: Value

# Stringify overrides
method `$`*(e: ExprC): string {.base.} = quit "must override `$` for ExprC subclasses"
method `$`*(e: NumC): string = "NumC(" & $e.n & ")"
method `$`*(e: StrC): string = "StrC(" & $e.s & ")"
method `$`*(e: IdC): string = "IdC(" & $e.s & ")"
method `$`*(e: IfC): string = "IfC(" & $e.c & ", " & $e.t & ", " & $e.f & ")"
method `$`*(e: AppC): string = "AppC(" & $e.a & ", " & $e.b & ")"
method `$`*(e: LamC): string = "LamC(" & $e.a & ", " & $e.b & ")"
method `$`*(e: Value): string {.base.} = quit "must override `$` for Value subclasses"
method `$`*(e: NumV): string = "NumV(" & $e.n & ")"
method `$`*(e: PrimopV): string = "PrimopV(" & $e.o & ")"
method `$`*(e: StrV): string = "StrV(" & $e.s & ")"
method `$`*(e: BoolV): string = "BoolV(" & $e.b & ")"
method `$`*(e: Binding): string = "Binding(" & $e.n & ", " & $e.v & ")"
method `$`*(e: ClosV): string = "ClosV(" & $e.a & ", " & $e.b & ", " & $e.e & ")"
# Equality uses stringify
method `==`*(a, b: NumC | StrC | IdC | IfC | LamC | AppC): bool = $a == $b
method `==`*(a, b: NumV | ClosV | PrimopV | BoolV | StrV): bool = $a == $b

var top_env: seq[Binding] = @[Binding(n: "+", v: PrimopV(o: "+")), 
Binding(n: "-", v: PrimopV(o: "-")), 
Binding(n: "*", v: PrimopV(o: "*")), 
Binding(n: "/", v: PrimopV(o: "/")),
Binding(n: "<=", v: PrimopV(o: "<="))]

# Top-level functions
proc serialize(v: Value): string = 
  if v of StrV:
    return StrV(v).s
  elif v of NumV:
    return $(NumV(v).n)
  elif v of BoolV:
    return $(BoolV(v).b)
  elif v of PrimopV:
    return "#<primop>"
  elif v of ClosV:
    return "#<procedure>"
  else:
    raise newException(ValueError, "Invalid Input to serialize")

proc lookup(what: string, env: seq[Binding]): Value =
  if len(env) == 0:
    raise newException(ValueError, "No Binding for Variable")
  else:
    if what == env[0].n:
      return env[0].v
    else:
      return lookup(what, env[1 .. len(env)-1])

proc primop_eval(op: string, a1: Value, a2: Value): Value = 
  if a1 of NumV and a2 of NumV:
    var arg1: float = NumV(a1).n
    var arg2: float = NumV(a2).n
    case op:
      of "+":
        return NumV(n: arg1 + arg2)
      of "-":
        return NumV(n: arg1 - arg2)
      of "/":
        if arg2 == 0:
          raise newException(ValueError, "Division By Zero")
        else:
          return NumV(n: arg1 / arg2)
      of "*":
        return NumV(n: arg1 * arg2)
      of "<=":
        return BoolV(b: arg1 <= arg2)
      else:
        raise newException(ValueError, "Invalid Primitive Operator")
  else:
    raise newException(ValueError, "Invalid Inputs to Arithmetic Expresison")

proc interp(e: ExprC, env: seq[Binding]): Value =
  if e of NumC:
    return NumV(n: float(NumC(e).n))
  elif e of StrC:
    return StrV(s: StrC(e).s)
  elif e of IdC:
    if IdC(e).s == "true":
      return BoolV(b: true)
    elif IdC(e).s == "false":
      return BoolV(b: false)
    else:
      return lookup(IdC(e).s, env)
  elif e of IfC:
    var cond_res: Value = interp(IfC(e).c, env)
    if cond_res of BoolV:
      if BoolV(cond_res).b == true:
        return interp(IfC(e).t, env)
      else:
        return interp(IfC(e).f, env)
    else:
      raise newException(ValueError, "invalid type")
  elif e of LamC:
    return ClosV(a: LamC(e).a, b: LamC(e).b, e: env)
  elif e of AppC:
    var int_v: Value = interp(AppC(e).b, env)
    if int_v of PrimopV:
      if len(AppC(e).a) == 2:
        var arg_1: Value = interp(AppC(e).a[0], env)
        var arg_2: Value = interp(AppC(e).a[1], env)
        return primop_eval(PrimopV(int_v).o, arg_1, arg_2)
    else:
      return NumV(n: -1)
  else:
    return NumV(n: -1)

# Helper functions
## lookup above interp due to undeclared identifier errors

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
  doAssert serialize(interp(NumC(n: 3), @[])) == "3.0"
  doAssert serialize(interp(IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar")), @[])) == "foo"
  doAssert serialize(lookup("x", @[Binding(n: "y", v: NumV(n: 5)), Binding(n: "x", v: NumV(n: 34)), 
  Binding(n: "z", v: StrV(s: "5"))])) == "34.0"
  doAssert serialize(interp( IfC(c: AppC(a: @[NumC(n: 42), NumC(n: 41)], b: IdC(s: "<=")) , t: StrC(s: "foo"), f: StrC(s: "bar")), top_env)) == "bar"

  