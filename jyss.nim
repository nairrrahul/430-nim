import strformat
import json
import std/sequtils

################################################
##       S-EXPRESSION CODE ADAPTED FROM       ##
## https://rosettacode.org/wiki/S-expressions ##
################################################
import strutils
type
  TokenKind = enum
    tokInt, tokFloat, tokString, tokIdent
    tokLPar, tokRPar
    tokEnd
  Token = object
    case kind: TokenKind
    of tokString: stringVal: string
    of tokInt: intVal: int
    of tokFloat: floatVal: float
    of tokIdent: ident: string
    else: discard
proc lex(input: string): seq[Token] =
  var pos = 0
  template current: char =
    if pos < input.len: input[pos]
    else: '\x00'
  while pos < input.len:
    case current
    of ';':
      inc(pos)
      while current notin {'\r', '\n'}:
        inc(pos)
      if current == '\r': inc(pos)
      if current == '\n': inc(pos)
    of '(': inc(pos); result.add(Token(kind: tokLPar))
    of ')': inc(pos); result.add(Token(kind: tokRPar))
    of '0'..'9':
      var
        num = ""
        isFloat = false
      while current in Digits:
        num.add(current)
        inc(pos)
      if current == '.':
        num.add(current)
        isFloat = true
        inc(pos)
        while current in Digits:
          num.add(current)
          inc(pos)
      result.add(if isFloat: Token(kind: tokFloat, floatVal: parseFloat(num))
                 else: Token(kind: tokInt, intVal: parseInt(num)))
    of ' ', '\t', '\n', '\r': inc(pos)
    of '"':
      var str = ""
      inc(pos)
      while current != '"':
        str.add(current)
        inc(pos)
      inc(pos)
      result.add(Token(kind: tokString, stringVal: str))
    else:
      const BannedChars = {' ', '\t', '"', '(', ')', ';'}
      var ident = ""
      while current notin BannedChars:
        ident.add(current)
        inc(pos)
      result.add(Token(kind: tokIdent, ident: ident))
  result.add(Token(kind: tokEnd))
type
  SExprKind = enum
    seInt, seFloat, seString, seIdent, seList
  SExpr = ref object
    case kind: SExprKind
    of seInt: intVal: int
    of seFloat: floatVal: float
    of seString: stringVal: string
    of seIdent: ident: string
    of seList: children: seq[SExpr]
  ParseError = object of CatchableError
proc `$`*(se: SExpr): string =
  case se.kind
  of seInt: result = $se.intVal
  of seFloat: result = $se.floatVal
  of seString: result = '"' & se.stringVal & '"'
  of seIdent: result = se.ident
  of seList:
    result = "("
    for i, ex in se.children:
      if ex.kind == seList and ex.children.len > 1:
        result.add("\n")
        result.add(indent($ex, 2))
      else:
        if i > 0:
          result.add(" ")
        result.add($ex)
    result.add(")")
proc current*[T: Ordinal](tokens: seq[Token], pos: var T): Token =
  if pos < tokens.len: tokens[pos]
  else: Token(kind: tokEnd)
proc parseInt(token: Token): SExpr =
  result = SExpr(kind: seInt, intVal: token.intVal)
proc parseFloat(token: Token): SExpr =
  result = SExpr(kind: seFloat, floatVal: token.floatVal)
proc parseString(token: Token): SExpr =
  result = SExpr(kind: seString, stringVal: token.stringVal)
proc parseIdent(token: Token): SExpr =
  result = SExpr(kind: seIdent, ident: token.ident)
proc parseSexpr*[T: Ordinal](tokens: seq[Token], pos: var T): SExpr
proc parseList*[T: Ordinal](tokens: seq[Token], pos: var T): SExpr =
  result = SExpr(kind: seList)
  while current(tokens, pos).kind notin {tokRPar, tokEnd}:
    result.children.add(parseSexpr(tokens, pos))
  if current(tokens, pos).kind == tokEnd:
    raise newException(ParseError, "Missing right paren ')'")
  else:
    inc(pos)
proc parseSexpr*[T: Ordinal](tokens: seq[Token], pos: var T): SExpr =
  var token = current(tokens, pos)
  inc(pos)
  result =
    case token.kind
    of tokInt: parseInt(token)
    of tokFloat: parseFloat(token)
    of tokString: parseString(token)
    of tokIdent: parseIdent(token)
    of tokLPar: parseList(tokens, pos)
    else: nil
#########################################
## END OF "BORROWED" S-EXPRESSION CODE ##
#########################################

# Equality override for SExpr (easy route with stringify)
proc `==`*(a, b: SExpr): bool = $a == $b

# Takes a string, and returns the parsed S-expression
proc quote(input: string): SExpr =
  var
    tokens = lex(input)
    pos = 0
  result = parseSexpr(tokens, pos)

## Typedefs

# ExprC = NumC | StrC | IdC | IfC | LamC | AppC
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

# Value = NumV | ClosV | PrimopV | BoolV | StrV, and Binding (for ClosV environments)
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

# Equality overrides to use stringify
method `==`*(a, b: NumC | StrC | IdC | IfC | LamC | AppC): bool = $a == $b
method `==`*(a, b: NumV | ClosV | PrimopV | BoolV | StrV): bool = $a == $b

# Declasre the top env with primitive operator bindings
var top_env: seq[Binding] = @[Binding(n: "+", v: PrimopV(o: "+")), 
                              Binding(n: "-", v: PrimopV(o: "-")),
                              Binding(n: "*", v: PrimopV(o: "*")),
                              Binding(n: "/", v: PrimopV(o: "/")),
                              Binding(n: "<=", v: PrimopV(o: "<="))]

## Top-level functions

# Parses an SExpr into ExprCs
proc parse(se: SExpr): ExprC =
  case se.kind
  of seInt: result = NumC(n: se.intVal)
  of seFloat: result = NumC(n: se.floatVal.int) # convert to int (quick fix for type errors...)
  of seString: result = StrC(s: se.stringVal)
  of seIdent: result = IdC(s: se.ident)
  of seList:
    if (se.children[0].kind == seIdent and se.children[0].ident == "if" and se.children.len == 4): #IfC
      result = IfC(c: parse(se.children[1]), t: parse(se.children[2]), f: parse(se.children[3]))
    elif (se.children[0].kind == seIdent and se.children[0].ident == "proc" and se.children[1].kind == seList and se.children[2].kind == seIdent and se.children[2].ident == "go" and se.children.len == 4): #LamC
      # Dangerous cast here, would spend more time if we could :)
      var args = cast[seq[IdC]](map(se.children[1].children, proc(c: SExpr): ExprC = parse(c)))
      result = LamC(a: args, b: parse(se.children[3]))
    else: #AppC
      var args = se.children
      args.delete(0..0)
      result = AppC(b: parse(se.children[0]), a: map(args, parse)) # map(se.children, proc(c: SExpr): ExprC = parse(c))

# Serializes Value(s) into printable strings
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

# Given a string (id), returns the value stored in an environment
proc lookup(what: string, env: seq[Binding]): Value =
  if len(env) == 0:
    raise newException(ValueError, "No Binding for Variable")
  else:
    if what == env[0].n:
      return env[0].v
    else:
      return lookup(what, env[1 .. len(env)-1])

# Based on the operator and its two input values, evaluates the output value
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

# Interprets an ExprC given an env
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

# Top-interp - a full JYSS.Nim program!
proc topInterp(input: string): string = serialize(interp(parse(quote(input)), top_env))

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
  # S-Expressions
  doAssert quote("1") == SExpr(kind: seInt, intVal: 1)
  doAssert quote("3.14") == SExpr(kind: seFloat, floatVal: 3.14)
  doAssert quote(""""bar"""") == SExpr(kind: seString, stringVal: "bar")
  doAssert quote("""(foo "bar")""") == SExpr(kind: seList, children: @[SExpr(kind: seIdent, ident: "foo"), SExpr(kind: seString, stringVal: "bar")])
  # Parse
  doAssert $parse(quote("1")) == $NumC(n: 1)
  doAssert $parse(quote("3.14")) == $NumC(n: 3)
  doAssert $parse(quote("""(if true "foo" "bar")""")) == $IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar"))
  doAssert $parse(quote("""(proc (x) go 42)""")) == $LamC(a: @[IdC(s: "x")], b: NumC(n: 42))
  doAssert $parse(quote("""(* 21 2)""")) == $AppC(a: cast[seq[ExprC]](@[NumC(n: 21), NumC(n: 2)]), b: IdC(s: "*"))
  # Serialize & Interp
  doAssert serialize(interp(NumC(n: 3), @[])) == "3.0"
  doAssert serialize(interp(IfC(c: IdC(s: "true"), t: StrC(s: "foo"), f: StrC(s: "bar")), @[])) == "foo"
  doAssert serialize(lookup("x", @[Binding(n: "y", v: NumV(n: 5)), Binding(n: "x", v: NumV(n: 34)), Binding(n: "z", v: StrV(s: "5"))])) == "34.0"
  doAssert serialize(interp(IfC(c: AppC(a: cast[seq[ExprC]](@[NumC(n: 42), NumC(n: 41)]), b: IdC(s: "<=")), t: StrC(s: "foo"), f: StrC(s: "bar")), top_env)) == "bar"
  # A full JYSS.Nim program :)
  doAssert topInterp("(* 2 21)") == "42.0"
  doAssert topInterp("(if true 7 3)") == "7.0"
  # Done
  echo "Done testing."