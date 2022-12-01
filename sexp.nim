# Everything of use from this file has been copied to jyss
# (You can safely ignore this file)

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
# Quote takes a string, and returns the parsed S-expression
proc quote(input: string): SExpr =
  var
    tokens = lex(input)
    pos = 0
  result = parseSexpr(tokens, pos)

when isMainModule:
  # S-Expressions
  doAssert quote("1") == SExpr(kind: seInt, intVal: 1)
  doAssert quote("3.14") == SExpr(kind: seFloat, floatVal: 3.14)
  doAssert quote(""""bar"""") == SExpr(kind: seString, stringVal: "bar")
  doAssert quote("""(foo "bar")""") == SExpr(kind: seList, children: @[SExpr(kind: seIdent, ident: "foo"), SExpr(kind: seString, stringVal: "bar")])
  # Parse
  echo "done testing."