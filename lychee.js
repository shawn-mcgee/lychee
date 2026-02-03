const Where = {
  new(
    idx = 0,
    row = 1,
    col = 1
  ) {
    return { idx, row, col }
  }
}

const Tokens = {
  ID : "token:id", 

  EQ : "token:eq", // =
  LS : "token:ls", // ,
  LP : "token:lp", // (
  RP : "token:rp", // )
  LB : "token:lb", // {
  RB : "token:rb", // }
  LK : "token:lk", // [
  RK : "token:rk", // ]
  SC : "token:sc", // ;
  NL : "token:nl", // \n

  NUM: "token:num",
  STR: "token:str",
}

const Token = {
  new(kind, value, where) {
    return { kind, value, where }
  },

  id(value, where) {
    return Token.new(Tokens.ID, value, where)
  },

  eq(where) {
    return Token.new(Tokens.EQ, "=", where)
  },

  ls(where) {
    return Token.new(Tokens.LS, ",", where)
  },

  lp(where) {
    return Token.new(Tokens.LP, "(", where)
  },
 
  rp(where) {
    return Token.new(Tokens.RP, ")", where)
  },
 
  lb(where) {
    return Token.new(Tokens.LB, "{", where)
  },
 
  rb(where) {
    return Token.new(Tokens.RB, "}", where)
  },
 
  lk(where) {
    return Token.new(Tokens.LK, "[", where)
  },
 
  rk(where) {
    return Token.new(Tokens.RK, "]", where)
  },

  sc(where) {
    return Token.new(Tokens.SC, ";", where)
  },

  nl(where) {
    return Token.new(Tokens.NL, "\\n", where)
  },

  num(value, where) {
    return Token.new(Tokens.NUM, value, where)
  },
 
  str(value, where) {
    return Token.new(Tokens.STR, value, where)
  }
}

function *lex(string) {
  let last = ""

  let i = Where.new()

  function eof() {
    return i.idx >= string.length
  }

  function peek() {
    return string[i.idx]
  }

  function read() {
    const c = string[i.idx]
    i.idx += 1
    if (c === "\n") {
      i.row += 1
      i.col  = 1
    } else {
      i.col += 1
    }
    return c
  }

  function isSpace(c) {
    return (
      c === " "  ||
      c === "\t" ||
      c === "\r"
    )
  }

  function isAlpha(c) {
    return (
      (c >= "a" && c <= "z") ||
      (c >= "A" && c <= "Z") ||
      c === "_"
    )
  }

  function isDigit(c) {
    return c >= "0" && c <= "9"
  }

  while (!eof()) {
    const j = {...i}
    const c = read()

    // skip whitespace
    if (isSpace(c)) 
      continue

    // read symbols
         if (c === "=") yield Token.eq(j);
    else if (c === ",") yield Token.ls(j);
    else if (c === "(") yield Token.lp(j);
    else if (c === ")") yield Token.rp(j);
    else if (c === "{") yield Token.lb(j);
    else if (c === "}") yield Token.rb(j);
    else if (c === "[") yield Token.lk(j);
    else if (c === "]") yield Token.rk(j);
    else if (c === ";") yield Token.sc(j);
    else if (c === "\n") {
      if (
        last !== "#" &&
        last !== "(" &&
        last !== "[" &&
        last !== "{" &&
        last !== "," &&
        last !== ";" &&
        last !== "\n"
      ) yield Token.nl(j);
    }
    // read number
    else if (isDigit(c) || c === "-" || c === "+") {
      if ((
        c === "-" ||
        c === "+"
      ) && (eof() || !isDigit(peek())))
        throw new Error(`[lex] Expected digit but received '${c}' instead on line ${i.row}:${i.col}`)
      
      let value = c

      while (!eof() && isDigit(peek()))
        value += read()
      if (!eof() && peek() === ".")
        value += read()
      while (!eof() && isDigit(peek()))
        value += read()
      yield Token.num(Number(value), j)
    }
    // read string
    else if (c === '"') {
      let value = ""
      while (!eof() && peek() !== '"')
        value += read()
      if (eof() || peek() !== '"')
        throw new Error(`[lex] Expected '"' but reached end of file instead on line ${j.row}:${j.col}`)
      read()
      yield Token.str(value, j)
    }
    // read id
    else if (isAlpha(c)) {
      let value = c
      while (!eof() && (
        isAlpha(peek()) ||
        isDigit(peek())
      )) value += read()
      yield Token.id(value, j)
    }
    // read comment
    else if (c === "#") {
      while (!eof() && peek() !== "\n")
        read()
    }
    else throw new Error(`[lex] Unexpected character '${c}' on line ${i.row}:${i.col}`)
    last = c
  }
}

const Nodes = {
  ID : "node:id" ,
  VAR: "node:var",
  FUN: "node:fun",
  RUN: "node:run" ,

  NUM: "node:num",
  STR: "node:str",
  OBJ: "node:obj",

  NOOP: "node:noop",
}

const Node = {
  id(value) {
    return { kind: Nodes.ID, value }
  },

  fun(parameters, statements) {
    return { kind: Nodes.FUN, parameters, statements }
  },

  var(identifier, expression) {
    return { kind: Nodes.VAR, identifier, expression }
  },

  run(definition, arguments_) {
    return { kind: Nodes.RUN, definition, arguments_ }
  },

  num(value) {
    return { kind: Nodes.NUM, value }
  },

  str(value) {
    return { kind: Nodes.STR, value }
  },

  obj(pairs) {
    return { kind: Nodes.OBJ, pairs }
  },

  noop() {
    return { kind: Nodes.NOOP }
  }
}

function ast(s) {
  function node(i, node) {
    return { ok: true , node, i }
  }

  function warn(i, warn) {
    return { ok: false, warn, i }
  }

  function tryParseId(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseId] Expected id but reached end of file instead`)

    const expectsId = s[i ++]
    if (expectsId.kind !== Tokens.ID)
      return warn(i, `[ast/tryParseId] Expected id but received '${expectsId.kind}' instead on line ${expectsId.where.row}:${expectsId.where.col}`)

    return node(i, Node.id(expectsId.value))
  }

  function tryParseNumber(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseNumber] Expected number but reached end of file instead`)
      
    const expectsNum = s[i ++]
    if (expectsNum.kind !== Tokens.NUM)
      return warn(i, `[ast/tryParseNumber] Expected number but received '${expectsNum.kind}' instead on line ${expectsNum.where.row}:${expectsNum.where.col}`)

    return node(i, Node.num(expectsNum.value))
  }

  function tryParseString(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseString] Expected string but reached end of file instead`)
  
    const expectsStr = s[i ++]
    if (expectsStr.kind !== Tokens.STR)
      return warn(i, `[ast/tryParseString] Expected string but received '${expectsStr.kind}' instead on line ${expectsStr.where.row}:${expectsStr.where.col}`)

    return node(i, Node.str(expectsStr.value))
  }

  function tryParseObject(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseObject] Expected object but reached end of file instead`)

    const expectsLb = s[i ++]
    if (expectsLb.kind !== Tokens.LB)
      return warn(i, `[ast/tryParseObject] Expected '{' but received '${expectsLb.kind}' instead on line ${expectsLb.where.row}:${expectsLb.where.col}`)

    if (i >= s.length)
      return warn(i, `[ast/tryParseObject] Expected pair or '}' but reached end of file instead`)

    const pairs = [ ]

    let maybeRb = s[i]
    while (maybeRb.kind !== Tokens.RB) {
      let        k = tryParseString(s, i)
      if (!k.ok) k = tryParseNumber(s, i)
      if (!k.ok) k = tryParseId    (s, i)
      if (!k.ok) return warn(i, `[ast/tryParseObject] Expected key but received ${maybeRb.kind} instead on line ${maybeRb.where.row}:${maybeRb.where.col}`)
    
      i = k.i
      if (i >= s.length)
        return warn(i, `[ast/tryParseObject] Expected '=' but reached end of file instead`)

      const expectsEq = s[i ++]
      if (expectsEq.kind !== Tokens.EQ)
        return warn(i, `[ast/tryParseObject] Expected '=' but received '${expectsEq.kind}' instead on line ${expectsEq.where.row}:${expectsEq.where.col}`)

      if (i >= s.length)
        return warn(i, `[ast/tryParseObject] Expected value but reached end of file instead`)

      const v = tryParseExpression(s, i)
      if (!v.ok) 
        return warn(i, `[ast/tryParseObject] Expected value but received ${s[i].kind} instead on line ${s[i].where.row}:${s[i].where.col}`)
  
      pairs.push([k.node, v.node])

      i = v.i
      if (i >= s.length)
        return warn(i, `[ast/tryParseObject] Expected ',' or '}' but reached end of file instead`)

      maybeRb = s[i]
      if (maybeRb.kind === Tokens.LS) {
        if (i >= s.length - 1)
          return warn(i, `[ast/tryParseObject] Expected value or '}' but reached end of file instead`)
        maybeRb = s[++ i]
      }
    }
    if (maybeRb.kind !== Tokens.RB)
      return warn(i, `[ast/tryParseObject] Expected '}' but received '${maybeRb.kind}' instead on line ${maybeRb.where.row}:${maybeRb.where.col}`)

    return node(++i, Node.obj(pairs))
  }

  function tryParseVar(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseVar] Expected assignment but reached end of file instead`)

    const expectsId = tryParseId(s, i)
    if (!expectsId.ok)
      return expectsId

    i = expectsId.i
    if (i >= s.length)
      return warn(i, `[ast/tryParseVar] Expected '=' but reached end of file instead`)

    const expectsEq = s[i ++]
    if (expectsEq.kind !== Tokens.EQ)
      return warn(i, `[ast/tryParseVar] Expected '=' but received '${expectsEq.kind}' instead on line ${expectsEq.where.row}:${expectsEq.where.col}`)

    if (i >= s.length)
      return warn(i, `[ast/tryParseVar] Expected expression but reached end of file instead`)

    const expression = tryParseExpression(s, i)
    if (!expression.ok)
      return expression

    i = expression.i

    return node(i, Node.var(expectsId.node, expression.node))
  }

  function tryParseFun(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseFun] Expected function but reached end of file instead`)

    const expectsLp = s[i ++]
    if (expectsLp.kind !== Tokens.LP)
      return warn(i, `[ast/tryParseFun] Expected '(' but received '${expectsLp.kind}' instead on line ${expectsLp.where.row}:${expectsLp.where.col}`)

    if (i >= s.length)
      return warn(i, `[ast/tryParseFun] Expected expression or ')' but reached end of file instead`)

    const parameters = [ ]

    let maybeRp = s[i]

    while (maybeRp.kind !== Tokens.RP) {
      console.log(maybeRp)
      let        p = tryParseVar(s, i)
      console.log(p)
      if (!p.ok) p = tryParseId (s, i)
      if (!p.ok) return p

      parameters.push(p.node)

      i = p.i
      if (i >= s.length)
        return warn(i, `[ast/tryParseFun] Expected ',' or ')' but reached end of file instead`)

      maybeRp = s[i]
      if (maybeRp.kind === Tokens.LS) {
        if (i >= s.length - 1)
          return warn(i, `[ast/tryParseFun] Expected parameter or ')' but reached end of file instead`)
        maybeRp = s[++ i]
      }
    }

    if (maybeRp.kind !== Tokens.RP)
      return warn(i, `[ast/tryParseFun] Expected ')' but received '${maybeRp.kind}' instead on line ${maybeRp.where.row}:${maybeRp.where.col}`)
  
    if (i >= s.length - 1)
      return warn(i, `[ast/tryParseFun] Expected '{' but reached end of file instead`)
  
    const expectsLb = s[++ i]
    if (expectsLb.kind !== Tokens.LB)
      return warn(i, `[ast/tryParseFun] Expected '{' but received '${expectsLb.kind}' instead on line ${expectsLb.where.row}:${expectsLb.where.col}`)

    if (i >= s.length - 1)
      return warn(i, `[ast/tryParseFun] Expected statement or '}' but reached end of file instead`)

    const statements = [ ]

    let maybeRb = s[++ i]
    while (maybeRb.kind !== Tokens.RB) {
      const maybeStatement = tryParseStatement(s, i)
      if (!maybeStatement.ok) 
        return maybeStatement

      statements.push(maybeStatement.node)

      i = maybeStatement.i
      if (i >= s.length)
        return warn(i, `[ast/tryParseFun] Expected statement or '}' but reached end of file instead`)
      
      maybeRb = s[i]
    }

    if (maybeRb.kind !== Tokens.RB)
      return warn(i, `[ast/tryParseFun] Expected '}' but received '${maybeRb.kind}' instead on line ${maybeRb.where.row}:${maybeRb.where.col}`)

    return node(++i, Node.fun(parameters, statements))
  }

  function tryParseRun(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseRun] Expected dispatch but reached end of file instead`)

    let definition = tryParseId (s, i);
    if (!definition.ok)
        definition = tryParseFun(s, i);
    if (!definition.ok)
        return definition;

    i = definition.i
    if (i >= s.length)
      return warn(i, `[ast/tryParseRun] Expected '(' but reached end of file instead`)

    const expectsLp = s[i ++]
    if (expectsLp.kind !== Tokens.LP)
      return warn(i, `[ast/tryParseRun] Expected '(' but received '${expectsLp.kind}' instead on line ${expectsLp.where.row}:${expectsLp.where.col}`)

    if (i >= s.length)
      return warn(i, `[ast/tryParseRun] Expected expression or ')' but reached end of file instead`)

    const arguments_ = [ ]

    let maybeRp = s[i]
    while (maybeRp.kind !== Tokens.RP) {
      const maybeExpression = tryParseExpression(s, i)
      if (!maybeExpression.ok) 
        return maybeExpression

      arguments_.push(maybeExpression.node)

      i = maybeExpression.i
      if (i >= s.length)
        return warn(i, `[ast/tryParseRun] Expected ',' or ')' but reached end of file instead`)

      maybeRp = s[i]
      if (maybeRp.kind === Tokens.LS) {
        if (i >= s.length)
          return warn(i, `[ast/tryParseRun] Expected expression or ')' but reached end of file instead`)
        maybeRp = s[++ i]
      }
    }

    if (maybeRp.kind !== Tokens.RP)
      return warn(i, `[ast/tryParseRun] Expected ')' but received '${maybeRp.kind}' instead on line ${maybeRp.where.row}:${maybeRp.where.col}`)

    return node(++i, Node.run(definition.node, arguments_))
  }

  function tryParseExpression(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseExpression] Expected expression but reached end of file instead`)

    if (s[i].kind === Tokens.ID) {
      const maybeVar = tryParseVar(s, i)
      if (maybeVar.ok)
        return maybeVar

      const maybeRun = tryParseRun(s, i)
      if (maybeRun.ok)
        return maybeRun

      const maybeId  = tryParseId (s, i)
      if (maybeId .ok)
        return maybeId
    }
    else if (s[i].kind === Tokens.NUM)
      return tryParseNumber(s, i)
    else if (s[i].kind === Tokens.STR)
      return tryParseString(s, i)
    else if (s[i].kind === Tokens.LB )
      return tryParseObject(s, i)
    else if (s[i].kind === Tokens.LP ) {
      const maybeRun = tryParseRun(s, i)
      if (maybeRun.ok)
        return maybeRun

      const maybeFun = tryParseFun(s, i)
      if (maybeFun.ok)
        return maybeFun
    }

    return warn(i, `[ast/tryParseExpression] Expected expression but received '${s[i].kind}' instead on line ${s[i].where.row}:${s[i].where.col}`)
  }

  function tryParseStatement(s, i) {
    if (i >= s.length)
      return warn(i, `[ast/tryParseStatement] Expected statement but reached end of file instead`)

    // skip end tokens
    let peek = s[i]
    while (
      i < s.length && (
      peek.kind === Tokens.SC ||
      peek.kind === Tokens.NL
    )) peek = s[++ i]

    if (i >= s.length)
      return node(i, Node.noop())

    const maybeExpression = tryParseExpression(s, i)
    if (!maybeExpression.ok) 
      return maybeExpression

    i = maybeExpression.i
    if (i >= s.length)
      return maybeExpression

    let maybeSc = s[i]
    while (i < s.length && (
      maybeSc.kind === Tokens.SC ||
      maybeSc.kind === Tokens.NL
    )) maybeSc = s[++ i]

    return node(i, maybeExpression.node)
  }

  const statements = [ ]

  let i = 0
  while (i < s.length) {
    const statement = tryParseStatement(s, i)
    if (!statement.ok) 
      throw new Error(`[ast] ${statement.warn}`)
    statements.push(statement.node)
    i = statement.i
  } 

  return statements
}



function *run(statements) {
  const scope = { }

  

  function tryEvalNoop(scope, node) {
    if (node.kind !== Nodes.NOOP)
      throw new Error(`[run] Expected noop but received '${node.kind}' instead`)


  }

  function tryEvalNumber(scope, node) {
    if (node.kind !== Nodes.NUM)
      throw new Error(`[run] Expected number but received '${node.kind}' instead`)

    return node.value
  }

  function tryEvalString(scope, node) {
    if (node.kind !== Nodes.STR)
      throw new Error(`[run] Expected string but received '${node.kind}' instead`)
    
    return node.value
  }

  function tryEvalId(scope, node) {
    if (node.kind !== Nodes.ID)
      throw new Error(`[run] Expected id but received '${node.kind}' instead`)
    
    const value = scope[node.identifier]
    if (value === undefined)
      throw new Error(`[run] Id '${node.identifier}' not found in current scope`)
    
    return value
  }

  function tryEvalVar(scope, node) {
    if (node.kind !== Nodes.VAR)
      throw new Error(`[run] Expected var but received '${node.kind}' instead`)
    
    const id    = node.identifier.value
    const value = tryEvalExpression(scope, node.expression)

    return scope[id] = value
  }

  function tryEvalRun(scope, node) {
    if (node.kind !== Nodes.RUN)
      throw new Error(`[run] Expected run but received '${node.kind}' instead`)

    let definition = node.definition
    if (definition.kind === Nodes.ID)
      definition = tryEvalId(scope, definition)
  }
}