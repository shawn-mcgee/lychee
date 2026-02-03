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

  NUM: "token:num",
  STR: "token:str",

  END: "token:end",
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

  num(value, where) {
    return Token.new(Tokens.NUM, value, where)
  },
 
  str(value, where) {
    return Token.new(Tokens.STR, value, where)
  },

  end(value, where) {
    return Token.new(Tokens.END, value, where)
  },
}

function *lex(string) {
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
      c === "\n" ||
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
    if (isSpace(c)) continue
    // read symbols
    else if (c === "=") yield Token.eq(j);
    else if (c === ",") yield Token.ls(j);
    else if (c === "(") yield Token.lp(j);
    else if (c === ")") yield Token.rp(j);
    else if (c === "{") yield Token.lb(j);
    else if (c === "}") yield Token.rb(j);
    else if (c === "[") yield Token.lk(j);
    else if (c === "]") yield Token.rk(j);
    else if (c === ";") yield Token.sc(j);
    // read number
    else if (isDigit(c) || c === "-" || c === "+") {
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
      let value = c
      while (!eof() && peek() !== '"')
        value += read()
      if (!eof() && peek() === '"')
        value += read()
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
}

const Node = {
  id(identifier) {
    return { kind: Nodes.ID, identifier }
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
  }
}

function ast(s) {

  function node(s, node) {
    return { ok: true , node, s }
  }

  function warn(s, warn) {
    return { ok: false, warn, s }
  }

  function tryParseId(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseId] Expected id but reached end of file instead`)

    const expectsId = s.shift()
    if (expectsId.kind !== Tokens.ID)
      return warn([expectsId, ...s], `[ast/tryParseId] Expected id but received '${expectsId.kind}' instead at ${expectsId.where?.row}:${expectsId.where?.col}`)

    return node(s, Node.id(expectsId.value))
  }

  /** @param {Array<Token>} s */
  function tryParseNumber(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseNumber] Expected number but reached end of file instead`)
      
    const expectsNum = s.shift()
    if (expectsNum.kind !== Tokens.NUM)
      return warn([expectsNum, ...s], `[ast/tryParseNumber] Expected number but received '${expectsNum.kind}' instead at ${expectsNum.where?.row}:${expectsNum.where?.col}`)

    return node(s, Node.num(expectsNum.value))
  }

  /** @param {Array<Token>} s */
  function tryParseString(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseString] Expected string but reached end of file instead`)
  
    const expectsStr = s.shift()
    if (expectsStr.kind !== Tokens.STR)
      return warn([expectsStr, ...s], `[ast/tryParseString] Expected string but received '${expectsStr.kind}' instead at ${expectsStr.where?.row}:${expectsStr.where?.col}`)

    return node(s, Node.str(expectsStr.value))
  }

  /** @param {Array<Token>} s */
  function tryParseObject(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseObject] Expected object but reached end of file instead`)

    const expectsLb = s.shift()
    if (expectsLb.kind !== Tokens.LB)
      return warn([expectsLb, ...s], `[ast/tryParseObject] Expected '{' but received '${expectsLb.kind}' instead at ${expectsLb.where?.row}:${expectsLb.where?.col}`)

    if (s.length === 0)
      return warn(s, `[ast/tryParseObject] Expected pair or '}' but reached end of file instead`)

    const pairs = [ ]

    let maybeRb = s.shift()
    while (maybeRb.kind !== Tokens.RB) {
      let key = tryParseString([maybeRb, ...s])
      if (!key.ok)
          key = tryParseNumber([maybeRb, ...s])
      if (!key.ok)
          key = tryParseId    ([maybeRb, ...s])
      if (!key.ok)
        return warn(s, `[ast/tryParseObject] Expected key`)

      s = key.s
      if (s.length === 0)
        return warn(s, `[ast/tryParseObject] Expected ':' but reached end of file instead`)

      const expectsEq = s.shift()
      if (expectsEq.kind !== Tokens.EQ)
        return warn([expectsEq, ...s], `[ast/tryParseObject] Expected '=' but received '${expectsEq.kind}' instead at ${expectsEq.where?.row}:${expectsEq.where?.col}`)

      let val = tryParseExp(s)
      if (!val.ok)
        return warn(s, `[ast/tryParseObject] Expected value`)

      pairs.push([key.node, val.node])

      s = val.s
      if (s.length === 0)
        return warn(s, `[ast/tryParseObject] Expected ',' or '}' but reached end of file instead`)
    
      maybeRb = s.shift()
      if (maybeRb.kind === Tokens.LS) {
        if (s.length === 0)
          return warn(s, `[ast/tryParseObject] Expected value or '}' but reached end of file instead`)
        maybeRb = s.shift()
      }
    }

    if (maybeRb.kind !== Tokens.RB)
      return warn([maybeRb, ...s], `[ast/tryParseObject] Expected '}' but received '${maybeRb.kind}' instead at ${maybeRb.where?.row}:${maybeRb.where?.col}`)

    return node(s, Node.obj(pairs))
  }

  function tryParseVar(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseVar] Expected assignment but reached end of file instead`)

    const expectsId = tryParseId (s)
    if (!expectsId.ok)
      return expectsId

    s = expectsId .s

    const expectsEq = s.shift()
    if (expectsEq.kind !== Tokens.EQ)
      return warn([expectsEq, ...s], `[ast/tryParseVar] Expected '=' but received '${expectsEq.kind}' instead at ${expectsEq.where?.row}:${expectsEq.where?.col}`)

    const expectsExp = tryParseExp(s)
    if (!expectsExp.ok) 
      return expectsExp

    s = expectsExp.s
    
    return node(s, Node.var(expectsId.node, expectsExp.node))
  }

  function tryParseFun(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseFun] Expected function but reached end of file instead`)
  
    const expectsLp = s.shift()
    if (expectsLp.kind !== Tokens.LP)
      return warn([expectsLp, ...s], `[ast/tryParseFun] Expected '(' but received '${expectsLp.kind}' instead at ${expectsLp.where?.row}:${expectsLp.where?.col}`)
  
    if (s.length === 0)
      return warn(s, `[ast/tryParseFun] Expected expression or ')' but reached end of file instead`)
  
    const parameters = [ ]

    let maybeRp = s.shift()
    while (maybeRp.kind !== Tokens.RP) {
      let parameter = tryParseVar([maybeRp, ...s])
      if (!parameter.ok)
          parameter = tryParseId ([maybeRp, ...s])
      if (!parameter.ok)
        return warn(s, `[ast/tryParseFun] Expected parameter or ')'`)

      parameters.push(parameter.node)

      s = parameter.s
      if (s.length === 0)
        return warn(s, `[ast/tryParseFun] Expected ',' or ')' but reached end of file instead`)

      maybeRp = s.shift()
      if (maybeRp.kind === Tokens.LS) {
        if (s.length === 0)
          return warn(s, `[ast/tryParseFun] Expected parameter or ')' but reached end of file instead`)
        maybeRp = s.shift()
      }
    }

    if (maybeRp.kind !== Tokens.RP)
      return warn([maybeRp, ...s], `[ast/tryParseFun] Expected ')' but received '${maybeRp.kind}' instead at ${maybeRp.where?.row}:${maybeRp.where?.col}`)
  
    const expectsLb = s.shift()
    if (expectsLb.kind !== Tokens.LB)
      return warn([expectsLb, ...s], `[ast/tryParseFun] Expected '{' but received '${expectsLb.kind}' instead at ${expectsLb.where?.row}:${expectsLb.where?.col}`)

    if (s.length === 0)
      return warn(s, `[ast/tryParseFun] Expected statement or '}' but reached end of file instead`)

    const statements = [ ]

    let maybeRb = s.shift()
    while (maybeRb.kind !== Tokens.RB) {
      const maybeStatement = tryParseStatement([maybeRb, ...s])
      if (!maybeStatement.ok) 
        return maybeStatement
      statements.push(maybeStatement.node)

      s = maybeStatement.s
      if (s.length === 0)
        return warn(s, `[ast/tryParseFun] Expected statement or '}' but reached end of file instead`)
      
      maybeRb = s.shift()
    }

    if (maybeRb.kind !== Tokens.RB)
      return warn([maybeRb, ...s], `[ast/tryParseFun] Expected '}' but received '${maybeRb.kind}' instead at ${maybeRb.where?.row}:${maybeRb.where?.col}`)

    return node(s, Node.fun(parameters, statements))
  }

  function tryParseRun(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseRun] Expected dispatch but reached end of file instead`)

    let definition = tryParseId (s);
    if (!definition.ok)
        definition = tryParseFun(s);
    if (!definition.ok)
      return definition;

    s = definition.s
    if (s.length === 0)
      return warn(s, `[ast/tryParseRun] Expected '(' but reached end of file instead`)

    const expectsLp = s.shift()
    if (expectsLp.kind !== Tokens.LP)
      return warn([expectsLp, ...s], `[ast/tryParseRun] Expected '(' but received '${expectsLp.kind}' instead at ${expectsLp.where?.row}:${expectsLp.where?.col}`)

    if (s.length === 0)
      return warn(s, `[ast/tryParseRun] Expected expression or ')' but reached end of file instead`)

    const arguments_ = [ ]

    let maybeRp = s.shift()
    while (maybeRp.kind !== Tokens.RP) {
      const maybeExp = tryParseExp([maybeRp, ...s])
      if (!maybeExp.ok) 
        return maybeExp

      arguments_.push(maybeExp.node)

      s = maybeExp.s
      if (s.length === 0)
        return warn(s, `[ast/tryParseRun] Expected ',' or ')' but reached end of file instead`)

      maybeRp = s.shift()
      if (maybeRp.kind === Tokens.LS) {
        if (s.length === 0)
          return warn(s, `[ast/tryParseRun] Expected expression or ')' but reached end of file instead`)
        maybeRp = s.shift()
      }
    }

    if (maybeRp.kind !== Tokens.RP)
      return warn([maybeRp, ...s], `[ast/tryParseRun] Expected ')' but received '${maybeRp.kind}' instead at ${maybeRp.where?.row}:${maybeRp.where?.col}`)

    return node(s, Node.run(definition.node, arguments_))
  }

  function tryParseExp(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseExp] Expected expression but reached end of file instead`)

    // skip end tokens
    let peek = s.shift()
    while (peek?.kind === Tokens.SC)
      peek = s.shift()

    if (s.length === 0)
        return warn(s, `[ast/tryParseExp] Expected expression but reached end of file instead`)

    if (peek.kind === Tokens.ID) {
      const maybeVar = tryParseVar([peek, ...s])
      if (maybeVar.ok)
        return maybeVar

      const maybeRun = tryParseRun([peek, ...s])
      if (maybeRun.ok)
        return maybeRun

      return tryParseId([peek, ...s])
    }

    else if (peek.kind === Tokens.NUM)
      return tryParseNumber([peek, ...s])
    else if (peek.kind === Tokens.STR)
      return tryParseString([peek, ...s])
    else if (peek.kind === Tokens.LB )
      return tryParseObject([peek, ...s])
    else if (peek.kind === Tokens.LP ) {
      const maybeRun = tryParseRun([peek, ...s])
      if (maybeRun.ok)
        return maybeRun

      const maybeFun = tryParseFun([peek, ...s])
      if (maybeFun.ok)
        return maybeFun
    }

    return warn(s, `[ast/tryParseExp] Expected expression but received '${peek.kind}' instead at ${peek.where?.row}:${peek.where?.col}`)
  }

  function tryParseStatement(s) {
    s = [...s]
    if (s.length === 0)
      return warn(s, `[ast/tryParseStatement] Expected statement but reached end of file instead`)

    const maybeExp = tryParseExp(s)
    if (!maybeExp.ok) 
      return maybeExp

    s = maybeExp.s
    if (s.length === 0)
      return warn(s, `[ast/tryParseStatement] Expected ';' but reached end of file instead`)

    const maybeSc = s.shift()
    if (maybeSc.kind !== Tokens.SC)
      return warn([maybeSc, ...s], `[ast/tryParseStatement] Expected ';' but received '${maybeSc.kind}' instead at ${maybeSc.where?.row}:${maybeSc.where?.col}`)

    return maybeExp
  }

  const statements = [ ]

  while(s.length > 0) {
    const maybeStatement = tryParseStatement(s)
    if (!maybeStatement.ok) 
      throw new Error(`[ast] ${maybeStatement.warn}`)
    statements.push(maybeStatement.node)
    s = maybeStatement.s
  }

  return statements
}