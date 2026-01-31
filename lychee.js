const Token = {
  ID        : "id",
  EQUAL     : "eq",
  COMMA     : "ma",
  SEMICOLON : "sc",
  L_PAREN   : "lp",
  R_PAREN   : "rp",
  L_BRACE   : "lb",
  R_BRACE   : "rb",
  NUMBER    : "num",
  STRING    : "str",

  new(kind, value, row, col) {
    return { kind, value, row, col }
  }
}

function *lex(s) {
  let i   = 0
  let row = 1
  let col = 1

  function eof() {
    return i >= s.length
  }
  
  function peek() {
    return s[i]
  }

  function read() {
    let c = s[i]
    i += 1
    if (c === "\n") {
      row += 1
      col  = 1
    } else {
      col += 1
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
    let _row = row
    let _col = col
    let c = read()

    if (isSpace(c)) continue
    else if (c === "=") yield Token.new(Token.EQUAL    , c, _row, _col)
    else if (c === ",") yield Token.new(Token.COMMA    , c, _row, _col)
    else if (c === "(") yield Token.new(Token.L_PAREN  , c, _row, _col)
    else if (c === ")") yield Token.new(Token.R_PAREN  , c, _row, _col)
    else if (c === "{") yield Token.new(Token.L_BRACE  , c, _row, _col)
    else if (c === "}") yield Token.new(Token.R_BRACE  , c, _row, _col)
    else if (c === ";") yield Token.new(Token.SEMICOLON, c, _row, _col)
    else if (isDigit(c)) {
      let value = c
      while (!eof() && isDigit(peek()))
        value += read()
      if (!eof() && peek() === ".")
        value += read()
      while (!eof() && isDigit(peek()))
        value += read()
      yield Token.new(Token.NUMBER, value, _row, _col)
    } else if (c === '"') {
      let value = c
      while (!eof() && peek() !== '"')
        value += read()
      if (!eof() && peek() === '"')
        value += read()
      yield Token.new(Token.STRING, value, _row, _col)
    } else if (isAlpha(c)) {
      let value = c
      while (!eof() && isAlpha(peek()))
        value += read()
      yield Token.new(Token.ID, value, _row, _col)
    } else if (c === "#") {
      while (!eof() && peek() !== "\n")
        read()
    } else throw new Error(`[lex] Unexpected character '${c}' on line ${_row}:${_col}`)
  }
}

function ast(tokens) {

  const Number = {
    new(value) {
      return { is: "number", value }
    }
  }

  const String = {
    new(value) {
      return { is: "string", value }
    }
  }

  const Identifier = {
    new(value) {
      return { is: "identifier", value }
    }
  }

  const Assignment = {
    new(lhs, rhs) {
      return { is: "assignment", lhs, rhs }
    }
  }

  const Definition = {
    new(parameters, statements) {
      return { is: "definition", parameters, statements }
    }
  }

  const Dispatch = {
    new(expression, arguments_) {
      return { is: "dispatch", expression, arguments: arguments_ }
    }
  }

  function tryParseIdentifier(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected identifier but reached end of file instead` }

    const expectsId = tokens.shift()
    if (expectsId.kind !== Token.ID   ) 
      return { ok: false, warn: `Expected identifier but received '${expectsId.kind}' instead` }

    return { ok: true, node: Identifier.new(expectsId.value), tokens }
  }

  /** @param {Array<Token>} tokens */
  function tryParseNumber(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected number but reached end of file instead` }

    const expectsNumber = tokens.shift()
    if (expectsNumber.kind !== Token.NUMBER)
      return { ok: false, token: expectsNumber, warn: `Expected number but received '${expectsNumber.kind}' instead` }

    return { ok: true, node: Number.new(expectsNumber.value), tokens }
  }

  /** @param {Array<Token>} tokens */
  function tryParseString(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected string but reached end of file instead` }

    const expectsString = tokens.shift()
    if (expectsString.kind !== Token.STRING) 
      return { ok: false, token: expectsString, warn: `Expected string but received '${expectsString.kind}' instead` }

    return { ok: true, node: String.new(expectsString.value), tokens }
  }

  /** @param {Array<Token>} tokens */
  function tryParseAssignment(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected id but reached end of file instead` }

    const maybeIdentifier = tryParseIdentifier(tokens)
    if (!maybeIdentifier.ok) 
      return maybeIdentifier

    tokens = maybeIdentifier.tokens
    if (tokens.length === 0)
      return { ok: false, warn: `Expected '=' but reached end of file instead` }

    const expectsEq = tokens.shift()
    if (expectsEq.kind !== Token.EQUAL) 
      return { ok: false, warn: `Expected '=' but received '${expectsEq.kind}' instead` }

    const maybeExpression = tryParseExpression(tokens)
    if (!maybeExpression.ok) 
      return maybeExpression

    return { ok: true, node: Assignment.new(maybeIdentifier.node, maybeExpression.node), tokens: maybeExpression.tokens }
  } 

  function tryParseDispatch(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected id but reached end of file instead` }

    const peekOnce = tokens.shift()

    let lhs

    if (peekOnce.kind === Token.ID) {
      lhs = tryParseIdentifier([peekOnce, ...tokens])
      if (!lhs.ok) 
        return lhs
    } else  if (peekOnce.kind === Token.L_PAREN) {
      lhs = tryParseDefinition([peekOnce, ...tokens])
      if (!lhs.ok) 
        return lhs
    }

    tokens = lhs.tokens
    if (tokens.length === 0)
      return { ok: false, warn: `Expected '=' but reached end of file instead` }

    const expectsLp = tokens.shift()
    if (expectsLp.kind !== Token.L_PAREN) 
      return { ok: false, warn: `Expected '(' but received '${expectsLp.kind}' instead` }

    /** @type {Array<Expression>} */
    const arguments_ = [ ]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected expression or ')' but reached end of file instead` }

    let maybeRp = tokens.shift()
    while (maybeRp.kind !== Token.R_PAREN) {
      const maybeExpression = tryParseExpression([maybeRp, ...tokens])
      if (!maybeExpression.ok) 
        return maybeExpression

      arguments_.push(maybeExpression.node)

      tokens = maybeExpression.tokens
      if (tokens.length === 0)
        return { ok: false, warn: `Expected expression or ')' but reached end of file instead` }

      maybeRp = tokens.shift()
      if (maybeRp.kind === Token.COMMA) {
        if (tokens.length === 0)
          return { ok: false, warn: `Expected expression or ')' but reached end of file instead` }
        maybeRp = tokens.shift()
      } else
        if (maybeRp.kind !== Token.R_PAREN)
          return { ok: false, warn: `Expected expression or ')' but received '${maybeRp.kind}' instead` }
    }

    if (maybeRp.kind !== Token.R_PAREN)
      return { ok: false, token: maybeRp, warn: `Expected ')' but received '${maybeRp.kind}' instead` }

    return { ok: true, node: Dispatch.new(lhs.node, arguments_), tokens }
  } 

  /** @param {Array<Token>} tokens */
  function tryParseDefinition(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected '(' but reached end of file instead` }

    const expectsLp = tokens.shift()    
    if (expectsLp.kind !== Token.L_PAREN) 
       return { ok: false, warn: `Expected '(' but received '${expectsLp.kind}' instead` }

    /** @type {Array<string>} */
    const parameters = [ ]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected id or ')' but reached end of file instead` }

    let maybeRp = tokens.shift()
    while (maybeRp.kind !== Token.R_PAREN) {
      const maybeIdentifier = tryParseIdentifier([maybeRp, ...tokens])
      if (!maybeIdentifier.ok) 
        return maybeIdentifier

      parameters.push(maybeIdentifier.node)

      tokens = maybeIdentifier.tokens
      if (tokens.length === 0)
        return { ok: false, warn: `Expected ',' or ')' but reached end of file instead` }

      maybeRp = tokens.shift()
      if (maybeRp.kind === Token.COMMA  ) {
        if (tokens.length === 0)
          return { ok: false, warn: `Expected identifier or ')' but reached end of file instead` }
        maybeRp = tokens.shift()
      }
    }

    if (maybeRp.kind !== Token.R_PAREN)
      return { ok: false, token: maybeRp, warn: `Expected ')' but received '${maybeRp.kind}' instead` }

    if (tokens.length === 0)
      return { ok: false, warn: `Expected '{' but reached end of file instead` }

    const expectsLb = tokens.shift()
    if (expectsLb.kind !== Token.L_BRACE) 
      return { ok: false, warn: `Expected '{' but received '${expectsLb.kind}' instead` }

    /** @type {Array<Statement>} */
    const statements = [ ]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected statement or '}' but reached end of file instead` }

    let maybeRb = tokens.shift()
    while (maybeRb.kind !== Token.R_BRACE) {
      const maybeStatement = tryParseStatement([maybeRb, ...tokens])
      if (!maybeStatement.ok) 
        return maybeStatement

      statements.push(maybeStatement.node)

      tokens = maybeStatement.tokens
      if (tokens.length === 0)
        return { ok: false, warn: `Expected statement or '}' but reached end of file instead` }

      maybeRb = tokens.shift()
    }

    if (maybeRb.kind !== Token.R_BRACE)
      return { ok: false, token: maybeRb, warn: `Expected '}' but received '${maybeRb.kind}' instead` }

    return { ok: true, node: Definition.new(parameters, statements), tokens }
  }
  
  function tryParseExpression(tokens) {
    tokens = [...tokens]

    if (tokens.length === 0)
      return { ok: false, warn: `Expected expression but reached end of file instead` }

    const peekOnce = tokens.shift()

    if (peekOnce.kind === Token.ID) {
      if (tokens.length === 0)
        return { ok: false, token: peekOnce, warn: `Expected expression but reached end of file instead` }

      const peekTwice = tokens.shift()
           if (peekTwice.kind === Token.EQUAL  )
        return tryParseAssignment  ([peekOnce, peekTwice, ...tokens])
      else if (peekTwice.kind === Token.L_PAREN)
        return tryParseDispatch  ([peekOnce, peekTwice, ...tokens])
      else
        return tryParseIdentifier([peekOnce, peekTwice, ...tokens])

      return { ok: false, token: peekTwice, warn: `Expected expression but received '${peekTwice.kind}' instead` }
    }

    else if (peekOnce.kind === Token.L_PAREN) {
      const maybeDispatch = tryParseDispatch  ([peekOnce, ...tokens])
      if (!maybeDispatch.ok)
        return tryParseDefinition([peekOnce, ...tokens])
      return maybeDispatch
    }

    else if (peekOnce.kind === Token.NUMBER) {
      return tryParseNumber([peekOnce, ...tokens])
    }

    else if (peekOnce.kind === Token.STRING) {
      return tryParseString([peekOnce, ...tokens])
    }

    return { ok: false, token: peekOnce, warn: `Expected expression but received '${peekOnce.kind}' instead` }
  }

  function tryParseStatement (tokens) {
    tokens = [...tokens]

    const maybeExpression = tryParseExpression(tokens)
    if (!maybeExpression.ok) 
      return maybeExpression

    tokens = maybeExpression.tokens
    if (tokens.length === 0)
      return { ok: false, warn: `Expected ';' but reached end of file instead` }

    const expectsSc = tokens.shift()
    if (expectsSc.kind !== Token.SEMICOLON) 
      return { ok: false, token: expectsSc, warn: `Expected ';' but received '${expectsSc.kind}' instead` }

    return { ok: true, node: maybeExpression.node, tokens }
  }

  const statements = [ ]

  while (tokens.length > 0) {
    const maybeStatement = tryParseStatement(tokens)
    if (!maybeStatement.ok) 
      throw new Error(`[ast] ${maybeStatement.warn} at ${maybeStatement.token?.row}:${maybeStatement.token?.col}`)
    statements.push(maybeStatement.node)
    tokens = maybeStatement.tokens
  }

  return statements
}