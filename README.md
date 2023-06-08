# 🐒 The Monkey programming language

## 📝 Description

`monkey` is an interpreted programming language, written in Rust. This is my implementation of the book ["Writing An Interpreter In Go", by Thorsten Ball](https://edu.anarcho-copy.org/Programming%20Languages/Go/writing%20an%20INTERPRETER%20in%20go.pdf).

### Why the name "Monkey"?

As stated in Thorsten Ball's "Writing an Interpreter in Go":

> But why the name? Why is it called “Monkey”? Well, because monkeys are magnificent, elegant, fascinating and funny creatures. Exactly like our interpreter.

## 👨‍💻 Running the interpreter

```sh
# Navigate to the project directory
cd monkey

# Run tests
cargo test

# Run the REPL
cargo run
```

## 🚀 The journey

### Chapter 1 - Lexical analysis

Lexical analysis focuses on lexing, the process of converting raw string input to a stream of tokens. A token represents a type and the literal value. As an example, the string `fn` would be converted to a token with the type `Function` and the literal value `fn`. In Rust, `enum`s are a good fit for representing tokens. They can even hold data, which is perfect for the literal value! The `Token` enum is defined in [`src/lexer.rs`](/src/lexer.rs).

The lexer is really straightforward. Some things to note:

- Let's say we encounter `!`, but we don't know if it's a `Bang` or a `NotEqual`. We can't know until we see the next character. So we peek at the next character, and if it's `=`, we know it's a `NotEqual` token. Otherwise, it's a `Bang` token.
- In the `is_letter()` method, we can customize which characters are considered letters. I chose to allow underscores, so that we can have identifiers like `my_var` or `_unused`.

The tests for the lexer are only concerned with the lexical validity of the input, not the correctness of the tokens. For example, the input `let 5 = 10;` is lexically valid, but semantically invalid. The lexer doesn't care about that, it just checks if the input is lexically valid. I also created a small custom macro to reduce some boilerplate in the tests - neat.

This chapter finished with a very simple REPL, which prompts the user for input, lexes it, and prints the tokens. It's not very useful, but it's a good starting point for the next chapter.

### Chapter 2 - Parsing

Parsing focuses on parsing the stream of tokens into an abstract syntax tree (AST). The AST is a tree representation of the source code. It's a way to represent the source code in a way that's easier to work with. The AST is defined in [`src/ast.rs`](/src/ast.rs), and the parser is defined in [`src/parser.rs`](/src/parser.rs).

This task of parsing was much larger than the lexical analysis, and much more challenging.

#### The abstract syntax tree (AST)

I learnt a lot about traits in Rust, especially when defining the abstract syntax tree. Traits in Rust are equivalent to interfaces in other languages. They allow us to define a set of methods that a type must implement. The only reason nodes in the AST need to implement `as_any()` is because we need to downcast them in the parser tests. Below follows an overview of the type of nodes in the AST:

- **Statements**
  - `LetStatement`
  - `ReturnStatement`
  - `ExpressionStatement`, which is just a wrapper around an `Expression`
  - `BlockStatement`
- **Expressions**
  - `Identifier`
  - `IntegerLiteral`
  - `PrefixExpression`
  - `InfixExpression`
  - `Boolean`
  - `IfExpression`
  - `FunctionLiteral`
  - `CallExpression`

#### The parser

The parser is responsible for taking the lexically analyzed input and creating an abstract syntax tree in order to give structure to the input. The parser also does the syntactical analysis, validating if the structure of the input is correct. It's a recursive descent parser, which means it's a top-down parser with one lookahead token. The parser is defined in [`src/parser.rs`](/src/parser.rs). In order to facilitate good error messages from my interpreter, I created a custom `ParserError`. This can be extended later on to include more information about the error, such as the line number, column number, and filename. The most challenging part of the parser was implementing the recursive parsing of expressions. I had to think carefully about the precedence of operators, and how to handle them. This was solved using a precedence table, specifically using a hashmap.

I wrote extensive tests for the parser, which was very helpful. I also created several custom macros to reduce some boilerplate in the tests. I found writing tests in a test-first manner to be very helpful. I would write a test, then implement the functionality to make the test pass. This was a very iterative process, and I found myself refactoring the parser a lot.

#### Extending the REPL

After fully implementing the parser for the abstract syntax tree, I extended the REPL to now display the parsed AST. Earlier, only the lexical information was displayed back to the user when interacting with the REPL. Now, the parsed AST is displayed back to the user. This is a huge step forward.

---

> TODO: Jot down some notes for each completed chapter.

## 💡 Future ideas

- [ ] also attach the line number, column number and filename to a token. Why? For example, to later output more useful error messages in the parsing stage. Instead of "error: expected semicolon token" it can output with line numbers etc.
- [ ] Don't treat all integers as `i64`. Based on size, allocate only the type needed.
- [ ] Support floating point numbers.
- [ ] Flesh out the `Error` in the parser to contain more information about what kind of error occurred. That way, the user gets a much better compile time error.
- [ ] Remove uneccessary empty method in the AST for `Expression`, `Statement`, etc.
- [ ] Print colored and nice output to terminal when using the REPL. Thinking especially of the error messages.
