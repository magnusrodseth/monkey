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
  - `Let`
  - `Return`
  - `ExpressionStatement`, which is just a wrapper around an `Expression`
  - `BlockStatement`
  - `Empty`
- **Expressions**
  - `Identifier`
  - `Literal`
    - `Integer`
    - `String`
    - `Boolean`
  - `Prefix`
    - `+`, `-`, `!`
  - `Infix`
    - `+`, `-`, `*`, `/`, `==`, `!=`, `<`, `>`, `<=`, `>=`
  - `Boolean`
  - `If`
  - `Function`
  - `Call`

#### The parser

The parser is responsible for taking the lexically analyzed input and creating an abstract syntax tree in order to give structure to the input. The parser also does the syntactical analysis, validating if the structure of the input is correct. It's a recursive descent parser, which means it's a top-down parser with one lookahead token. The parser is defined in [`src/parser.rs`](/src/parser.rs). In order to facilitate good error messages from my interpreter, I created a custom `ParserError`. This can be extended later on to include more information about the error, such as the line number, column number, and filename. The most challenging part of the parser was implementing the recursive parsing of expressions. I had to think carefully about the precedence of operators, and how to handle them. This was solved using a precedence table, specifically using a hashmap.

I wrote extensive tests for the parser, which was very helpful. I also created several custom macros to reduce some boilerplate in the tests. I found writing tests in a test-first manner to be very helpful. I would write a test, then implement the functionality to make the test pass. This was a very iterative process, and I found myself refactoring the parser a lot.

#### Extending the REPL

After fully implementing the parser for the abstract syntax tree, I extended the REPL to now display the parsed AST. Earlier, only the lexical information was displayed back to the user when interacting with the REPL. Now, the parsed AST is displayed back to the user. This is a huge step forward.

### Chapter 3 - Evaluation

Evaluation focuses on evaluating the AST, and producing a result. The evaluator is defined in [`src/evaluator.rs`](/src/evaluator.rs). **This is where code becomes meaningful**. Other relevant files for this section is the [`environment.rs`](/src/environment.rs), which is responsible for keeping track of variable scopes, and [`object.rs`](/src/object.rs), which defines the `Object` enum. The `Object` enum is used to represent the result of evaluating an expression. It's a very simple enum, with variants for `Integer`, `Boolean`, `Null`, `ReturnValue`, `Error`, `Function`, and `Null`.

#### Representing objects

The `Object` enum is used to represent the result of evaluating an expression. It's a very simple enum, with variants for `Integer`, `Boolean`, `Null`, `ReturnValue`, `Error`, `Function`, and `Null`. It is defined in [`src/object.rs`](/src/object.rs), and used in the [`evaluator.rs`](/src/evaluator.rs).

#### Managing variable scopes

The `Environment` struct is responsible for keeping track of variable scopes. It's defined in [`src/environment.rs`](/src/environment.rs). In Monkey, functions and closures are higher order functions, which means they can capture variables from the surrounding environment. This is implemented (in practise) using a linked list of variable scopes; each scope has a pointer to the outer scope. When a variable is looked up, the current scope is searched first, then the outer scope, and so on. In short, **the outer scope encloses the inner scope, and the inner scope extends the outer scope**.

### Chapter 4 - Extending the interpreter

This chapter focused on extending the interpreter with more familiar concepts found in other programming languages. This included adding:

- **Strings**. This includes string concatenation.
- **Built-in functions**. This includes `len`, `first`, `last`, `rest`, `push` and `println`.
- **Arrays**. This includes array literals, and array indexing.
- **Hashes**. This includes hash literals, and hash indexing.

This part was generally straight forward, but I was impressed with how easy it is to extend the built-in functions.

#### Built-in functions

In order to add a new built-in function, you simply add its entry in the `HashMap` defined in [`src/builtins.rs`](/src/builtins.rs). The key is the name of the function, and the value is a function pointer to the function. The function pointer is defined in the same file, just below. This is simply Rust code that calls the evaluator with the correct arguments. The evaluator then evaluates the arguments, and returns the result.

## 💡 Future ideas

- [ ] also attach the line number, column number and filename to a token. Why? For example, to later output more useful error messages in the parsing stage. Instead of "error: expected semicolon token" it can output with line numbers etc.
- [ ] Don't treat all integers as `i64`. Based on size, allocate only the type needed.
- [ ] Support floating point numbers.
- [ ] Flesh out the `Error` in the parser to contain more information about what kind of error occurred. That way, the user gets a much better compile time error.
- [ ] Print colored and nice output to terminal when using the REPL. Thinking especially of the error messages.
- [ ] Add `Option` (`Some`, `None`) and `Result` (`Ok`, `Err`) to the language
