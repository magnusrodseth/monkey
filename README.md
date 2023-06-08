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

> TODO

---

> TODO: Jot down some notes for each completed chapter.

## 💡 Future ideas

- [ ] also attach the line number, column number and filename to a token. Why? For example, to later output more useful error messages in the parsing stage. Instead of "error: expected semicolon token" it can output with line numbers etc.
- [ ] Don't treat all integers as `i64`. Based on size, allocate only the type needed.
- [ ] Support floating point numbers.
- [ ] Flesh out the `Error` in the parser to contain more information about what kind of error occurred. That way, the user gets a much better compile time error.
- [ ] Remove uneccessary empty method in the AST for `Expression`, `Statement`, etc.
- [ ] Print colored and nice output to terminal when using the REPL. Thinking especially of the error messages.
