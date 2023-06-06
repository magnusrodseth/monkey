# 🐒 The Monkey programming language

## Description 

> TODO: Add description of the repository

## Why the name "Monkey"?

As stated in Thorsten Ball's "Writing an Interpreter in Go":

> But why the name? Why is it called “Monkey”? Well, because monkeys are magnificent, elegant, fascinating and funny creatures. Exactly like our interpreter.

## Running the interpreter

```sh
# Navigate to the project directory
cd monkey

# Run tests
cargo test
```

## Future ideas

- [ ] also attach the line number, column number and filename to a token. Why? For example, to later output more useful error messages in the parsing stage. Instead of "error: expected semicolon token" it can output with line numbers etc.
- [ ] Don't treat all integers as `i64`. Based on size, allocate only the type needed.
- [ ] Support floating point numbers.
