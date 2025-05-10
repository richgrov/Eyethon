# Eyethon

(pronounced "eye-thon")

A lightweight, embeddable, inspectable Python interpreter

## Why?

In the age of AI, writing code will be a task more for machines rather than humans. Models excel at
writing Python, but it performance and lack of portability restrict its use. Eyethon aims to
interpret a subset of Python that's not only runnable, but who's syntax tree is inspectable to be
converted to other languages, formats, or domains.

## Goals and non-goals

Eyethon hopes to:

- Be very lightweight
- Support some of Python's standard library
- Provide an API for running and inspecting code

MicroGD **does not** hope to:

- Use Just-in-time compilation
- Adhere 100% to every detail of Python (although this would be ideal)

# Roadmap

- ✅ Tokenization (97% passing)
- ✅ Parsing (55% passing)
- 🔄 Compiling
- 🔄 Interpreting
- ❌ Optimization
- 🔄 Rust and C API
- ❌ LSP
- ❌ Debugger

Eyethon uses tests from
[PocketPy](https://github.com/godotengine/godot/tree/master/modules/gdscript/tests)
to test spec compliance.

For now, working features are prioritized over performance and 100% compatibility.
