# MicroGD

A lightweight, embeddable GDScript interpreter

## Why?

GDScript is a great language for its domain: game engines

- Beginner-friendly, approachable syntax
- Gradually typed, opening possibilities for better optimization
- Established ecosystem of editors, IDEs, and tools
- Object-oriented nature is perfect for game programming

## Goals and non-goals

MicroGD hopes to:

- Be near-spec-compliant in parsing and execution
- Very lightweight, even embeddable in microcontrollers (hence the name)
- Support LSP and debugging integration

MicroGD **does not** hope to:

- Use Just-in-time compilation
- Adhere 100% to every detail of GDScript (although this would be ideal)
- Support future features of GDScript, as this could result in sacrificing the core hopes of
  MicroGD, depending on how exotic the feature is

# Roadmap

- ✅ Tokenization (95% passing)
- ✅ Parsing (27% passing)
- 🔄 Compiling
- 🔄 Interpreting
- ❌ Optimization
- 🔄 Rust and C API
- ❌ LSP
- ❌ Debugger

MicroGD uses tests from the official
[Godot Engine Repository](https://github.com/godotengine/godot/tree/master/modules/gdscript/tests)
to ensure identical behavior.

For now, working features are prioritized over performance and 100% compatibility.
