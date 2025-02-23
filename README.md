# MicroGD

A lightweight, embeddable GDScript interpreter

## Why?

GDScript is a great language for the purpose it's trying to solve: game engines

- Approachable syntax designed for beginners
- Object-oriented nature is perfect for game engines
- Stricter than similar dynamic languages, opening possibilities for better optimizations
- Established ecosystem of editors, IDEs, and tools

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

MicroGD is in **extremely early** alpha, so there will be bugs and mismatches with the spec.

For now, working features are prioritized over performance and 100% adherence

Project status:

- ✅ Tokenization
- 🔄 Parsing
- ❌ Compiling
- 🔄 Interpreting
- ❌ Optimization
- 🔄 Rust and C API
- ❌ LSP
- ❌ Debugger
