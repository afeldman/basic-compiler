# BASIC Compiler

A classic BASIC to LLVM compiler that brings the simplicity of 1970s BASIC to modern native code compilation.

Part of [ACC (Anton's Compiler Collection)](https://github.com/afeldman/acc).

## Features

- 🎯 **Classic BASIC Syntax** - Line numbers, GOTO, FOR/NEXT, IF/THEN
- 🚀 **LLVM Backend** - Compiles to optimized native code via LLVM
- 📝 **BNFC Grammar** - Clean, maintainable parser generator
- 🔧 **Haskell Implementation** - Type-safe compiler construction
- ⚡ **Fast Execution** - Native binaries with C runtime

## Language Support

### Statements

- `LET` - Variable assignment: `LET X = 42`
- `PRINT` - Output: `PRINT X`
- `INPUT` - Read input: `INPUT N`
- `GOTO` - Jump to line: `GOTO 100`
- `IF...THEN` - Conditional: `IF X = 0 THEN 100`
- `FOR...TO...NEXT` - Loops: `FOR I = 1 TO 10 ... NEXT I`
- `REM` - Comments: `REM This is a comment`
- `END` - Program termination

### Operators

- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `=`, `<`, `<=`, `>`, `>=`, `<>`
- Logical: `&` (AND), `|` (OR), `!` (NOT)

## Quick Start

### Prerequisites

```bash
# Install Stack (Haskell)
curl -sSL https://get.haskellstack.org/ | sh

# Install LLVM (choose your platform)
brew install llvm@9         # macOS
sudo apt install llvm-9     # Ubuntu 20.04
sudo apt install llvm-14    # Ubuntu 22.04+

# Install BNFC
stack install BNFC

# Install Task (optional, but recommended)
brew install go-task        # macOS
# or see: https://taskfile.dev/installation/
```

### Build

```bash
# Using Task (recommended)
task build

# Or manually
bnfc -m --haskell -d basic.cf
mkdir -p src
mv basic/*.hs src/
stack build
```

### Run

```bash
# Compile BASIC program to LLVM IR
stack exec basic -- -S test/hallo_welt.basic

# Compile LLVM IR to executable
clang test/hallo_welt.ll runtime.c -o hello

# Run
./hello
```

## Example Programs

### Hello World

```basic
10 PRINT "Hello, World!"
20 END
```

### Fibonacci Sequence

```basic
10 REM Fibonacci sequence
20 INPUT N
30 LET A = 0
40 LET B = 1
50 PRINT A
60 PRINT B
70 FOR I = 2 TO N
80 LET C = A + B
90 PRINT C
100 LET A = B
110 LET B = C
120 NEXT I
130 END
```

### Factorial

```basic
10 PRINT "Enter a number:"
20 INPUT N
30 LET F = 1
40 FOR I = 1 TO N
50 LET F = F * I
60 NEXT I
70 PRINT "Factorial:"
80 PRINT F
90 END
```

More examples in `test/`:

- `fibonacci.basic` - Fibonacci sequence generator
- `factorial.basic` - Factorial calculator
- `loops.basic` - Nested loop examples
- `conditions.basic` - Conditional branching
- `test_prog.basic` - Average calculator with loop

## Testing

```bash
# Run all tests
task test

# Or manually test a specific file
stack exec basic -- -S test/fibonacci.basic
clang test/fibonacci.ll runtime.c -o fib
./fib
```

## Usage

```bash
basic [OPTIONS] <input.basic>

Options:
  -o FILE, --output FILE     Output file (default: out.ll or a.out)
  -S, --emit-llvm            Emit LLVM IR (.ll file)
  -h, --help                 Show help message

Examples:
  # Emit LLVM IR
  basic -S program.basic

  # Specify output file
  basic -S -o myprogram.ll input.basic

  # Compile to executable (two-step process)
  basic -S program.basic
  clang program.ll runtime.c -o program
```

## Project Structure

```
basic/
├── README.md              # This file
├── basic.cf               # BNFC grammar definition
├── runtime.c              # C runtime (I/O functions)
├── basic-compiler.cabal   # Cabal package file
├── stack.yaml             # Stack configuration
├── Taskfile.yaml          # Task automation
├── src/
│   ├── Main.hs           # Compiler entry point
│   ├── CodeGen.hs        # LLVM code generation
│   └── LLVM.hs           # LLVM helper functions
└── test/
    ├── hallo_welt.basic  # Hello World
    ├── fibonacci.basic   # Fibonacci generator
    ├── factorial.basic   # Factorial calculator
    ├── loops.basic       # Loop examples
    ├── conditions.basic  # Conditional examples
    └── test_prog.basic   # Average calculator
```

## Implementation Details

### LLVM IR Generation

- Variables are represented as double-precision floats
- Line numbers create basic blocks for GOTO targets
- FOR loops use phi nodes for induction variables
- Runtime functions handle I/O operations

### Runtime Functions (runtime.c)

- `print_double(double)` - Print number to stdout
- `print_string(char*)` - Print string to stdout
- `input_double()` - Read number from stdin
- `read_string_as_double(char*)` - Parse string to number

### Compilation Pipeline

```
BASIC Source → Parser (BNFC) → AST → CodeGen → LLVM IR → Clang → Native Binary
```

## Docker Support

Build and run in Docker container:

```bash
# Build Docker image
docker compose build

# Run compiler in container
docker compose run acc-dev bash
cd basic
task build
task test
```

## Contributing

This is part of the ACC (Anton's Compiler Collection). For contributions:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## License

See LICENSE file.

## Resources

- [LLVM Documentation](https://llvm.org/docs/)
- [BNFC Tutorial](https://bnfc.digitalgrammars.com/)
- [Classic BASIC Reference](https://en.wikipedia.org/wiki/BASIC)
- [ACC Project](https://github.com/afeldman/acc)

## Part of ACC

This compiler was extracted from the ACC (Anton's Compiler Collection) monorepo for better modularity and maintenance.

**Related Projects:**

- [Karel Compiler](https://github.com/afeldman/karel-compiler) - FANUC Karel to LLVM
- [Brainfuck Compiler](https://github.com/afeldman/brainfuck-compiler) - Brainfuck to LLVM
- [Whitespace Compiler](https://github.com/afeldman/whitespace-compiler) - Whitespace to LLVM

---

**Author:** Anton Feldmann  
**Created:** 2025-12-02  
**Repository:** https://github.com/afeldman/basic-compiler
