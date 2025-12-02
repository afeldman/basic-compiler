# Building the BASIC Compiler

Detailed build instructions for different platforms and scenarios.

## Quick Start

```bash
# Clone repository
git clone https://github.com/afeldman/basic-compiler.git
cd basic-compiler

# Build with Task (recommended)
task build

# Or build manually
stack build

# Test
task test
```

## Prerequisites

### Required

- **Haskell Stack** (or Cabal)
- **GHC 9.4.8** (automatically installed by Stack)
- **LLVM 9-15** (any version in this range)
- **Clang** (for compiling LLVM IR to executables)
- **BNFC** (BNF Converter)

### Optional

- **Task** (task runner, recommended)
- **Make** (alternative to Task)
- **Docker** (for containerized builds)

## Platform-Specific Instructions

### macOS

```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Install LLVM and Clang
brew install llvm@14

# Install BNFC
stack install BNFC

# Install Task (optional)
brew install go-task

# Build
task build
# or
make -f Makefile.dev build
```

### Ubuntu 20.04

```bash
# Update package list
sudo apt-get update

# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Install LLVM 9 (matches llvm-hs-9.0.1)
sudo apt-get install -y llvm-9 llvm-9-dev clang-9

# Or install LLVM 14
sudo apt-get install -y llvm-14 llvm-14-dev clang-14

# Install build tools
sudo apt-get install -y build-essential git

# Install BNFC
stack install BNFC

# Install Task (optional)
sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin

# Build
task build
# or
make -f Makefile.dev build
```

### Ubuntu 22.04+

```bash
# Update package list
sudo apt-get update

# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Install LLVM 14 or 15
sudo apt-get install -y llvm-14 llvm-14-dev clang-14

# Install build tools
sudo apt-get install -y build-essential git

# Install BNFC
stack install BNFC

# Build
task build
```

### Arch Linux

```bash
# Install Stack
sudo pacman -S stack

# Install LLVM and Clang
sudo pacman -S llvm clang

# Install BNFC via Stack
stack install BNFC

# Build
task build
```

### Windows (WSL2)

Use Ubuntu instructions within WSL2:

```bash
# From PowerShell, install WSL2
wsl --install -d Ubuntu-22.04

# Enter WSL
wsl

# Follow Ubuntu 22.04 instructions above
```

## Build Methods

### Method 1: Task (Recommended)

```bash
# Install Task
# macOS
brew install go-task

# Linux
sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin

# Build
task build

# Run tests
task test

# Clean
task clean
```

### Method 2: Make

```bash
# Build
make -f Makefile.dev build

# Run tests
make -f Makefile.dev test

# Clean
make -f Makefile.dev clean
```

### Method 3: Stack Directly

```bash
# Generate parser
bnfc -m --haskell -d basic.cf
mkdir -p src
mv basic/*.hs src/
rmdir basic

# Build
stack build

# Run compiler
stack exec basic -- -S test/hallo_welt.basic

# Install globally
stack install
```

### Method 4: Cabal

```bash
# Generate parser
bnfc -m --haskell -d basic.cf
mkdir -p src
mv basic/*.hs src/

# Update Cabal
cabal update

# Build
cabal build

# Run
cabal exec basic -- -S test/hallo_welt.basic

# Install
cabal install
```

## Docker Build

### Using Docker Compose

```bash
# From ACC root directory
cd /path/to/acc

# Build Docker image
docker compose build

# Enter container
docker compose run acc-dev bash

# Inside container
cd basic
task build
task test
```

### Standalone Docker

```dockerfile
# Dockerfile
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    curl git build-essential \
    llvm-9 llvm-9-dev clang-9 \
    libffi-dev libgmp-dev zlib1g-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /workspace
COPY . .

RUN stack install BNFC
RUN bnfc -m --haskell -d basic.cf
RUN mkdir -p src && mv basic/*.hs src/ || true
RUN stack build

CMD ["bash"]
```

Build and run:

```bash
docker build -t basic-compiler .
docker run -it basic-compiler
```

## Troubleshooting

### LLVM Version Mismatch

**Problem:** `llvm-hs-14.0.0.0 not found`

**Solution:** Use LLVM 9 (matches llvm-hs-9.0.1):

```bash
# Ubuntu
sudo apt-get install llvm-9 llvm-9-dev

# Or modify stack.yaml to use correct llvm-hs version
```

### BNFC Not Found

**Problem:** `bnfc: command not found`

**Solution:**

```bash
stack install BNFC
export PATH="$HOME/.local/bin:$PATH"
```

### Missing Dependencies

**Problem:** `Could not find module 'LLVM.AST'`

**Solution:**

```bash
# Ensure all dependencies are installed
stack build --dependencies-only

# Or update Stack
stack update
```

### Parser Generation Fails

**Problem:** BNFC fails to generate parser

**Solution:**

```bash
# Ensure basic.cf is valid
bnfc --check basic.cf

# Try generating without makefile
bnfc --haskell -d basic.cf
```

### Clang Not Found

**Problem:** `clang: command not found` when testing

**Solution:**

```bash
# macOS
brew install llvm

# Ubuntu
sudo apt-get install clang

# Check installation
which clang
```

## Build Options

### Debug Build

```bash
# Build with debugging symbols
stack build --ghc-options="-g"
```

### Optimized Build

```bash
# Build with optimizations
stack build --ghc-options="-O2"
```

### Profiling Build

```bash
# Build with profiling
stack build --profile
```

### Verbose Build

```bash
# Show detailed build output
stack build --verbose
```

## Testing the Build

### Quick Test

```bash
# Build and test Hello World
stack exec basic -- -S test/hallo_welt.basic -o hello.ll
clang hello.ll runtime.c -o hello
./hello
```

### Run All Tests

```bash
# Using Task
task test

# Or Make
make -f Makefile.dev test

# Or manually
task test:hello
task test:fibonacci
task test:factorial
```

### Verify Installation

```bash
# After stack install
which basic
basic --help

# Test installed version
basic -S test/hallo_welt.basic
```

## Continuous Integration

The project includes GitHub Actions for automated testing:

- **Workflow:** `.github/workflows/build-and-test.yml`
- **Triggers:** Push, Pull Request
- **Platform:** Ubuntu Latest
- **Tests:** All example programs

View build status: [GitHub Actions](https://github.com/afeldman/basic-compiler/actions)

## Development Setup

### IDE Support

#### VS Code

Install extensions:
- Haskell (haskell.haskell)
- LLVM (llvm-vs-code-extensions.vscode-lldb)

#### Emacs

Use `haskell-mode` and `lsp-mode`:

```elisp
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp))
```

#### Vim/Neovim

Use `vim-haskell` and CoC:

```vim
Plug 'neovimhaskell/haskell-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
```

### Useful Commands

```bash
# Format code
stack exec -- ormolu --mode inplace src/**/*.hs

# Lint
stack exec -- hlint src/

# Type check only
stack build --fast --test --no-run-tests

# Watch for changes
stack build --file-watch

# REPL
stack ghci
```

## Build Performance

### Caching

Stack caches compiled dependencies:

- **Cache location:** `~/.stack/`
- **Clear cache:** `rm -rf ~/.stack/`
- **Project cache:** `.stack-work/`

### Parallel Builds

```bash
# Use all CPU cores
stack build -j

# Use specific number of cores
stack build -j4
```

### Incremental Builds

Stack only rebuilds changed modules:

```bash
# Fast rebuild after changes
stack build --fast
```

## Next Steps

1. **Build the compiler** using your preferred method
2. **Run the tests** to verify everything works
3. **Read EXAMPLES.md** for BASIC programming examples
4. **Read README.md** for usage instructions
5. **Start coding** your own BASIC programs!

## Getting Help

- **Issues:** [GitHub Issues](https://github.com/afeldman/basic-compiler/issues)
- **Discussions:** [GitHub Discussions](https://github.com/afeldman/basic-compiler/discussions)
- **Email:** anton@example.com
- **Documentation:** See README.md and EXAMPLES.md

---

**Last Updated:** 2025-12-02
