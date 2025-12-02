 #!/bin/bash
# Build script for BASIC compiler

set -e

echo "=== Building BASIC Compiler ==="

# Generate parser with BNFC
echo "Generating parser..."
bnfc -m --haskell -d basic.cf
mv basic/*.hs src/ 2>/dev/null || true
rmdir basic 2>/dev/null || true

# Build with Stack
echo "Building compiler..."
stack build

echo ""
echo "✅ Build complete!"
echo ""
echo "Usage:"
echo "  stack exec basic -- input.basic"
echo "  stack exec basic -- -S input.basic  # Emit LLVM IR"
echo ""