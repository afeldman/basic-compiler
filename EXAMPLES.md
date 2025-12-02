# BASIC Compiler - Example Programs

This document contains detailed examples of BASIC programs that can be compiled with this compiler.

## Table of Contents

1. [Hello World](#hello-world)
2. [Variables and Arithmetic](#variables-and-arithmetic)
3. [User Input](#user-input)
4. [Conditional Statements](#conditional-statements)
5. [Loops](#loops)
6. [Fibonacci Sequence](#fibonacci-sequence)
7. [Factorial Calculator](#factorial-calculator)
8. [Nested Loops](#nested-loops)
9. [Average Calculator](#average-calculator)

---

## Hello World

The simplest BASIC program prints a message and exits.

```basic
10 PRINT "Hello, World!"
20 END
```

**Compile and run:**

```bash
stack exec basic -- -S test/hallo_welt.basic -o hello.ll
clang hello.ll runtime.c -o hello
./hello
```

**Output:**

```
Hello, World!
```

---

## Variables and Arithmetic

BASIC uses the `LET` statement to assign values to variables.

```basic
10 LET X = 10
20 LET Y = 5
30 LET SUM = X + Y
40 LET DIFF = X - Y
50 LET PROD = X * Y
60 LET QUOT = X / Y
70 PRINT SUM
80 PRINT DIFF
90 PRINT PROD
100 PRINT QUOT
110 END
```

**Output:**

```
15
5
50
2
```

**Supported operators:**

- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/`
- Modulo: `%`

---

## User Input

The `INPUT` statement reads a number from the user.

```basic
10 PRINT "Enter a number:"
20 INPUT X
30 PRINT "You entered:"
40 PRINT X
50 END
```

**Usage:**

```bash
stack exec basic -- -S input.basic -o input.ll
clang input.ll runtime.c -o input
./input
```

**Example interaction:**

```
Enter a number:
42
You entered:
42
```

---

## Conditional Statements

### Simple IF...THEN

Jump to a line number if a condition is true.

```basic
10 INPUT X
20 IF X = 0 THEN 100
30 PRINT "Not zero"
40 GOTO 200
100 PRINT "Zero!"
200 END
```

### Comparison with Multiple Conditions

```basic
10 INPUT X
20 IF X < 0 THEN 100
30 IF X = 0 THEN 200
40 PRINT "Positive"
50 GOTO 300
100 PRINT "Negative"
110 GOTO 300
200 PRINT "Zero"
300 END
```

**Comparison operators:**

- Equal: `=`
- Less than: `<`
- Less than or equal: `<=`
- Greater than: `>`
- Greater than or equal: `>=`
- Not equal: `<>`

---

## Loops

### Simple FOR Loop

Count from 1 to 10 and print each number.

```basic
10 FOR I = 1 TO 10
20 PRINT I
30 NEXT I
40 END
```

**Output:**

```
1
2
3
...
10
```

### Loop with STEP

```basic
10 FOR I = 0 TO 20 STEP 5
20 PRINT I
30 NEXT I
40 END
```

**Output:**

```
0
5
10
15
20
```

### Sum in a Loop

```basic
10 LET SUM = 0
20 FOR I = 1 TO 10
30 LET SUM = SUM + I
40 NEXT I
50 PRINT "Sum 1-10:"
60 PRINT SUM
70 END
```

**Output:**

```
Sum 1-10:
55
```

---

## Fibonacci Sequence

Generate the first N Fibonacci numbers.

```basic
10 REM Fibonacci sequence
20 PRINT "How many Fibonacci numbers?"
30 INPUT N
40 LET A = 0
50 LET B = 1
60 PRINT A
70 PRINT B
80 FOR I = 2 TO N
90 LET C = A + B
100 PRINT C
110 LET A = B
120 LET B = C
130 NEXT I
140 END
```

**Usage:**

```bash
stack exec basic -- -S test/fibonacci.basic -o fib.ll
clang fib.ll runtime.c -o fib
echo "10" | ./fib
```

**Output:**

```
How many Fibonacci numbers?
0
1
1
2
3
5
8
13
21
34
```

---

## Factorial Calculator

Calculate the factorial of a number.

```basic
10 REM Calculate factorial
20 PRINT "Enter a number:"
30 INPUT N
40 LET F = 1
50 FOR I = 1 TO N
60 LET F = F * I
70 NEXT I
80 PRINT "Factorial is:"
90 PRINT F
100 END
```

**Usage:**

```bash
stack exec basic -- -S test/factorial.basic -o fact.ll
clang fact.ll runtime.c -o fact
echo "5" | ./fact
```

**Output:**

```
Enter a number:
Factorial is:
120
```

**Explanation:**

- 5! = 5 × 4 × 3 × 2 × 1 = 120

---

## Nested Loops

Demonstrate nested FOR loops.

```basic
10 REM Test various loop constructs
20 LET SUM = 0
30 FOR I = 1 TO 10
40 LET SUM = SUM + I
50 NEXT I
60 PRINT "Sum 1-10:"
70 PRINT SUM
80 REM Nested loop
90 LET TOTAL = 0
100 FOR X = 1 TO 3
110 FOR Y = 1 TO 3
120 LET TOTAL = TOTAL + X * Y
130 NEXT Y
140 NEXT X
150 PRINT "Nested sum:"
160 PRINT TOTAL
170 END
```

**Output:**

```
Sum 1-10:
55
Nested sum:
36
```

**Explanation:**

- Sum of 1-10 = 55
- Nested sum: (1×1 + 1×2 + 1×3) + (2×1 + 2×2 + 2×3) + (3×1 + 3×2 + 3×3) = 36

---

## Average Calculator

Calculate the average of N numbers using a loop with GOTO.

```basic
5 LET S = 0
10 INPUT V
20 LET N = V
30 IF N = 0 THEN 99
40 FOR I = 1 TO N
45 LET S = S + I
50 NEXT I
60 PRINT S/N
70 GOTO 5
99 END
```

**Usage:**

```bash
stack exec basic -- -S test/test_prog.basic -o avg.ll
clang avg.ll runtime.c -o avg
echo "5" | ./avg
```

**Output:**

```
3
```

**Explanation:**

- Reads N = 5
- Calculates sum: 1 + 2 + 3 + 4 + 5 = 15
- Average: 15 / 5 = 3
- Loops back to line 5 (enter 0 to exit)

---

## Advanced Examples

### Number Guessing Game

```basic
10 REM Number guessing (simplified)
20 LET SECRET = 42
30 PRINT "Guess the number:"
40 INPUT GUESS
50 IF GUESS = SECRET THEN 100
60 IF GUESS < SECRET THEN 200
70 PRINT "Too high!"
80 GOTO 30
100 PRINT "Correct!"
110 GOTO 300
200 PRINT "Too low!"
210 GOTO 30
300 END
```

### Prime Number Check (simplified)

```basic
10 PRINT "Enter a number:"
20 INPUT N
30 LET PRIME = 1
40 FOR I = 2 TO N - 1
50 LET MOD = N % I
60 IF MOD = 0 THEN 100
70 NEXT I
80 GOTO 200
100 LET PRIME = 0
110 GOTO 220
200 PRINT "Prime!"
210 GOTO 300
220 PRINT "Not prime"
300 END
```

### Temperature Converter

```basic
10 PRINT "Celsius to Fahrenheit"
20 INPUT C
30 LET F = C * 9 / 5 + 32
40 PRINT "Fahrenheit:"
50 PRINT F
60 END
```

---

## Tips and Tricks

### Comments

Use `REM` for comments:

```basic
10 REM This is a comment
20 LET X = 10  ` This is also a comment
```

### Line Numbering

- Line numbers don't need to be sequential
- Common practice: increment by 10 (allows inserting lines later)
- Example: 10, 20, 30... instead of 1, 2, 3...

### Variable Names

- Single letters work best: A, B, C, X, Y, Z
- Multi-character names are supported: SUM, COUNT, TOTAL

### Debugging

- Use PRINT statements to see variable values
- Add REM comments to document your logic

### Performance

- Avoid excessive GOTO statements (makes code hard to follow)
- Use FOR loops instead of GOTO for counting
- Keep loop bodies simple for better performance

---

## Compilation Tips

### LLVM IR Only

```bash
# Generate LLVM IR (.ll file)
stack exec basic -- -S program.basic -o output.ll
```

### Full Compilation

```bash
# Generate LLVM IR
stack exec basic -- -S program.basic -o program.ll

# Compile to object file
clang program.ll runtime.c -o program

# Run
./program
```

### Optimization

```bash
# With LLVM optimizations
clang -O3 program.ll runtime.c -o program
```

### View LLVM IR

```bash
# Human-readable LLVM IR
cat program.ll

# Or use LLVM tools
llvm-dis program.bc -o program.ll
```

---

## Next Steps

1. **Try the examples** - Run all example programs in `test/`
2. **Write your own** - Create custom BASIC programs
3. **Explore LLVM IR** - Study the generated `.ll` files
4. **Optimize** - Experiment with compiler flags
5. **Contribute** - Add new language features or examples

## Resources

- [BASIC Language Reference](https://en.wikipedia.org/wiki/BASIC)
- [LLVM Documentation](https://llvm.org/docs/)
- [Project Repository](https://github.com/afeldman/basic-compiler)
- [ACC Collection](https://github.com/afeldman/acc)
