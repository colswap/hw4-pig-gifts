# HW4-Ex4: Pig Gifts (Constraint Solver)

[cite_start]This project solves the "Pig Gifts" problem from the SNU Programming Languages course (HW4-Ex4) [cite: 1545-1546, 1561-1564].

The problem is a constraint satisfaction problem involving set dependencies between multiple agents ("pigs"). This OCaml program is designed to find the minimal set of "gifts" that satisfies all stated conditions.

### Tech Stack
* **OCaml**

### How to Compile & Run Tests

This project uses the `testlib.ml` utility. You can compile and run the tests using the OCaml compiler:

```sh
# Compile all source files and link them into an executable named 'test'
ocamlc -o test testlib.ml hw4_4.ml ta_test4_4.ml test4_4.ml

# Run the compiled test runner
./test