# Partial evaluation of Unstructured Reversible programs.
A specializer, inverter, and interpreter for RL.

## Dependencies
The project uses Stack to manage its dependencies.

## Quick Start Guide
The program can be built using the command `stack build`.
It can be ran using `stack run -- MODE`, where `MODE` is `specialize`, `interpret`, or `invert`. This defaults to specialization.

## Specializer
Performs partial evaluation of a program with respect to a given specialization input.
The specialization is given as a sequence of `variable = value` declarations.
For examples see the "test/progs" folder.

### Options
 - `-i FILEPATH`: Declare input program, default: "input.rl"
 - `-o FILEPATH`: Declare output destination, default: "output.rl"
 - `-s FILEPATH`: Declare input values, default: "input.spec"
 - `-silent`: Disable regular logging to stdout
 - `-skipSpec`: Print the annotated program.
 - `-trace`: Always trace the program states being specialized. 
 - `-skipPost`: Skip all forms of post-processing.
 - `-lift`: Residualizes the final static state of the output variables.

## Inversion
Invert the input program.

### Options
 - `-i FILEPATH`: Declare input program, default: "input.rl"
 - `-o FILEPATH`: Declare output destination, default: "output.rl"
 - `-silent`: Disable regular logging to stdout

## Interpretation
Interpret the input program with the given input.
Shows final output and statistics. 
The input follows the same format as in the specializer.

### Options
 - `-i FILEPATH`: Declare input program, default: "input.rl"
 - `-s FILEPATH`: Declare input values, default: "input.spec".
 - `-silent`: Disable regular logging to stdout