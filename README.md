# A Partial Evaluator for A Reversible Language.
A specializer, inverter, (abstract) interpreter, and normalizer for ARL.
Based on [previous work](https://dl.acm.org/doi/10.1145/3635800.3636967).

## Dependencies
The project uses Stack to manage its dependencies.

## Quick Start Guide
The program can be built using the command `stack build`.
It can be ran using `stack run -- MODE`, where `MODE` is either `spec`, `interpret`, `invert`, or `bench`.

## Specializer
Command: `spec`.
Performs partial evaluation of a program with respect to a given specialization input.
The specialization is given as a sequence of `variable = 'constant` declarations in a file.
For examples see the "progs" folder.

    Usage: PERevFlow-exe spec <Input RL file> <Output path> <Spec file>
                              [-u|--uniformBTA] [-s|--skipSpec] [-p|--skipPost]
                              [-v|--verbose] [-t|--trace]

### Options
 - `-u | --uniformBTA`: Use the uniform binding-time analysis
 - `-s | --skipSpec`: Disable post-processing of residual program
 - `-v | --verbose`: Enable/Disable the default logging to stdout. Default: Enabled
 - `-t | --trace`: Enable tracing of the specialization process. May be very long.

## Inversion
Invert the input program.

    Usage: PERevFlow-exe invert <Input RL file> <Output path> [-v|--verbose]

### Options
 - `-v | --verbose`: Enable/Disable the default logging to stdout. Default: Enabled

## Interpretation
Interpret the input program with the given input.
Shows final output and statistics.
The input follows the same format as in the specializer.

    Usage: PERevFlow-exe interpret <Input RL file> <Spec file> [-v|--verbose]

### Options
 - `-v | --verbose`: Enable/Disable the default logging to stdout. Default: Enabled

## Benchmark
Compare the size and speed of a source program for a given input with the size and speed of the residual programs from specializing it with the different BTA's.

The variables that should remain dynamic during specialization are specified in a space-separated list, e.g. `"x y z"`.

    Usage: PERevFlow-exe bench <Input RL file> <Spec file>
                               <List of dynamic input vars> [-v|--verbose]

### Options
 - `-v | --verbose`: Enable/Disable the default logging to stdout. Default: Enabled
