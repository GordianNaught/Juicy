# Contribution
This project is under the GPL 3.0 license.
The full license text can be found [here](#/GordianNaught/Juicy/lob/master/LICENSE.md).

## Intrinsic Functions
Many intrinsic functions are in need of implementation.

To implement an intrinsic function, a transation of the function into [pseudoassembly] must be made. This translation must be made in the [juicy_intrinsics.pl](/GordianNaught/Juicy/blob/master/juicy_intrinsics.pl) file.
To make a complete intrinsic implementation, an implementation of `intrinsic/3` and `intrinsic_instructions` must be made. For a one argument intrinsic, `intrinsic_instructions/3` must be implemented. For a two argument intrinsic, `intrinic_instructions/4` must be implemented. There is currently not a structure in place to implement intrinsics of more than 2 arguments.

Intrinsic functions in need of immediate implementation are arithmetic operations on floats, bytes, and bits and i/o functions.

## Intrinsic Types
An intrinsic type is a type the language understands with no explicit implementation.

A type must either have a literal representation, as is the case for integers, or be castable from a type with a literal represenation.

A typecast is a function like any other. To create a casting function, implement it as an intrinsic function named for the type you are casting to. Information on the implementation of intrinsic functions is detailed in [Intrinsic Functions](#intrinsic-functions).

Without functions that act on an intrinsic type, the type is of little use.
Consider adding additional [Intrinsic Functions](#intrinsic-functions) that take your created type as an argument. Any existing function allows an overloaded implementation for your new argument type.
