# Interfacing D with C and Fortran

## Introduction

This article describes how to interface D with C and Fortran. The examples presented here are run on a Linux Ubuntu 64-bit 16.04 system and use the [gcc 5.4.0][gcc] C and Fortran compilers and [ldc2][ldc2] D's LLVM-based D compiler. The D code in this article focuses on the `-betterC` style, this mode of programming is increasingly popular in D circles focusing on D as a replacement for C in systems programming. The [`-betterC`][betterC] flag in the D compiler is used to program with a ligth-weight subset of D and as the side-effect of facilitating an integration of D code to C that is as seamless as code from C to D.


## Calling C functions from D.

The math.h is a standard library in C. [example1 code](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/example1) below shows the D code for importing the `fabs` and `pow` functions from the C `math.h` library. The curly brace encapsulated `extern (C)` statement is used to declare the functions that we would like to import from C. The qualifiers `nogc` and `nothrow` are because the imported C functions are not garbage collected and C does not throw exceptions. In general the C language all too easily allows unsafe actions one of which is corrupting data. Some of the imported functions are modified with `@safe` which [ensures that they do not carry out a number of potentially unsafe actions][progD]. The `printf` function is not marked with `@safe` and instead the `scope` statement is used because the pointer is not preserved internally.

```
/* example1.d */

@nogc nothrow: 

/* Declare C functions */
extern(C)
{
    double pow(double x, double y) @safe;
    double fabs(double x) pure @safe;
    int printf(scope const char* format, ...);
}

pragma(inline, false)
void printArray(double[] arr)
{
    foreach(elem; arr)
        printf("%f ", elem);
    printf("\n");
}

pragma(inline, true)
void apply(alias fun)(double[] arr)
{
    foreach(ref elem; arr)
        elem = fun(elem);
}

__gshared double[] x = [-1, 2, -3, 4, -5 , 6];

extern(C)
int main()
{
    apply!fabs(x);
    printArray(x);
    apply!(a => pow(a, 2.5))(x);
    printArray(x);
    return 0;
}
```

```
$ ldc2 -betterC -Oz -release -linkonce-templates -run example1.d
1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 
1.000000 5.656854 15.588457 32.000000 55.901699 88.181631 
```

The following are helpful descriptors of the code below:

* Full descriptions of the flags for the `ldc2` compiler are given with the `-help` flag, but the current flags are to reduce code size and bloat and create light-weight executables.

* If you are new to D the `apply` function is an example of using meta-programming techniques to pass a function using the template [alias parameters][aliasParameters]. Purity of functions can be enforced in D using the `pure` qualifier to indicate that the function can not have side-effects which is the case with the `fabs` function.

* The `printArray` function is used multiple times and can be explicitly marked with [`pragma(inline, false)`][pragma].

* The [`__gshared`][share] qualifier indicates global shared data - the default behaviour in C/C++, it places `x` in the DATA section of the object file.

* Garbage collection and execptions are not used so `extern(C)` declares common C `_main` instead of D `__Dmain`. This allows the final executable to be reduced in size by 72 times (from 617.5 KB to 8.5 KB).

The program example does not require DRuntime to be linked.

```
$ ldc2 -betterC -nogc -Oz -release -linkonce-templates -c example1.d
$ nm example1.o
0000000000000000 T __D7article10printArrayFNbNiAdZv
0000000000000050 T _main
0000000000000170 D __D7article1xAd
0000000000000180 d _.constarray
                 U _fabs
                 U _pow
                 U _printf
                 U _putchar
```

The [`nm` tool][nmtool] lists the symbols in the object file that can be described as:

 - `__D7article10printArrayFNbNiAdZv ` - `printArray`, D mangling.
 - `_main` - `main`, C mangling.
 - `__D7article1xAd ` - `x`'s length and pointer
 - `_.constarray` - `x`'s data
 - `_fabs`, `_pow`, `_printf` - extern C symbols.
 - `_putchar` - extern C symbol, optimized version for `printf("\n")`


### Calling static C libraries from D

The main difference between calling static C libraries from D and calling installed C libraries is in the compilation process. Below is an example of a simple multiplication function written in C:

```
/* example2c.c */
double mult(double x, double y)
{
    return x * y;
}
```

The D code to call this is as before:

```
/* example2d.d */
extern(C) @nogc nothrow:

double mult(double x, double y) @safe pure;
int printf(scope const char* format, ...);

extern(C)
int main()
{
    printf("%f\n", mult(3, 4));
    return 0;
}
```

There are many ways to compile these two scripts. Below the C code is first compiled to an object file, and the D code is compiled to a separate object file. Both object files are compiled together with the `gcc` compiler. Another option is to compile the C code to an object file and then use the D compiler to compile the C object file together with the `.d` script(s) to generate the executable.

```
$ gcc -O -c example2c.c
$ ldc2 -Oz -c -release -betterC -nogc example2d.d
$ gcc example2c.o example2d.o -oexample2
$ ./example2
12.000000

# in addition
$ nm example2c.o
0000000000000000 T _mult
$ nm example2d.o
0000000000000000 T _main
                 U _mult
                 U _printf
```

The code is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/example2).

## Calling D functions from C

Calling C from D is a less seamless affair because D has features that are not supported in C, however as before, `-betterC` can be used to create compatitable object files. The D code below creates multiplication functions of different types using string mixins.

```
/* example3d.d */
extern (C) nothrow @nogc @safe pure:

/* Template to generate string */
template GenWrap(string type)
{
	enum string GenWrap = type ~ " " ~ type[0] ~ "mult(" ~ type ~ " x, " ~ type ~ " y)\n{\n\treturn x*y;\n}";
}

mixin(GenWrap!"double");
mixin(GenWrap!"float");
```

On the C-side we create a script to reference and call the functions we have created in D.


```
/* example3c.c */
#include <stdio.h>

extern double dmult(double x, double y);
extern float fmult(float x, float y);;

int main()
{
    double xd = 3.0, yd = 4.0;
    float xf = 3.0, yf = 4.0;
    printf("output: %f\n", dmult(xd, yd));
    printf("output: %f\n", fmult(xf, yf));
    return 0;
}
```

To compile:

```
$ gcc -O -c example3c.c
$ ldc2 -O -release -betterC -linkonce-templates -c example3d.d
$ gcc example3c.o example3d.o -oexample3
$ ./example3
output: 12.000000
output: 12.000000

# in addition
$ nm example3c.o
                 U _dmult
                 U _fmult
0000000000000000 T _main
                 U _printf
$ nm example3d.o
0000000000000000 T _dmult
0000000000000010 T _fmult
```

The above code is [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/example3).

## Calling FORTRAN code from D

Someone or a group of brave souls have created a high performance numeric library in FORTRAN and you would like to call this library from D. Calling Fortran from C is straightforward, but so is calling Fortran from D. Here is the Fortran version of my multiplication function:

Fortran subroutines can be called directly from D in a very similar way to calling C from D. Below is a Fortran subroutine equivalent of the multiplication function:

```
!example4f.f90
SUBROUTINE MULT(x, y)
IMPLICIT NONE
REAL*8, INTENT(IN) :: x
REAL*8, INTENT(INOUT) :: y
y = x*y
END SUBROUTINE MULT
```

The main difference between calling C code and Fortran code from D is that the inputs to the Fortran subroutines must be referenced and the name of the function is mangled in the Fortran object file to include an underscore afterwards. C-style notation can be used `double mult_(double* x, double* y);`, however D provides `ref` notation which allows referenced inputs without requring pointers.

```
/* example4d.d */
extern (C) nothrow @nogc:

double mult_(ref double x, ref double y) @safe pure;
int printf(scope const char* format, ...);

int main(){
    double x = 4, y = 5;
    printf("%f\n", mult_(x, y));
    return 0;
}
```

Compilation is similar to calling C from D:

```
gfortran -c example4f.f90
ldc2 -ofexample4 example4d.d example4f.o && ./example4
```

The output from the Fortran object file showing the underscore mangled append name:

``
$ nm example4f.o
0000000000000000 T mult_
``

The code for the above is located [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/example4).

This [link](http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/F90-Subprograms.pdf) is a useful resource for Fortran and [this](http://www.yolinux.com/TUTORIALS/LinuxTutorialMixingFortranAndC.html) has useful for compilation hints.

## Calling Fortran code from D

This section uses string mixins to generate D code to generate wrapper code for Fortran functions. The Fortran functions to port are subroutines for sine, cosine, tangent, and arctangent functions:

```
!example5f.f90
SUBROUTINE SIN(x)
IMPLICIT NONE
REAL*8, INTENT(INOUT) :: x
x = DSIN(x)
END SUBROUTINE SIN

SUBROUTINE COS(x)
IMPLICIT NONE
REAL*8, INTENT(INOUT) :: x
x = DCOS(x)
END SUBROUTINE COS

SUBROUTINE TAN(x)
IMPLICIT NONE
REAL*8, INTENT(INOUT) :: x
x = DTAN(x)
END SUBROUTINE TAN

SUBROUTINE ATAN(x)
IMPLICIT NONE
REAL*8, INTENT(INOUT) :: x
x = DATAN(x)
END SUBROUTINE ATAN
```

On the D side the `Declare` template is used to generate a compile-time string of the appropriate declaration and the `Wrap` template generates a string:


```
/* example5d.d */
template Declare(string fun)
{
	enum string Declare = "double " ~ fun ~ "_(ref double x) pure;";
}

extern(C) nothrow @nogc
{
	int printf(scope const char* format, ...);
	mixin(Declare!"sin");
	mixin(Declare!"cos");
	mixin(Declare!"tan");
	mixin(Declare!"atan");
}

template Wrap(string fun)
{
	enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(x);\n}";
}

mixin(Wrap!"sin");
mixin(Wrap!"cos");
mixin(Wrap!"tan");
mixin(Wrap!"atan");

/* To Compile:
** gfortran -c example5f.f90
** ldc2 -ofexample5 example5d.d example5f.o && ./example5
*/

int main(){
	double pii = 1;
    immutable double pi = 4*atan(pii);
    assert(sin(pi/2) == 1);
	assert(cos(0) == 1);
	assert(tan(0) == 0);
	printf("Yay!\n");
	return 0;
}
```

It doesn't take too much imagination to realise that you can use D code to generate the Fortran string, compile it and generate
the relevant D wrapper code. Yes it is possible to do this in C/C++ using nasty macros or maybe template programming (I wouln't
like to try), but hopefully you can see that this kind of thing is pretty straightforward in D, because it was designed with
meta-progamming in mind. We can compile the Fortran and D code with `make` though D has it's own package manager called 
[DUB](https://code.dlang.org/getting_started). The `make` code is below:

```
example5 : example5d.d example5f.o
    ldc2 -O -release -betterC -linkonce-templates -oftrig example5d.d example5f.o
example5f.o : example5f.f90
    gfortran -O -c example5f.f90
.PHONY : clean
clean :
    rm example5 *.o
```

Then run with `make`. The output of the object file is:

```
$ nm example5f.o
                 U atan
0000000000000090 T atan_
                 U cos
0000000000000030 T cos_
                 U sin
0000000000000000 T sin_
                 U tan
0000000000000060 T tan_

```

The code for this section is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/example5).


## References

[quickD]: http://www.active-analytics.com/blog/a-quick-look-at-d/  "A quick look at D"
[gcc]: https://gcc.gnu.org "GNU C Collection"
[ldc2]: https://github.com/ldc-developers/ldc#installation "LDC LLVM-based D compiler"
[progD]: http://ddili.org/ders/d.en/functions_more.html "Programming in D, More Functions, by Ali Çehreli"
[betterC]: http://dconf.org/2017/talks/arneaud.html "D as a better C by Simon Arneaud"
[pragma]: http://ddili.org/ders/d.en/pragma.html "Programming in D, Pragmas, by Ali Çehreli"
[share]: http://ddili.org/ders/d.en/concurrency_shared.html "Programming in D, Data Sharing Concurrency, by Ali Çehreli"
[aliasParameters]: https://dlang.org/spec/template.html#aliasparameters "Template Alias Parameters"
[nmtool]: https://en.wikipedia.org/wiki/Nm_(Unix) "nm Unix"


