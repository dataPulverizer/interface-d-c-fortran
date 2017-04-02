# Interfacing D with C and Fortran

## Introduction

The C and Fortran programming languages have many popular libraries that are heavily relied upon. D's generic and meta-programming features, as well as its multi-paradigm nature and clear syntax makes it a great choice for various applications. Many programming languages allow varying degrees of compatibility with C and Fortran libraries. However C libraries have full compatibility with D, and Fortran subroutines can be called from D. This article describes how to interface D code with C and Fortran. This includes outlining the ease in which functions in C standard libraries can be accessed from D, calling functions from C static libraries, calling D functions from C, and calling Fortran subroutines from D. The article also covers how to use D's string mixins to generate code to avoid repetitive writing of wrapper functions and declarations.

The D code in this article uses `-betterC` style. This mode of programming is increasingly popular in D circles focusing on D as a replacement for C in systems programming. The [`-betterC`][betterC] flag in the D compiler uses a ligth-weight subset of D and has the side-effect of facilitating an integration of D code to C that is as seamless as calling C code from D.

The examples presented here are run on a Linux Ubuntu 64-bit 16.04 system and use the [gcc 5.4.0][gcc] C and Fortran compilers and [ldc2][ldc2] D's LLVM-based D compiler.

## Calling C functions from D.

The math.h header describes a standard library in C. [Example1 code](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/example1) below shows the D code for importing the `fabs` and `pow` functions from this library. The `extern (C)` braces contain functions to be imported from C. The qualifiers `nogc` and `nothrow` are used because the imported C functions are not garbage collected and do not throw exceptions.

In general the C language all too easily allows unsafe actions one of which is corrupting data. Some of the imported functions are modified with `@safe` which [ensures that they do not carry out a number of potentially unsafe actions][progD]. The `printf` function is not marked with `@safe` but instead the `scope` statement is used because the pointer is not preserved internally.

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

The following are helpful descriptors of the code above:

* Full descriptions of the flags for the `ldc2` compiler are given with the `-help` flag, but the current flags are to reduce code size and bloat and create light-weight executables.

* If you are new to D the `apply` function is an example of using meta-programming techniques to pass a function using the template [alias parameters][aliasParameters].

* Purity of functions can be enforced in D using the `pure` qualifier to indicate that the function can not have side-effects which is the case with the `fabs` function.

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

The D code to call this similar to our previous example:

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

There are many ways to compile these two scripts. Below the C and D scripts are compiled into separate object files. Then both object files are compiled together with the `gcc` compiler. Another option is to compile the C code to an object file and then use the D compiler to compile the C object file together with the `.d` script(s) to generate the executable.

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

The code above is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/example2).

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

The above code is located [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/example3).

## Calling FORTRAN code from D

There are many numeric libraries written in Fortran that are still frequently used. It is thus important that they can be accessed from D. Fortran subroutines can be called directly from D in a very similar way to calling C from D. Below is a Fortran subroutine equivalent of the multiplication function:

```
!example4f.f90
SUBROUTINE MULT(x, y)
IMPLICIT NONE
REAL*8, INTENT(IN) :: x
REAL*8, INTENT(INOUT) :: y
y = x*y
END SUBROUTINE MULT
```

The main difference between calling C code and Fortran code from D is that the inputs to the Fortran subroutines must be referenced and the name of the function is mangled in the Fortran object file to include an underscore afterwards. C-style notation can be used `double mult_(double* x, double* y);`, however D provides `ref` notation which allows referenced inputs without requring pointers:

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

### Calling Fortran code from D with mixins

Consider a situation where Fortran subroutines of the same signature but with different names need to be called in D. D can be used to generate the necessary declarative and wrapper code to avoid repetitive code. As an example consider porting the the following trigonometric subroutines from Fortran:

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

The D code we need generates strings at compile time. These strings are compile time "interpreted" into functions. This is done using string mixins and templates to generate the compile time strings.

The first template is used to generate the declarations under `extern (C)`:

```
template Declare(string fun)
{
    enum string Declare = "double " ~ fun ~ "_(ref double x) pure;";
}
```

`Declare!"sin"` will generate the string `double sin_(ref double x) pure;` which declares the ported Fortran `sin_` function. Next is the wrapper function to allow the user to use `sin(x)` rather than `sin_(x)`:

```
template Wrap(string fun)
{
    enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(x);\n}";
}
```
Then a template function that recursively concatenates the outputs for many functions to generate one string for all the functions is given below.

```
template GenFuns(string[] funs, alias wrapper)
{
    static if(funs.length > 0)
        enum string GenFuns = wrapper!(funs[0]) ~ GenFuns!(funs[1..$], wrapper);
    else
        enum string GenFuns = "";
}
```

The complete D script for this is given below:

```
/* example5d.d */
template Declare(string fun)
{
    enum string Declare = "double " ~ fun ~ "_(ref double x) pure;";
}

template Wrap(string fun)
{
    enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(x);\n}";
}

template GenFuns(string[] funs, alias wrapper)
{
    static if(funs.length > 0)
        enum string GenFuns = wrapper!(funs[0]) ~ GenFuns!(funs[1..$], wrapper);
    else
        enum string GenFuns = "";
}

/* Name of the functions to be ported */
immutable(string)[4] trigFuns = ["sin", "cos", "tan", "atan"];

extern(C) nothrow @nogc
{
    int printf(scope const char* format, ...);
    mixin(GenFuns!(trigFuns, Declare));
}

mixin(GenFuns!(trigFuns, Wrap));

extern (C):
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

The `make` code is below:

```
example5 : example5d.d example5f.o
    ldc2 -O -release -betterC -linkonce-templates -oftrig example5d.d example5f.o
example5f.o : example5f.f90
    gfortran -O -c example5f.f90
.PHONY : clean
clean :
    rm example5 *.o
```
After running `make`, `./example5` will return the output `"Yay!"`

The code for this section is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/example5).

## Summary

This article shows that D can interface with C and Fortran simply and efficiently. In the case of calling C functions from D, there is minimal effort required in that the function(s) need only be declared under `extern (C)`. In the case of calling D code from C, efforts need to be made to remove features in D that are incompatible with C using `-betterC` and other appropriate flags. Calling Fortran libraries from D is almost as strightforward as calling C from D, however Fortran mangles the exported names with a postfix underscore and arguments on the D code side must be referenced either using `ref` or with C-style pointers. D's mixins can be used to generate repetitive sections of code that can make it easier to port functions that have the same general call structure.

[quickD]: http://www.active-analytics.com/blog/a-quick-look-at-d/  "A quick look at D"
[gcc]: https://gcc.gnu.org "GNU C Collection"
[ldc2]: https://github.com/ldc-developers/ldc#installation "LDC LLVM-based D compiler"
[progD]: http://ddili.org/ders/d.en/functions_more.html "Programming in D, More Functions, by Ali Çehreli"
[betterC]: http://dconf.org/2017/talks/arneaud.html "D as a better C by Simon Arneaud"
[pragma]: http://ddili.org/ders/d.en/pragma.html "Programming in D, Pragmas, by Ali Çehreli"
[share]: http://ddili.org/ders/d.en/concurrency_shared.html "Programming in D, Data Sharing Concurrency, by Ali Çehreli"
[aliasParameters]: https://dlang.org/spec/template.html#aliasparameters "Template Alias Parameters"
[nmtool]: https://en.wikipedia.org/wiki/Nm_(Unix) "nm Unix"


