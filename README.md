# Interfacing D with C and Fortran

## Introduction

In a [previous article](http://www.active-analytics.com/blog/a-quick-look-at-d/) I mentioned that C has full compatibility with D. In this article we decribe how to interface D with C and Fortran.

## Calling C functions from D.

If you use a Linux system, certain C libraries will installed. These functions can be called from D with ease - actually easier than calling them from C or C++. You do not need to explicitly import the C library where function sits, just declare the function signature under `extern (C)`.

For example a Linux system will have the `math.h` library installed. Below is code for importing the `fabs` and `pow` functions from the C `math.h` library, we have added `@nogc` and `nothrow` because C does not have garbage collection nor does it throw exceptions. We have also included C's `printf` function from the `stdio.h` library and used it to construct a simple template function for printing out an array. The main program uses the `fabs` function on each element of an array and the `pow` function to raise the power of each element to `2.5`:

```
/* article.d */

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
$ ldc2 --betterC -Oz -release -linkonce-templates -run article.d
1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 
1.000000 5.656854 15.588457 32.000000 55.901699 88.181631 
```

`printArray` is used multiple times. We can explicitly mark this function with `pragma(inline, false)`.

`__gshared` is qualifier for global shared data.
It places `x` in the DATA section of object file
the same way like it would be in C.

`extern(C)` was added to declare common C `_main` instead of D `__Dmain`
because we do not use Garbage Collection and Exceptions.
This allows to reduce code size 72 times (from 617.5 KB to 8.5 KB) of the final executable file.

The program example does not require DRuntime to be linked. 
```
$ ldc2 --betterC -nogc -Oz -release -linkonce-templates -c article.d
$ nm article.o
0000000000000000 T __D7article10printArrayFNbNiAdZv
0000000000000050 T _main
0000000000000170 D __D7article1xAd
0000000000000180 d _.constarray
                 U _fabs
                 U _pow
                 U _printf
                 U _putchar
```

Description for symbols in the object file:

 - `__D7article10printArrayFNbNiAdZv ` - `printArray`, D mangling.
 - `_main` - `main`, C maingling.
 - `__D7article1xAd ` - `x`'s length and pointer
 - `_.constarray` - `x`'s data
 - `_fabs`, `_pow`, `_printf` - extern C symbols.
 - `_putchar` - extern C symbol, optimized version for `printf("\n")`


Admit it, this is even easier than [calling C functions from Julia](http://docs.julialang.org/en/stable/manual/calling-c-and-fortran-code/)! The script for the above code is [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/pow_fabs.d).

The use of `scope` in `printf` is to allow the argument type to be limited to the function call. C's `printf` function does not preserve the pointer internally.

### Calling static C libraries from D

Here is my snazzy multiplication function written in C:

```
/* cmult.c */
double mult(double x, double y)
{
    return x * y;
}
```

I would like to compile it and call it from D, here is the code for this:

```
/* dmult.d */
extern(C) @nogc nothrow:

double mult(double x, double y) @safe pure;
int printf(scope const char* format, ...);

int main() // extern(C) too
{
    printf("%f\n", mult(3, 4));
    return 0;
}
```

Evidently you simply need to register the function using the `extern (C)` directive and list functions. Then it's all a matter of compiler magic. I first compile the C and D scripts but not link, then I use the D compiler to compile both together and run:

```
$ gcc -O -c multc.c
$ ldc2 -O -release -betterC multd.d
$ gcc cmult.o dmult.o -omult
$ ./mult
12.000000

# in addition
$ nm cmult.o
0000000000000000 T _mult
$ nm dmult.o
0000000000000000 T _main
                 U _mult
                 U _printf
```

The code is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts) in the `multc.c` and `multd.d` files. For more details, see the [D language website](https://dlang.org/dll-linux.html).

## Calling D functions from C

Calling C from D is a less seamless affair because D has features that are not supported in C. Below is my templated multiplication function written in D. If I want to export it to C, I need create concrete types:

```
extern (C) nothrow @nogc @safe pure:

pragma(inline, true)
T mult(T)(T x, T y)
{
    return x * y;
}

double dmult(double x, double y)
{
    return mult(x, y);
}

float fmult(float x, float y)
{
    return mult(x, y);
}
```

On the C-side we create a script to reference and call the functions we have created in D.

```
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

Simply using D's `alias` to declare different instances of functions will **not** work for exporting to C:

```
/* ... */
alias mult!double dmult;
alias mult!float fmult;
```

The alias `dmult` and `fmult` will not exist in the object file for C. See [this discussion](https://forum.dlang.org/thread/ehdfiatwevdrqejiqaen@forum.dlang.org) for more details.

To compile:

```
$ gcc -O -c multc.c
$ ldc2 -O -release -betterC -linkonce-templates -c multd.d
$ gcc multc.o multd.o -omult
$ ./mult
output: 12.000000
output: 12.000000

# in addition
$ nm multc.o
                 U _dmult
                 U _fmult
0000000000000000 T _main
                 U _printf
$ nm multd.o
0000000000000000 T _dmult
0000000000000010 T _fmult
```

The above code is [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/DfromC).

## Calling FORTRAN code from D

Someone or a group of brave souls have created a high performance numeric library in FORTRAN and you would like to call this library from D. Calling Fortran from C is straightforward, but so is calling Fortran from D. Here is the Fortran version of my multiplication function:

```
SUBROUTINE MULT(x, y)
IMPLICIT NONE
REAL*8, INTENT(IN) :: x
REAL*8, INTENT(INOUT) :: y
y = x*y
END SUBROUTINE MULT
```

The D code for calling Fortran is pretty much the same as calling C, however Fortran's inputs must be referenced. The C-style notation is done with pointers for example the declaration would be `double mult_(double* x, double* y);`, however D provides `ref` notation which allows referenced inputs without requring pointers. You will also notice that the name function on the D side is modified with an underscore after the name:

```
extern (C) nothrow @nogc
{
    double mult_(ref double x, ref double y);
    int printf(scope ref const(char) format, ...);
}

void main(){
    double x = 4, y = 5;
    printf("%f\n", mult_(x, y));
}
```

Compilation is similar to calling C from D:

```
gfortran -c multf.f90
ldc2 -ofmult multd.d multf.o && ./mult
```

The code for the above is located [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/Fortran2D).

In terms of resource I found [this](http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/F90-Subprograms.pdf) useful for creating my Fortran example and [this](http://www.yolinux.com/TUTORIALS/LinuxTutorialMixingFortranAndC.html) useful for compilation hints.

## Calling Fortran code from D with `mixin` magic

In my [previous](http://www.active-analytics.com/blog/a-quick-look-at-d/) blog article we touched on templates in D. In D not only are templates powerful but they are so much more straight forward than in C++. In this section we will use a string mixin to generate wrapper code from Fortran. In D a string mixin allows you to generate code from strings, they are easy to use and very powerful. The only real requirement is that the string needs to be known at compile time.

We would like to port some trigonometric function from Fortran to D, so as before we create subroutines for sine, cosine, tangent,
and arctangent functions:

```
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

We can port these to D using `extern (C)` as before, however we don't really want to write out the same code over and over again so here is a template to generate a string:


```
template Declare(string fun)
{
    enum string Declare = "double " ~ fun ~ "_(ref double x);";
}
```

Note that the `~` operator is for concatenating in D.

Then this, when appropriately compiled:

```
Declare!"cos";
```

gives:

```
double cos_(ref double x);
```

The string representation that we need to import the Fortran function. We can use the `mixin` function
to generate the code we need:

```
extern(C) nothrow @nogc
{
    mixin(Declare!"sin");
    mixin(Declare!"cos");
    mixin(Declare!"tan");
    mixin(Declare!"atan");
}
```

I would like to clean up the functions before use, perhaps have input as `double` rather than `ref double`
and remove the trailing underscore in the name using a wrapper function. So I create another template
function for this:

```
template Wrap(string fun)
{
    enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(x);\n}";
}
```

Appropriately compiling with this:

```
Wrap!"cos";
```

Gives

```
double cos(double x)
{
    return cos_(x);
}
```

The complete declaration is:

```
template Declare(string fun)
{
    enum string Declare = "double " ~ fun ~ "_(ref double x);";
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

extern(C) int main()
{
    double pii = 1;
    immutable double pi = 4 * atan(pii);
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
trig : trigd.d trigf.o
    ldc2 -O -release -betterC -linkonce-templates -oftrig trigd.d trigf.o
trigf.o : trigf.f90
    gfortran -O -c trigf.f90
.PHONY : clean
clean :
    rm trig *.o
```

Then run with `make`.

The code for this section is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/Trig).
