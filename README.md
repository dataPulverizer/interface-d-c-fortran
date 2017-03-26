# Interfacing D with C and Fortran

## Introduction

In a [previous article](http://www.active-analytics.com/blog/a-quick-look-at-d/) I mentioned that D has full compatibility with C. In this article we describe how to interface D with C and Fortran, and we throw in a brief introduction to string mixins.

## Calling C functions from D.

If you are on Linux, you will have C libraries installed these functions can be called from D with ease - actually easier than calling them from C or C++, you don't even need to explicitly import the C library that the function sits in, just declare the function signature under `extern (C)`. Below we import the `fabs` and `pow` functions from the C `math.h` library.

```
/+ dub.json: {
    "name":"example1",
    "dependencies": {"mir-algorithm":"~>0.1.1"},
    "dflags": ["-betterC", "-linkonce-templates",  "-enable-cross-module-inlining", "-mcpu=native"]
}+/
import mir.ndslice.slice: sliced;
import mir.ndslice.topology: map;
import mir.ndslice.algorithm: each;

/* Importing C functions into D */


extern(C) int printf(scope const char* format, ...) nothrow @nogc;
// alternative:
// import core.stdc.stdio: printf;

version(none2)
{
    // LLVM allows to inline fabs, sqrt and some other functions.
    import ldc.intrinsics: fabs = llvm_fabs, sqrt = llvm_sqrt;
}
else extern(C) nothrow @nogc // common way
{
    // In addition to `nothrow @nogc`,
    // fabs and sqrt functions are `pure @safe`
    double sqrt(double x) pure @safe;
    double fabs(double x) pure @safe;
}

void printArray(S)(S slice)
{
    slice.each!(a => printf("%f ", a));
    printf("\n");
}

/* To Compile and Run:
$ dub --compiler=ldc2 --build=release --single example1.d
or, if mir-algorithm and mir-internal are in the same folder

 1. for object file output
$ ldc2 -Imir-algorithm/source -Imir-internal/source -betterC -O -release -linkonce-templates -enable-cross-module-inlining -mcpu=native -c example1.d
$ nm example1.o
0000000000000120 s _.arrayliteral
0000000000000100 T __D3mir7ndslice5slice18__T6slicedVmi1TAdZ6slicedFNaNbNiAdG1mXS3mir7ndslice5slice52__T5SliceVE3mir7ndslice5slice9SliceKindi2VAmA1i1TPdZ5Slice
0000000000000000 T _main
                 U _printf
                 U _putchar
ls -lh example1.o
-rw-r--r--  1 9il  staff   1.4K Mar 27 02:35 example1.o
// Size of the object file is realy tiny! Only 1.4K

 2. for assembler output
$ ldc2 -Imir-algorithm/source -Imir-internal/source -betterC -O -release -linkonce-templates -enable-cross-module-inlining -mcpu=native -c -output-s example1.d
cat example1.s
 ... second line / loop
LBB0_5:
    vmovsd  (%r12), %xmm0
    vandpd  LCPI0_0(%rip), %xmm0, %xmm0 # fabs
    vsqrtsd %xmm0, %xmm0, %xmm0         # sqrt
    movb    $1, %al
    movq    %r14, %rdi
    callq   _printf                     # printf("%f ", a)
    addq    $8, %r12
    addq    $-1, %r15
    jne LBB0_5
    jmp LBB0_6
LBB0_1:
    movl    $10, %edi
    callq   _putchar                    # printf("\n");
 ...
*/
extern(C) int main()
{
    double[6] data = [-1, 2, -3, 4, -5 , 6];
    auto x = data[].sliced;
    auto absx = x.map!fabs; // lazy view
    absx.printArray;
    absx
        .map!sqrt // lazy view
        .printArray;
    return 0;
}
```
Admit it, this is even easier than [calling C functions from Julia](http://docs.julialang.org/en/stable/manual/calling-c-and-fortran-code/)! The script for the above code is [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/pow_fabs.d).

### Calling static C libraries from D

Here is my snazzy multiplication function written in C:

```
/* multc.c */
double mult(double x, double y)
{
    return x*y;
}
```

I would like to compile it and call it from D, here is the code for this:

```
/* multd.d*/
import std.stdio: writeln;

extern(C){
    double mult(double x, double y);
}

void main()
{
    writeln(mult(3, 4));
}
```
Evidently you simply need to register the function using the `extern (C)` directive and list functions. Then it's all a matter of compiler magic. I first compile the C and D scripts but not link, then I use the D compiler to compile both together and run:

```
# To Compile:
gcc -c multc.c
ldc2 -c multd.d
# Now run
ldc2 multd.o multc.o && ./multd
```
The code is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts) in the `multc.c` and `multd.d` files. For more details of calling C from D, see the [D language website](https://dlang.org/dll-linux.html).

## Calling D functions from C

Calling C from D is a less seamless affair because D has features that are not supported in C. Below is my templated multiplication function written in D. If I want to export it to C, I need create concrete types:

```
extern (C) nothrow @nogc @system:
pragma(LDC_no_moduleinfo);
T mult(T)(T x, T y)
{
    return x*y;
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

As of writing this article, simply using `alias` will **not** work for exporting to C:

```
/* ... */
alias mult!double dmult;
alias mult!float fmult;
```
The alias `dmult` and `fmult` will not exist in the object file for C. See [this discussion](https://forum.dlang.org/thread/ehdfiatwevdrqejiqaen@forum.dlang.org) for more details.

The [`pragma(LDC_no_moduleinfo);`](https://wiki.dlang.org/LDC-specific_language_changes#LDC_no_moduleinfo) stops incompatible features in D from "leaking out". [This discussion](https://forum.dlang.org/thread/bvjfgvgtitrvxpqoatar@forum.dlang.org) was the source for that insight. To compile:

```
gcc -c multc.c
ldc2 -c multd.d
gcc -omult multd.o multc.o && ./mult
```

The `LDC_no_moduleinfo` directive this will only work for the LDC compiler. Here is the code omitting that directive:

```
extern (C) nothrow @nogc @system:
T mult(T)(T x, T y)
{
    return x*y;
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
Then the compilation:

```
gcc -c multc.c
ldc2 -ofmult multd.d multc.o && ./mult
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
The D code for calling Fortran is pretty much the same as calling C, however you will notice that the inputs are pointers and there is an underscore after the called function name:

```
import std.stdio : writeln;

extern(C){
    double mult_(double* x, double* y);
}

void main(){
    double x = 4, y = 5;
    writeln(mult_(&x, &y));
}
```

Compilation is similar to calling C from D:

```
gfortran -c multf.f90
ldc2 -ofmult multd.d multf.o && ./mult
```
Since the inputs are always passed by reference, you don't actually need to have an output, you can just modify one of the inputs. The above code is [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/Fortran2D).

In terms of resource I found [this](http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/F90-Subprograms.pdf) useful for creating my Fortran example and [this](http://www.yolinux.com/TUTORIALS/LinuxTutorialMixingFortranAndC.html) useful for compilation hints.

## Calling D code from Fortran with `mixin` magic

In my [previous](http://www.active-analytics.com/blog/a-quick-look-at-d/) blog article we touched on templates in D. In D not only
are templates powerful but they are so much more straight forward than in C++. In this section we will use a string mixin
to generate wrapper code from Fortran. In D a string mixin allows you to generate code from strings, they are easy to use and very
powerful. The only real requirement is that the string needs to be known at compile time.

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

We can port these to D using `extern (C)` as before, however we don't really want to write out the same code over 
and over again so here is a template to generate a string:


```
template Declare(string fun)
{
    enum string Declare = "double " ~ fun ~ "_(double* x);";
}
```

Note that the `~` operator is for concatenating in D.

Then this, when appropriately compiled:

```
writeln(Declare!"cos");
```

gives:

```
double cos_(double* x);
```

The string representation that we need to import the Fortran function. We can use the `mixin` function
to generate the code we need:

```
extern(C){
    mixin(Declare!"sin");
    mixin(Declare!"cos");
    mixin(Declare!"tan");
    mixin(Declare!"atan");
}
```

I would like to clean up the functions before use, perhaps have input as `double` rather than `double*`
and remove the trailing underscore in the name using a wrapper function. So I create another template
function for this:

```
template Wrap(string fun)
{
    enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(&x);\n}";
}
```

Appropriately compiling with this:

```
writeln(Wrap!"cos");
```

Gives

```
double cos(double x)
{
    return cos_(&x);
}
```

The complete declaration is:

```
import std.stdio : writeln;

template Declare(string fun)
{
    enum string Declare = "double " ~ fun ~ "_(double* x);";
}

extern(C){
    mixin(Declare!"sin");
    mixin(Declare!"cos");
    mixin(Declare!"tan");
    mixin(Declare!"atan");
}

template Wrap(string fun)
{
    enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(&x);\n}";
}


mixin(Wrap!"sin");
mixin(Wrap!"cos");
mixin(Wrap!"tan");
mixin(Wrap!"atan");


void main(){
    double pii = 1;
    immutable double pi = 4*atan(pii);
    assert(sin(pi/2) == 1);
    assert(cos(0) == 1);
    assert(tan(0) == 0);
    writeln("Yay!");
}
```

It doesn't take too much imagination to realise that you can use D code to generate the Fortran string, compile it and generate
the relevant D wrapper code. Yes it is possible to do this in C/C++ using nasty macros or maybe template programming (I wouln't
like to try), but hopefully you can see that this kind of thing is pretty straightforward in D, because it was designed with
meta-progamming in mind. We can compile the Fortran and D code with make though D has it's own package manager called 
[DUB](https://code.dlang.org/getting_started). The make code is below:

```
trig : trigd.d trigf.o
	ldc2 -oftrig trigd.d trigf.o
trigf.o : trigf.f90
	gfortran -c trigf.f90
.PHONY : clean
clean :
	rm *.o
```

Then run with `make`.

The code for this section is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts/trig).

## Conclusion

The C programming language is sometimes called the lingua franca of the programming world and any C library can be called from D. We have shown that D can call not only C libraries but also Fortran subroutines. We also introduced string mixins for code generation, and showed that this is straightforward to use these to generate code.

