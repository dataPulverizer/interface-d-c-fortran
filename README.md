# Interfacing D with C and Fortran

## Introduction

In a [previous article](http://www.active-analytics.com/blog/a-quick-look-at-d/) I mentioned that D has full compatibility with C. In this article we decribe how to interface D with C and Fortran.

## Calling C functions from D.

If you are on Linux, you will have C libraries installed these functions can be called from D with ease - actually easier than calling them from C or C++, you don't even need to explicitly import the C library that the function sits in. Just declare the function signature under `extern (C)`. Below we import the `fabs` and `pow` functions from the C `math.h` library.

```
import std.stdio: writeln;
import std.algorithm.iteration: sum, map;
import std.array: array;

/* Importing C functions into D */
extern (C){
    double pow(double x, double y);
    double fabs(double x);
}

/* To Compile: ldc2 calling_c.d && ./calling_c */
void main(){
	double[] x = [-1, 2, -3, 4, -5 , 6];
	x = x.map!(a => fabs(a)).array;
	writeln(x);
	writeln(x.map!(a => pow(a, 2.5)).array);
}
```
Admit it, this is even easier than [calling C functions from Julia](http://docs.julialang.org/en/stable/manual/calling-c-and-fortran-code/)! The script for the above code is [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts/pow_fabs.d).

### Calling static C libraries from D

Okay so I have a some code written in C. Here is my snazzy multiplication function written in C:

```
/* multc.c */
double mult(double x, double y)
{
	return x*y;
}
```

I would like to compile it and call it from D so I write this function:

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
I first compile the C and D scripts but not link, then I use the D compiler to compile both together:

```
# To Compile:
gcc -c multc.c
ldc2 -c multd.d
# Now run
ldc2 multd.o multc.o && ./multd
```
The code is given [here](https://github.com/dataPulverizer/interface-d-c-fortran/blob/master/code/scripts) in the `multc.c` and `multd.d` files.

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

As of writing this article, simply using `alias` will not work for exporting to C:

```
/* ... */
alias mult!double dmult;
alias mult!float fmult;
```
In D the `alias` instantiated `dmult` and `fmult` would function as intended, however these can not be exported correctly to C. The [`pragma(LDC_no_moduleinfo);`](https://wiki.dlang.org/LDC-specific_language_changes#LDC_no_moduleinfo) stops incompatible features in D from "leaking out". [This discussion](https://forum.dlang.org/thread/bvjfgvgtitrvxpqoatar@forum.dlang.org) was the source for that insight. To compile:

```
gcc -c multc.c
ldc2 -c multd.d
gcc -omult multd.o multc.o && ./mult
```

The `LDC_no_moduleinfo` directive this will only work for the LDC compiler. So alternatively, you can do the first stage of the C compilation and the last and final stage using any D compiler instead of the `gcc` compiler without the pragma directive. Meaning that the D code becomes

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
and you compile with this:

```
gcc -c multc.c
ldc2 -ofmult multd.d multc.o && ./mult
```

## The D code

In the same previous article, we created two simplified D functions for two BLAS routines `scal` (scaling an array by a constant) and `dot` (dot product of two arrays). Below is the code for the full implementation of the functions:

```
module scal_dot;

T OFFSET(T)(in T N, in T incX)
{
	return incX > 0 ?  0 : ((N - 1) * -incX);
}

void scal(N, X)(in N n, in X a, X* x, in N incX)
{
	N ix = OFFSET(n, incX);
	for(int i = 0; i < n; ++i)
	{
		x[ix] *= a;
		ix += incX;
	}
}


X dot(N, X)(in N n, X* x, in N incX, X* y, in N incY)
{
	N ix = OFFSET(n, incX);
	N iy = OFFSET(n, incY);
	auto res = X(0);
	for(int i = 0; i < n; ++i)
	{
		res += x[ix]*y[iy];
		ix += incX;
		iy += incY;
	}
	return res;
}
```
The function descriptions were removed for brevity, see our Github for full details.
The `OFFSET` function allows `incY` and `incX` to be negative, the functions are modelled after 
the [GNU Scientific Library](https://www.gnu.org/software/gsl/). Notice that pointers are used in the function declarations
because C has no equivalent to the D dynamic array type. The above code is located in [here](https://github.com/dataPulverizer/interface-d-c-fortran/tree/master/code/scripts).

