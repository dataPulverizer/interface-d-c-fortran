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

Okay so I have a some code written in C. I would like to compile it and call it from D.


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

