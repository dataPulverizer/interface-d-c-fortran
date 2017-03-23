# Interfacing D with C and Fortran

## Introduction

In a [previous article](http://www.active-analytics.com/blog/a-quick-look-at-d/) I mentioned that D has full compatibility with C. In this article we decribe how to interface D with C and Fortran.

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
because C has no equivalent to the D dynamic array type.

