module scal_dot;

import std.stdio;

T OFFSET(T)(in T N, in T incX)
{
	return incX > 0 ?  0 : ((N - 1) * -incX);
}


/**
*  @title scal blas function: Computes the product of a vector by a scalar
*  @description Computes the product of a vector by a scalar x = a*x
*               where a is a scala and x is an n-element vector
*  @param n The number of elements in vector x
*  @param a The scala a
*  @param x Pointer to array size at least (1 + (n + 1)*abs(incx))
*  @param incx Specifies the increment for the elements of x
*  @return void but the input array x is now multiplied by a
*  @example
*           double[] x = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0];
*           scal(x.length, 3.0, x.ptr, 1);
*           writeln(x);
*/
void scal(N, X)(in N n, in X a, X* x, in N incX)
{
	N ix = OFFSET(n, incX);
	for(int i = 0; i < n; ++i)
	{
		x[ix] *= a;
		ix += incX;
	}
}


/**
*  @title dot blas function: Computes a vector-vector dot product
*  @description The dot blas routine computes the inner product of two vectors
*               the accumulation of the intermediate results is the same type as
*               the elements of the inputs
*  @param n The number of elements in vectors x and y
*  @param x Pointer to array size at least (1 + (n + 1)*abs(incx))
*  @param y Pointer to array size at least (1 + (n + 1)*abs(incy))
*  @param incx Specifies the increment for the elements of x
*  @param incy Specifies the increment for the elements of y
*  @return returns dot product of vectors x and y
*  @example
*           double[] x = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0];
*           double[] y = [7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0];
*           writeln(dot(y.length, x.ptr, 1, y.ptr, 1));
*
*/
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
