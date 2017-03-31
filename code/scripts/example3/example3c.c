#include <stdio.h>

extern double dmult(double x, double y);
extern float fmult(float x, float y);;

/* To Compile:
** gcc -c example3c.c
** ldc2 -c example3d.d
** gcc -oexample3c example3d.o example3c.o && ./example3
*/

int main()
{
	double xd = 3.0, yd = 4.0;
	float xf = 3.0, yf = 4.0;
	printf("output: %f\n", dmult(xd, yd));
	printf("output: %f\n", fmult(xf, yf));
    return 0;
}
