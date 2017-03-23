#include <stdio.h>

extern double dmult(double x, double y);
extern float fmult(float x, float y);;

/* To Compile:
** gcc -c multc.c
** ldc2 -c multd.d
** gcc -omult multd.o multc.o && ./mult
*/

int main()
{
	double xd = 3.0, yd = 4.0;
	float xf = 3.0, yf = 4.0;
	printf("output: %f\n", dmult(xd, yd));
	printf("output: %f\n", fmult(xf, yf));
    return 0;
}
