extern(C) @nogc nothrow
{
	double mult(double x, double y);
	int printf(scope const char* format, ...);
}

/* To Compile:
** gcc -c multc.c
** ldc2 multd.d multc.o && ./multd
*/

void main()
{
	printf("%f\n", mult(3, 4));
}