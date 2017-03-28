extern (C) nothrow @nogc
{
    double mult_(ref double x, ref double y);
    int printf(scope const char* format, ...);
}

/* To Compile:
** gfortran -c multf.f90
** ldc2 -ofmult multd.d multf.o && ./mult
*/

void main(){
	double x = 4, y = 5;
	printf("%f\n", mult_(x, y));
}
