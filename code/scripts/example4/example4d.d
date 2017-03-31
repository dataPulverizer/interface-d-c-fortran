extern (C) nothrow @nogc:

double mult_(ref double x, ref double y) @safe pure;
int printf(scope const char* format, ...);

/* To Compile:
** gfortran -c example4f.f90
** ldc2 -ofexample4 example4d.d example4f.o && ./example4
*/

int main(){
    double x = 4, y = 5;
    printf("%f\n", mult_(x, y));
    return 0;
}