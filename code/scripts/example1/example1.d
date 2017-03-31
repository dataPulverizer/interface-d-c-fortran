@nogc nothrow: 

/* Declare C functions */
extern(C)
{
    double pow(double x, double y) @safe;
    double fabs(double x) pure @safe;
    int printf(scope const char* format, ...);
}

pragma(inline, false)
void printArray(double[] arr)
{
    foreach(elem; arr)
        printf("%f ", elem);
    printf("\n");
}

pragma(inline, true)
void apply(alias fun)(double[] arr)
{
    foreach(ref elem; arr)
        elem = fun(elem);
}

__gshared double[] x = [-1, 2, -3, 4, -5 , 6];

/* To compile: 
            ldc2 -betterC -Oz -release -linkonce-templates -run example1.d
*/

extern(C)
int main()
{
    apply!fabs(x);
    printArray(x);
    apply!(a => pow(a, 2.5))(x);
    printArray(x);
    return 0;
}
