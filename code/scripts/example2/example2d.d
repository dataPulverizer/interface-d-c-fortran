extern(C) @nogc nothrow:

double mult(double x, double y) @safe pure;
int printf(scope const char* format, ...);

extern(C)
int main()
{
    printf("%f\n", mult(3, 4));
    return 0;
}