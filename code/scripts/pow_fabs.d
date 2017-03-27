extern (C) @nogc nothrow
{
    double pow(double x, double y);
    double fabs(double x);
    int printf(scope const char* format, ...);
}

void printArray(X: U[], U)(X arr)
{
    foreach(el; arr)
    {
        printf("%f ", el);
    }
    printf("\n");
}

/* To Compile: ldc2 calling_c.d && ./calling_c */
void main(){
	double[] x = [-1, 2, -3, 4, -5 , 6];
	foreach(i, el; x)
	{
	    x[i] = fabs(el);
    }
	printArray(x);
	foreach(i, el; x)
	{
	    x[i] = pow(el, 2.5);
    }
	printArray(x);
}