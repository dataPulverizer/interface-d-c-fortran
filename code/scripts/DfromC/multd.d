extern (C) nothrow @nogc:
pragma(LDC_no_moduleinfo);
T mult(T)(T x, T y)
{
    return x*y;
}
double dmult(double x, double y)
{
	return mult(x, y);
}

float fmult(float x, float y)
{
	return mult(x, y);
}
