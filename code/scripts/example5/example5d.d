template Declare(string fun)
{
	enum string Declare = "double " ~ fun ~ "_(ref double x) pure;";
}

template Wrap(string fun)
{
	enum string Wrap = "double " ~ fun ~ "(double x)\n{\n    return " ~ fun ~ "_(x);\n}";
}

template GenFuns(string[] funs, alias wrapper)
{
	static if(funs.length > 0)
	    enum string GenFuns = wrapper!(funs[0]) ~ GenFuns!(funs[1..$], wrapper);
	else
		enum string GenFuns = "";
}

/* Name of the functions to be ported */
immutable(string)[4] trigFuns = ["sin", "cos", "tan", "atan"];

extern(C) nothrow @nogc
{
	int printf(scope const char* format, ...);
	mixin(GenFuns!(trigFuns, Declare));
}

mixin(GenFuns!(trigFuns, Wrap));


/* To Compile:
** gfortran -c example5f.f90
** ldc2 -ofexample5 example5d.d example5f.o && ./example5
*/

extern (C):
int main(){
	double pii = 1;
    immutable double pi = 4*atan(pii);
    assert(sin(pi/2) == 1);
	assert(cos(0) == 1);
	assert(tan(0) == 0);
	printf("Yay!\n");
	return 0;
}
