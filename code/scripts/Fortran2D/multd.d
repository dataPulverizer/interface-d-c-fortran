import std.stdio : writeln;

extern(C){
	double mult_(double* x, double* y);
}

/* To Compile:
** gfortran -c multf.f90
** ldc2 -ofmult multd.d multf.o && ./mult
*/

void main(){
	double x = 4, y = 5;
	writeln(mult_(&x, &y));
}
