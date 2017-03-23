import std.stdio: writeln;

extern(C){
	double mult(double x, double y);
}

/* To Compile:
** gcc -c multc.c
** ldc2 -c multd.d
** ldc2 multd.o multc.o && ./multd
*/

void main()
{
	writeln(mult(3, 4));
}