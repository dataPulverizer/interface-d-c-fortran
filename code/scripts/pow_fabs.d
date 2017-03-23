import std.stdio: writeln;
import std.algorithm.iteration: sum, map;
import std.array: array;

/* Importing C functions into D */
extern (C){
    double pow(double x, double y);
    double fabs(double x);
}

/* To Compile: ldc2 pow_fabs.d && ./pow_fabs */
void main(){
	double[] x = [-1, 2, -3, 4, -5 , 6];
	x = x.map!(a => fabs(a)).array;
	writeln(x);
	writeln(x.map!(a => pow(a, 2.5)).array);
}
