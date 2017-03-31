example5 : example5d.d example5f.o
	ldc2 -O -release -betterC -linkonce-templates -ofexample5 example5d.d example5f.o
example5f.o : example5f.f90
	gfortran -c example5f.f90
.PHONY : clean
clean :
	rm example5 *.o
