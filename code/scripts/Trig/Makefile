trig : trigd.d trigf.o
	ldc2 -oftrig trigd.d trigf.o
trigf.o : trigf.f90
	gfortran -c trigf.f90
.PHONY : clean
clean :
	rm trig *.o
