FC=gfortran
#FC=ifort

FLAGS=-Wall -o pretty_print
MODFLAGS=-Wall -c
SRCROOT=../../src

pretty_print: main.f90 libtt_print.mod libtt_precision.mod
	$(FC) $(FLAGS) main.f90 $(SRCROOT)/libtt_print.f90 $(SRCROOT)/libtt_precision.f90

libtt_print.mod: $(SRCROOT)/libtt_print.f90 libtt_precision.mod
	$(FC) $(MODFLAGS) $(SRCROOT)/libtt_print.f90

libtt_precision.mod: $(SRCROOT)/libtt_precision.f90
	$(FC) $(MODFLAGS) $(SRCROOT)/libtt_precision.f90

.PHONY: clean

clean:
	rm pretty_print *.mod *.o