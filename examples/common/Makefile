FC=gfortran
#FC=ifort

BINNAME=common_example.out

FLAGS=-Wall
MODFLAGS=-Wall -c
SRCROOT=../../src

$(BINNAME): main.f90 libtt_print.mod libtt_precision.mod libtt_common.mod
	$(FC) $(FLAGS) -o $(BINNAME) main.f90 $(SRCROOT)/libtt_print.f90 $(SRCROOT)/libtt_precision.f90 $(SRCROOT)/libtt_common.f90

libtt_common.mod: $(SRCROOT)/libtt_common.f90 libtt_precision.mod
	$(FC) $(MODFLAGS) $(SRCROOT)/libtt_common.f90

libtt_print.mod: $(SRCROOT)/libtt_print.f90 libtt_precision.mod
	$(FC) $(MODFLAGS) $(SRCROOT)/libtt_print.f90

libtt_precision.mod: $(SRCROOT)/libtt_precision.f90
	$(FC) $(MODFLAGS) $(SRCROOT)/libtt_precision.f90

.PHONY: clean

clean:
	rm $(BINNAME) *.mod *.o