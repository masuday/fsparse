include ../Make.inc

.PHONY: all clean

all: libfsparse.a

libfsparse.a: fsparse.o
	ar cr libfsparse.a fsparse.o

fsparse.o: fsparse.f90
	$(FC) $(FCFLAGS) -c $< -o $@

clean:
	rm -rf *~ *.o *.a *.mod a.out
