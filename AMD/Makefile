include ../Make.inc

.PHONY: all clean

all: libamd.a

libamd.a: amd.o amdbar.o
	ar cr libamd.a amd.o amdbar.o
	cp libamd.a ../lib

amd.o: amd.f
	$(FC) $(FCFLAGS) -c $< -o $@

amdbar.o: amdbar.f
	$(FC) $(FCFLAGS) -c $< -o $@

clean:
	rm -f *.o *.a *~
