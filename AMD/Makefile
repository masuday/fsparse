include ../Make.inc

.PHONY: all clean

all: libamd.a

libamd.a: amd.o amdbar.o amd64.o amdbar64.o
	ar cr libamd.a amd.o amdbar.o amd64.o amdbar64.o
	cp libamd.a ../lib

amd.o: amd.f
	$(FC) $(FCFLAGS) -c $< -o $@

amdbar.o: amdbar.f
	$(FC) $(FCFLAGS) -c $< -o $@

amd64.o: amd64.f
	$(FC) $(FCFLAGS) -c $< -o $@

amdbar64.o: amdbar64.f
	$(FC) $(FCFLAGS) -c $< -o $@

clean:
	rm -f *.o *.a *~
