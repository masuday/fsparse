include ../Make.inc

.PHONY: all clean

all: test_ordering.x

test_ordering.o: test_ordering.f90
	$(FC) $(FCFLAGS) -c $< -o $@

test_ordering.x: test_ordering.o
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	rm -rf *~ *.o *.a *.mod *.x a.out
