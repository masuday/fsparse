.PHONY: all clean

all:
	(cd AMD; make)
	(cd src; make)
	(cd test; make)

clean:
	rm -rf *~
	(cd AMD; make clean)
	(cd src; make clean)
	(cd test; make clean)
