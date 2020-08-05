.PHONY: all clean

all:
	(cd src; make)
	(cd test; make)

clean:
	rm -rf *~
	(cd src; make clean)
	(cd test; make clean)
