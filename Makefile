# Makefile to build Cardguesstest testing program

all:	Cardguesstest

OS=Cardguesstest.o Cardguess.o Response.o Card.o State.o Combinations.o

Cardguesstest:	$(OS)
	ghc -O2 -package containers -o $@ $(OS)

%.o %.hi:	%.hs
	ghc -O2 -c $<

Cardguesstest.o:	Card.hi Cardguess.hi Response.hi

Cardguess.o Cardguess.hi: Cardguess.hs State.o Combinations.o
	ghc -O2 -c $<

Response.o:	Card.hi

.PHONY: clean

clean:
	rm -rf *.o *.hi
