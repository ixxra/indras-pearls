
FC=gfortran
#FLAGS= -malign_double
#FLAGS= -melf_i386
#FLAGS= -mi386linux
FFLAGS=$(FLAGS) -O -Wimplicit -extend-source=132
CFLAGS=$(FLAGS) 

CLIBS=-lm

KLEINOBJS = mobius.o tree.o group.o chain.o bndy.o bndypt.o mufind.o mulist.o draw.o funcs.o poincare.o colors.o psgraph.o kleinian.o

BINDIR = $(PWD)/../bin
# PostScript Header File
HEADER = "'$(PWD)/psgraph.pro'"
# Location of default FSA files
FSADIR = "'$(PWD)/FSA/'"
# Limit on number of stored boundary points
MAXBNDY=10000
# Limit on number of solutions found for cusp
MAXCUSP=1000
# Limit on number of sides in fundamental polygon
MAXSIDE=10
# Limit on level in tree search
MAXLEV=100000
# Limit on number of states in FSA
MAXSTATE=200
# Limit on number of arcs in figure or tile
MAXARC=200
# Limit on length of beginning, ending, or special word
MAXLEN=500
# Limit on number of fixed points with given one-letter prefix
MAXFP=1000
# Limit on length of p/q chain curve of points
MAXCHAIN=100000
# NUMBER HORIZONTAL PIXELS IN COLOR IMAGE
ISIZE=3072
# NUMBER HORIZONTAL PIXELS IN COLOR IMAGE
JSIZE=1024

DEFINES=-DBINDIR=$(BINDIR) -DHEADER=$(HEADER) -DFSADIR=$(FSADIR)\
	-DMAXBNDY=$(MAXBNDY) -DMAXCUSP=$(MAXCUSP)\
	-DMAXSIDE=$(MAXSIDE) -DMAXLEV=$(MAXLEV) -DMAXSTATE=$(MAXSTATE)\
	-DMAXARC=$(MAXARC) -DMAXLEN=$(MAXLEN) -DMAXFP=$(MAXFP)\
	-DMAXCHAIN=$(MAXCHAIN) -DISIZE=$(ISIZE) -DJSIZE=$(JSIZE)

%.o: %.F
	$(FC) -c $(FFLAGS) $(DEFINES) $< -o $@

kleinian: $(KLEINOBJS) 
	$(FC) $(FFLAGS) $(KLEINOBJS) -o kleinian

install: kleinian
	cp -f kleinian $(BINDIR)

debug:
	make clean
	make kleinian FLAGS="-g3"

clean:
	rm -f *.o *.out *% *~ kleinian

