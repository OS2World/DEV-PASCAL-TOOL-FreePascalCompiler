#######################################################################
#
# Makefile to compile all examples and convert them to LaTeX
# 
#######################################################################

# Compiler
PP=ppc386

# Unit directory
# UNITDIR=/usr/lib/ppc/0.99.0/linuxunits


# Any options you wish to pass.
PPOPTS=

# Script to convert the programs to LaTeX examples which can be included.
PP2TEX=../pp2tex

# Script to collect all examples in 1 file.
MAKETEX=make1tex

#######################################################################
# No need to edit after this line.
#######################################################################

ifdef UNITDIR
PPOPTS:=$(PPOPTS) -Up$(UNITDIR);
endif

.SUFFIXES: .pp .tex

.PHONY: all tex clean execute

OBJECTS=ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 \
        ex15 ex16 ex17 ex18 ex19 ex20 ex21 ex22 ex23 ex24 ex25 ex26 ex27 \
        ex28 ex29 ex30 ex31 ex32 ex33 ex34 ex35 ex36 ex37 ex38 ex39 ex40 \
        ex41 ex42 ex43 ex44 ex45 ex46 ex47 ex48 ex49 ex50 ex51 ex52 ex53 \
        ex54 ex55 ex56 ex57 ex58 ex59 ex60 ex61 ex62 ex63 ex64 ex65 ex66 \
        ex67 ex68 ex69 ex70 ex71 ex72 ex73 ex74 ex75 ex76 ex77 ex78 ex79 \
	ex80 ex81 ex82 ex83 ex84 ex85 ex86 ex87 ex88 ex89 \
	ex90 ex91 ex92 ex93 ex94 ex95 ex96 ex97 ex98 ex99 ex100 ex101 ex102\
        ex103 ex104 ex105 ex106 ex107 ex108 ex109 ex110 ex111 ex112

TEXOBJECTS=$(addsuffix .tex, $(OBJECTS))

# This is not the same list as objects, since some of the
# tests are interactive.
TOTEST=ex1 ex2 ex3 ex4 ex5 ex8 ex9 ex10 ex11 ex12 ex13 ex14 \
        ex15 ex16 ex17 ex20 ex21 ex22 ex23 ex25 ex26 ex27 \
        ex28 ex29 ex31 ex32 ex33 ex34 ex36 ex37 ex38 ex39 ex40 \
        ex41 ex42 ex43 ex44 ex45 ex47 ex48 ex52 ex53 \
        ex54 ex56 ex59 ex60 ex62 ex63 ex64 ex65 ex66 \
        ex67 ex68 ex69 ex70 ex71 ex72 ex73 ex75 ex76 ex78 ex79 \
	ex80 ex81 ex82 ex83  ex85 ex86 ex87 ex88 ex89 \
	ex90 ex91 ex92 ex93 ex96 ex97 ex98 ex99 ex100 ex101 ex102\
        ex103 ex104 ex105 ex106 ex107 ex108 ex109 ex110 ex111 ex112
LOGFILE=$(addsuffix .log, $(TOTEST))


all : $(OBJECTS)

tex : $(TEXOBJECTS)

execute: $(LOGFILE)

onetex : tex
	$(MAKETEX) $(TEXOBJECTS)

clean : 
	rm -f *.o *.s *.rst $(OBJECTS) $(TEXOBJECTS)
	rm -f *.ow *.sw *.exe *.dll *.log *.tmp
 
$(OBJECTS): %: %.pp
	$(PP) $(PPOPTS) $*

$(TEXOBJECTS): %.tex: %.pp head.tex foot.tex
	$(PP2TEX) $*

$(LOGFILE): %.log: %.exe 
	$* > $*.log
