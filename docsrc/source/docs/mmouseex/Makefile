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

.PHONY: all tex clean

OBJECTS=mouse1 mouse2 mouse3 mouse4 mouse5 mouse6 mouse7 mouse8 mouse9 \
        mouse10

TEXOBJECTS=$(addsuffix .tex, $(OBJECTS))

all : $(OBJECTS)

tex : $(TEXOBJECTS)

onetex : tex
	$(MAKETEX) $(TEXOBJECTS)

clean : 
	rm -f *.o *.s $(OBJECTS) $(TEXOBJECTS)
	rm -f *.ow *.sw *.exe *.dll
 
$(OBJECTS): %: %.pp
	$(PP) $(PPOPTS) $*

$(TEXOBJECTS): %.tex: %.pp head.tex foot.tex
	$(PP2TEX) $*
