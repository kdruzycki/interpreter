## File generated by the BNF Converter (bnfc 2.9.4).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean

# Default goal.

all : Main

# Rules for building the parser.

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Main : AbsLatteMalinowe.hs LexLatteMalinowe.hs ParLatteMalinowe.hs PrintLatteMalinowe.hs Utils.hs TypeChecker.hs Globals.hs Evaluator.hs Statements.hs Statements.hs-boot Functions.hs Main.hs
	${GHC} ${GHC_OPTS} -o interpreter $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.hi-boot *.o-boot *.log *.aux *.dvi

# EOF
