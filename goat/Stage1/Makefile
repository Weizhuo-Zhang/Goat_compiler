###############################################################################
#
# Authors:
#   Shizhe Cai (shizhec) - 798125
#   Weizhuo Zhang (weizhuoz) - 1018329
#   Mingyang Zhang (mingyangz) - 650242
#   An Luo (aluo1) - 657605
#
# This is the make file of proejct of COMP90045, Programming Language
# Implementation.
#
###############################################################################

Goat: Goat.hs GoatParser.hs GoatAST.hs GoatPrettyPrint.hs
	ghc Goat.hs -o Goat

clean:
	rm -f *.o *.hi *.out
	rm -f Goat
