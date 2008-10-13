###########################################################################
#
# Parameters
#

contrib 	= ../contrib
sfLib   	= $(contrib)/StrategyLib-5.0
sfModel  = drift-default
sfPath   = $(sfLib)/library:$(sfLib)/models/$(sfModel)
shPath   = $(contrib)/Sdf2Haskell-2.3/generator
atLib    = $(contrib)/haterm-1.5
atPath   = $(atLib)/library
huPath   = $(contrib)/HUnit-1.0
haxml    = $(contrib)/HaXml-1.12/src/
uhs      = $(contrib)/UMinhoHaskellSoftware
hsPath  	= :$(uhs):$(sfPath):$(shPath):$(atPath):$(huPath):$(haxml)

# Don't lose generated Haskell files after make finishes
.PRECIOUS: *.hs


###########################################################################
#
# Targets to compile 
#

all: SdfMetz

###########################################################################
#
# Compilation with GHC
#

GHC     = ghc
GHCOPTS = -fglasgow-exts                \
          -fallow-overlapping-instances \
          -fallow-undecidable-instances \
          -i${hsPath}                   \
          -hidir hidir                  \
          -odir odir
#          -package data	                \
#          -package util                 \
#          -package lang                 \

%: %.hs ${wildcard *.hs *.lhs} tmpdirs
	${GHC} ${GHCOPTS} --make $*.hs -o $@

%.cgi: %.hs ${wildcard *.hs *.lhs} tmpdirs
	${GHC} ${GHCOPTS} -package WASH --make $*.hs -o $@

# Creates folders to hold temporary compiled files
tmpdirs:
	mkdir -p odir
	mkdir -p hidir


ghci:
	ghci ${GHCOPTS} 


###########################################################################
#
# Put on-line
#

cgiPrograms = SdfMetzOnline.cgi
cgiBinDir = /Library/WebServer/CGI-Executables

online: ${cgiPrograms}
	cp ${cgiPrograms} ${wildcard *.tbl} ${cgiBinDir}


###########################################################################
#
# Cleaning supoerfluous files
#

.PHONY: clean
clean:
	-@rm -rvf examples/*.af examples/*.asfix SdfMetz SdfDoc

very-clean: clean
	-@rm -rvf hidir odir

###########################################################################
#
# Distribution
#

distName        = SdfMetz
distVersion     = $(shell date "+%Y.%m.%d")
distFiles	= README TODO Makefile $(wildcard *.hs) doc/ SdfGrammars/
distFullName	= $(distName)-$(distVersion)

dist:
	@echo "Creating distribution $(distFullName)"
	@rm -rf $(distFullName)
	@mkdir -p $(distFullName)
	@cp -R $(distFiles) $(distFullName)
	@zip -q -r $(distFullName).zip $(distFullName)
	@tar zcf $(distFullName).tgz $(distFullName)
	@rm -rf $(distFullName)
	@echo "  READY for distribution:"
	@echo "    $(distFullName).zip"
	@echo "    $(distFullName).tgz"

###########################################################################
#
# Debugging
#

echo-%:
	@echo $($*)

###########################################################################
