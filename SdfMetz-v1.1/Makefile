###########################################################################
#
# Parameters
#

libs    	= .
contrib 	= contrib
sfLib   	= $(contrib)/StrategyLib-4.0-beta
sfModel  = drift-default
sfPath   = $(sfLib)/library:$(sfLib)/models/$(sfModel)
shPath   = $(contrib)/Sdf2Haskell-2.2/generator
atLib    = $(contrib)/haterm-1.4
atPath   = $(atLib)/library
huPath   = $(contrib)/HUnit-1.0
hsPath  	= :$(libs):$(sfPath):$(shPath):$(atPath):$(huPath)

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
          -package data	                \
          -i${hsPath}

%: %.hs ${wildcard *.hs *.lhs}
	mkdir -p odir
	mkdir -p hidir
	${GHC} ${GHCOPTS} -hidir hidir -odir odir --make $*.hs -o $@


Sdf.tbl: Language/Sdf/Sdf.def
	sdf2table -i $< -o $@


###########################################################################
#
# Cleaning supoerfluous files
#

.PHONY: clean
clean:
	-@rm -rvf SdfMetz

very-clean:
	-@rm -rvf odir hidir SdfMetz


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
