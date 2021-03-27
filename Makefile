#------------------------------------------------------------------------------
# Project: DKA-2-MKA (Deterministic Finite Automata to Minimal Finite Automata)
#          Functional and Logical Programming 2020 / 2021
# Author:  Martin Smutny, xsmutn13
# Date:    03.03.2021
#
# Makefile for compilation:
#	the binary 'dka-2-mka' is put in the same directory as this Makefile
#-------------------------------------------------------------------------------

# TODO Merlin test

GHC = ghc
FLAGS = -O2 

#-------------------------------------------------------------------------------
# Directories

SRC_DIR = src
TEST_DIR = test

#-------------------------------------------------------------------------------
# Files
BIN = dka-2-mka
TEST_SC = $(TEST_DIR)/run_tests.sh

MAIN = $(SRC_DIR)/Main.hs
TYPES = $(SRC_DIR)/Types.hs
PARSE = $(SRC_DIR)/ParseInput.hs
MIN = $(SRC_DIR)/Minimize.hs

FILES = $(MAIN) $(TYPES) $(PARSE) $(MIN)

UNIT = $(SRC_DIR)/UnitTest.hs
BINU = unitTest

#-------------------------------------------------------------------------------
# Labels
.PHONY: all run test clean unit

all: $(BIN)

$(BIN): $(FILES)
	$(GHC) $(FLAGS) -o $(BIN) $^

run: $(BIN)
	./$(BIN)

test: $(BIN) $(TEST_SC)
	./$(TEST_SC)

unit: $(UNIT) $(TYPES) $(MIN)
	$(GHC) $(FLAGS) -o $(BINU) $^
	time ./$(BINU)

clean:
	rm $(BIN) $(SRC_DIR)/*.hi $(SRC_DIR)/*.o $(TEST_DIR)/*.temp 
	   $(TEST_DIR)/*.err $(BINU)

