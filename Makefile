# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler

# Useful locations

SOURCE_DIR	 := src
OUTPUT_DIR	 := bin

# Project tools

MKDIR	:= mkdir -p
RM	:= rm -rf
SBT := sbt

# The make rules:

# run the antlr build script then attempts to compile all .java files within src/antlr
all:
	$(MKDIR) $(OUTPUT_DIR)
	$(SBT) assembly

# clean up all of the compiled files
clean:
	$(RM) $(OUTPUT_DIR)

.PHONY: all clean
