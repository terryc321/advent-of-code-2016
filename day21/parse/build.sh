#!/bin/bash


antlr4='java -jar /usr/local/lib/antlr-4.13.1-complete.jar'
grun="java org.antlr.v4.gui.TestRig"

# antlr4 not known ?

antlr4 Parse.g4 && javac *.java && grun Parse r -tree < test-001.txt
antlr4 Parse.g4 && javac *.java && grun Parse r -tree < test-002.txt
antlr4 Parse.g4 && javac *.java && grun Parse r -tree < test-003.txt
antlr4 Parse.g4 && javac *.java && grun Parse r -tree < test-004.txt
antlr4 Parse.g4 && javac *.java && grun Parse r -tree < test-005.txt

antlr4 Parse.g4 && javac *.java && grun Parse r -tree < ../input > output




