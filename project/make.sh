#!/bin/bash
set -e

ml-lex PlcLexer.lex
ml-yacc PlcParser.yacc
sml Plc.sml