#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"

int main(int argc, char **argv)
{
    char *inbuf = loadFile("tests/testgc.c");
    puts(inbuf);

    // Tokenize the input
    Token *tokens = tokenize(inbuf);
    // showTokens(tokens);

    // Create the global symbol table
    pushDomain();

    vmInit();

    // Parse the tokens
    parse(tokens);

    Symbol *symMain = findSymbolInDomain(symTable, "main");
    if (!symMain)
        err("missing main function");
    Instr *entryCode = NULL;
    addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
    addInstr(&entryCode, OP_HALT);
    run(entryCode);

    // Instr *testCode = genTestProgram();
    // run(testCode);

    // Show the symbol table
    // showDomain(symTable, "global");

    dropDomain();

    return 0;
}
