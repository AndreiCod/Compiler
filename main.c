#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"

int main(int argc, char **argv)
{
    char *inbuf = loadFile("tests/testad.c");
    puts(inbuf);

    // Tokenize the input
    Token *tokens = tokenize(inbuf);
    // showTokens(tokens);

    // Create the global symbol table
    pushDomain();

    // Parse the tokens
    parse(tokens);

    // Show the symbol table
    showDomain(symTable, "global");
    //dropDomain();

    return 0;
}
