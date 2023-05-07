#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin,const char *end){
	char *text=safeAlloc(end-begin+1);
	memcpy(text,begin,end-begin);
	text[end-begin]='\0';
	return text;
}


Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	for(;;){
		switch(*pch){
			case ' ':case '\t':pch++;break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case ',':addTk(COMMA);pch++;break;
			case ';':addTk(SEMICOLON);pch++;break;
			case '(':addTk(LPAR);pch++;break;
			case ')':addTk(RPAR);pch++;break;
			case '[':addTk(LBRACKET);pch++;break;
			case ']':addTk(RBRACKET);pch++;break;
			case '{':addTk(LACC);pch++;break;
			case '}':addTk(RACC);pch++;break;
			case '\0':addTk(END);return tokens;
			case '+':addTk(ADD);pch++;break;
			case '-':addTk(SUB);pch++;break;
			case '*':addTk(MUL);pch++;break;
			case '/':
				if(pch[1]=='/'){
					for(pch+=2;*pch!='\r'&&*pch!='\n'&&*pch!='\0';pch++){}
					}else{
					addTk(DIV);
					pch++;
					}
				break;
			case '.':addTk(DOT);pch++;break;
			case '&':
				if(pch[1]=='&'){
					addTk(AND);
					pch+=2;
					}else{
					err("invalid char: %c (%d)",*pch,*pch);
					}
				break;
			case '|':
				if(pch[1]=='|'){
					addTk(OR);
					pch+=2;
					}else{
					err("invalid char: %c (%d)",*pch,*pch);
					}
				break;
			case '!':
				if(pch[1]=='='){
					addTk(NOTEQ);
					pch+=2;
					}else{
					addTk(NOT);
					pch++;
					}
				break;
			case '=':
				if(pch[1]=='='){
					addTk(EQUAL);
					pch+=2;
					}else{
					addTk(ASSIGN);
					pch++;
					}
				break;
			case '<':
				if(pch[1]=='='){
					addTk(LESSEQ);
					pch+=2;
					}else{
					addTk(LESS);
					pch++;
					}
				break;
			case '>':
				if(pch[1]=='='){
					addTk(GREATEREQ);
					pch+=2;
					}else{
					addTk(GREATER);
					pch++;
					}
				break;
			default:
				if(isalpha(*pch)||*pch=='_'){
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
					char *text=extract(start,pch);
					if(strcmp(text,"char")==0)addTk(TYPE_CHAR);
					else if (strcmp(text, "double")==0)addTk(TYPE_DOUBLE);
					else if (strcmp(text,"else")==0)addTk(ELSE);
					else if (strcmp(text,"if")==0)addTk(IF);
					else if (strcmp(text,"int")==0)addTk(TYPE_INT);
					else if (strcmp(text,"return")==0)addTk(RETURN);
					else if (strcmp(text,"struct")==0)addTk(STRUCT);
					else if (strcmp(text,"void")==0)addTk(VOID);
					else if (strcmp(text,"while")==0)addTk(WHILE);
					else {
						tk=addTk(ID);
						tk->text=text;
					}
				}
				// extract string and char
				else if(*pch=='"'||*pch=='\''){
					char delim=*pch;
					for(start=++pch;*pch!=delim&&*pch!='\0';pch++){
						if(*pch=='\r'||*pch=='\n'){
							err("unterminated string or char");
							}
						}
					if(*pch=='\0'){
						err("unterminated string or char");
						}
					if (delim == '"') {
						tk = addTk(STRING);
						tk->text = extract(start, pch);
						pch++;
					}
					// check if char is valid
					else if (pch - start > 1) {
						err("invalid char");
					}
					else{
						tk = addTk(CHAR);
						tk->c = *start;
						pch++;
					}
					pch++;
				}
				// extract double and int
				// double format: [0-9]+ ( '.' [0-9]+ ( [eE] [+-]? [0-9]+ )? | ( '.' [0-9]+ )? [eE] [+-]? [0-9]+ )
				else if (isdigit(*pch)) {
					for (start = pch++; isdigit(*pch); pch++) {}
					if (*pch == '.') {
						pch++;
						if (isdigit(*pch)) {
							for (pch++; isdigit(*pch); pch++) {}
							if (*pch == 'e' || *pch == 'E') {
								pch++;
								if (*pch == '+' || *pch == '-') {
									pch++;
								}
								if (isdigit(*pch)) {
									for (pch++; isdigit(*pch); pch++) {}
								}
								else {
									err("invalid char: %c (%d)", *pch, *pch);
								}
							}
						}
						else {
							err("invalid char: %c (%d)", *pch, *pch);
						}
						tk = addTk(DOUBLE);
						tk->d = atof(extract(start, pch));
					}
					else if (*pch == 'e' || *pch == 'E') {
						pch++;
						if (*pch == '+' || *pch == '-') {
							pch++;
						}
						if (isdigit(*pch)) {
							for (pch++; isdigit(*pch); pch++) {}
						}
						else {
							err("invalid char: %c (%d)", *pch, *pch);
						}
						tk = addTk(DOUBLE);
						tk->d = atof(extract(start, pch));
					}
					else {
						tk = addTk(INT);
						tk->i = atoi(extract(start, pch));
					}
				}
				else err("invalid char: %c (%d)",*pch,*pch);
			}
		}
	}

const char* tkCodeName(int code) {
	switch (code) {
		case ID: return "ID";
		case TYPE_CHAR: return "TYPE_CHAR";
		case TYPE_DOUBLE: return "TYPE_DOUBLE";
		case ELSE: return "ELSE";
		case IF: return "IF";
		case TYPE_INT: return "TYPE_INT";
		case RETURN: return "RETURN";
		case STRUCT: return "STRUCT";
		case VOID: return "VOID";
		case WHILE: return "WHILE";
		case COMMA: return "COMMA";
		case END: return "END";
		case SEMICOLON: return "SEMICOLON";
		case LPAR: return "LPAR";
		case RPAR: return "RPAR";
		case LBRACKET: return "LBRACKET";
		case RBRACKET: return "RBRACKET";
		case LACC: return "LACC";
		case RACC: return "RACC";
		case ADD: return "ADD";
		case SUB: return "SUB";
		case MUL: return "MUL";
		case DIV: return "DIV";
		case DOT: return "DOT";
		case AND: return "AND";
		case OR: return "OR";
		case NOT: return "NOT";
		case ASSIGN: return "ASSIGN";
		case EQUAL: return "EQUAL";
		case NOTEQ: return "NOTEQ";
		case LESS: return "LESS";
		case LESSEQ: return "LESSEQ";
		case GREATER: return "GREATER";
		case GREATEREQ: return "GREATEREQ";
		case CHAR: return "CHAR";
		case INT: return "INT";
		case DOUBLE: return "DOUBLE";
		case STRING: return "STRING";
	}

	err("invalid token code: %d", code);
}

void showTokens(const Token *tokens){
	for(const Token *tk=tokens;tk;tk=tk->next){
		printf("%d\t%s",tk->line, tkCodeName(tk->code));
		if (tk->code == ID || tk->code == STRING) {
			printf(":%s", tk->text);
		}
		else if (tk->code == CHAR) {
			printf(":%c", tk->c);
		}
		else if (tk->code == INT) {
			printf(":%d", tk->i);
		}
		else if (tk->code == DOUBLE) {
			printf(":%f", tk->d);
		}
		printf("\n");
	}
}
