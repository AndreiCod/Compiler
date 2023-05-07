#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

Token *iTk;				// the iterator in the tokens list
Token *consumedTk;		// the last consumed token
Token *finalConsumedTk; // the last consumed token before the last error

bool unit();
bool structDef();
bool varDef();
bool typeBase();
bool arrayDecl();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound();
bool expr();
bool exprAssign();
bool exprOr();
bool exprOrPrime();
bool exprAnd();
bool exprAndPrime();
bool exprEq();
bool exprEqPrime();
bool exprRel();
bool exprRelPrime();
bool exprAdd();
bool exprAddPrime();
bool exprMul();
bool exprMulPrime();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrime();
bool exprPrimary();

void tkerr(const char *fmt, ...)
{
	fprintf(stderr, "error in line %d: ", iTk->line);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fprintf(stderr, "\n");
	exit(EXIT_FAILURE);
}

bool consume(int code)
{
	// printf("consume(%s)", tkCodeName(code));
	if (iTk->code == code)
	{
		consumedTk = iTk;
		iTk = iTk->next;
		// printf(" => consumed\n");
		return true;
	}
	// printf(" => found %s\n", tkCodeName(iTk->code));
	return false;
}

// unit: ( structDef | fnDef | varDef )* END // with error control
bool unit()
{
	for (;;)
	{
		if (structDef())
		{
			for (; finalConsumedTk != iTk; finalConsumedTk = finalConsumedTk->next)
			{
				printf("consumed: %s\n", tkCodeName(finalConsumedTk->code));
			}

			continue;
		}
		if (fnDef())
		{
			for (; finalConsumedTk != iTk; finalConsumedTk = finalConsumedTk->next)
			{
				printf("consumed: %s\n", tkCodeName(finalConsumedTk->code));
			}

			continue;
		}
		if (varDef())
		{
			for (; finalConsumedTk != iTk; finalConsumedTk = finalConsumedTk->next)
			{
				printf("consumed: %s\n", tkCodeName(finalConsumedTk->code));
			}

			continue;
		}
		break;
	}

	if (consume(END))
	{
		printf("consumed: %s\n", tkCodeName(finalConsumedTk->code));
		return true;
	}
	else
		tkerr("syntax error");
	return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON // with error control
bool structDef()
{
	Token *start = iTk;
	if (consume(STRUCT))
	{
		if (consume(ID))
		{
			if (consume(LACC))
			{
				for (;;)
				{
					if (varDef())
					{
						continue;
					}
					break;
				}
				if (consume(RACC))
				{
					if (consume(SEMICOLON))
					{
						return true;
					}
					else
						tkerr("expected ';'");
				}
			}
		}
	}
	iTk = start;
	return false;
}

// varDef: typeBase ID arrayDecl? SEMICOLON // with error control
bool varDef()
{
	Token *start = iTk;
	if (typeBase())
	{
		if (consume(ID))
		{
			arrayDecl();
			if (consume(SEMICOLON))
			{
				return true;
			}
		}
	}
	iTk = start;
	return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID // with error control
bool typeBase()
{
	Token *start = iTk;
	if (consume(TYPE_INT))
	{
		return true;
	}
	if (consume(TYPE_DOUBLE))
	{
		return true;
	}
	if (consume(TYPE_CHAR))
	{
		return true;
	}
	if (consume(STRUCT))
	{
		if (consume(ID))
		{
			return true;
		}
		else
			tkerr("expected identifier");
	}
	iTk = start;
	return false;
}

// arrayDecl: LBRACKET INT? RBRACKET // with error control
bool arrayDecl()
{
	Token *start = iTk;
	if (consume(LBRACKET))
	{
		consume(INT);
		if (consume(RBRACKET))
		{
			return true;
		}
		else
			tkerr("expected ']'");
	}
	iTk = start;
	return false;
}

// fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound // with error control
bool fnDef()
{
	Token *start = iTk;
	if (typeBase() || consume(VOID))
	{
		if (consume(ID))
		{
			if (consume(LPAR))
			{
				if (fnParam())
				{
					for (;;)
					{
						if (consume(COMMA))
						{
							if (fnParam())
							{
								continue;
							}
						}
						break;
					}
				}
				if (consume(RPAR))
				{
					if (stmCompound())
					{
						return true;
					}
				}
				else
					tkerr("expected ')'");
			}
		}
	}
	iTk = start;
	return false;
}

// fnParam: typeBase ID arrayDecl? // with error control
bool fnParam()
{
	Token *start = iTk;
	if (typeBase())
	{
		if (consume(ID))
		{
			arrayDecl();
			return true;
		}
		else
			tkerr("expected identifier"); // not sure
	}
	iTk = start;
	return false;
}

// stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON // with error control
bool stm()
{
	Token *start = iTk;
	if (stmCompound())
	{
		return true;
	}
	if (consume(IF))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				if (consume(RPAR))
				{
					if (stm())
					{
						if (consume(ELSE))
						{
							if (stm())
							{
								return true;
							}
						}
						else
							return true;
					}
				}
				else
					tkerr("expected ')'");
			}
			else
				tkerr("expected expression");
		}
	}
	if (consume(WHILE))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				if (consume(RPAR))
				{
					if (stm())
					{
						return true;
					}
					else
						tkerr("expected statement");
				}
				else
					tkerr("expected ')'");
			}
			else
				tkerr("expected expression");
		}
	}
	if (consume(RETURN))
	{
		expr();
		if (consume(SEMICOLON))
		{
			return true;
		}
		else
			tkerr("expected ';'");
	}
	expr();
	if (consume(SEMICOLON))
	{
		return true;
	}
	iTk = start;
	return false;
}

// stmCompound: LACC ( varDef | stm )* RACC // with error control
bool stmCompound()
{
	Token *start = iTk;
	if (consume(LACC))
	{
		for (;;)
		{
			if (varDef())
			{
			}
			else if (stm())
			{
			}
			else
				break;
		}
		if (consume(RACC))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

// expr: exprAssign // with error control
bool expr()
{
	Token *start = iTk;
	if (exprAssign())
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr // with error control
bool exprAssign()
{
	Token *start = iTk;
	if (exprUnary())
	{
		if (consume(ASSIGN))
		{
			if (exprAssign())
			{
				return true;
			}
		}
	}
	iTk = start;
	if (exprOr())
	{
		return true;
	}
	return false;
}

// exprOr: exprAnd exprOrPrime; exprOrPrime: OR exprAnd exprOrPrime | epsilon // with error control
bool exprOr()
{
	Token *start = iTk;
	if (exprAnd())
	{
		if (exprOrPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprOrPrime()
{
	if (consume(OR))
	{
		if (exprAnd())
		{
			if (exprOrPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprAnd: exprEq exprAndPrime; exprAndPrime: AND exprEq exprAndPrime | epsilon // with error control
bool exprAnd()
{
	Token *start = iTk;
	if (exprEq())
	{
		if (exprAndPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprAndPrime()
{
	if (consume(AND))
	{
		if (exprEq())
		{
			if (exprAndPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprEq: exprRel exprEqPrime; exprEqPrime: EQUAL exprRel exprEqPrime | NOTEQ exprRel exprEqPrime | epsilon // with error control
bool exprEq()
{
	Token *start = iTk;
	if (exprRel())
	{
		if (exprEqPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprEqPrime()
{
	if (consume(EQUAL))
	{
		if (exprRel())
		{
			if (exprEqPrime())
			{
				return true;
			}
		}
	}
	else if (consume(NOTEQ))
	{
		if (exprRel())
		{
			if (exprEqPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprRel: exprAdd exprRelPrime; exprRelPrime: LESS exprAdd exprRelPrime | LESSEQ exprAdd exprRelPrime | GREATER exprAdd exprRelPrime | GREATEREQ exprAdd exprRelPrime | epsilon // with error control
bool exprRel()
{
	Token *start = iTk;
	if (exprAdd())
	{
		if (exprRelPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprRelPrime()
{
	if (consume(LESS))
	{
		if (exprAdd())
		{
			if (exprRelPrime())
			{
				return true;
			}
		}
	}
	else if (consume(LESSEQ))
	{
		if (exprAdd())
		{
			if (exprRelPrime())
			{
				return true;
			}
		}
	}
	else if (consume(GREATER))
	{
		if (exprAdd())
		{
			if (exprRelPrime())
			{
				return true;
			}
		}
	}
	else if (consume(GREATEREQ))
	{
		if (exprAdd())
		{
			if (exprRelPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprAdd: exprMul exprAddPrime; exprAddPrime: ADD exprMul exprAddPrime | SUB exprMul exprAddPrime | epsilon // with error control
bool exprAdd()
{
	Token *start = iTk;
	if (exprMul())
	{
		if (exprAddPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprAddPrime()
{
	if (consume(ADD))
	{
		if (exprMul())
		{
			if (exprAddPrime())
			{
				return true;
			}
		}
	}
	else if (consume(SUB))
	{
		if (exprMul())
		{
			if (exprAddPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprMul: exprCast exprMulPrime; exprMulPrime: MUL exprCast exprMulPrime | DIV exprCast exprMulPrime | epsilon // with error control
bool exprMul()
{
	Token *start = iTk;
	if (exprCast())
	{
		if (exprMulPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprMulPrime()
{
	if (consume(MUL))
	{
		if (exprCast())
		{
			if (exprMulPrime())
			{
				return true;
			}
		}
	}
	else if (consume(DIV))
	{
		if (exprCast())
		{
			if (exprMulPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary // with error control
bool exprCast()
{
	Token *start = iTk;
	if (consume(LPAR))
	{
		if (typeBase())
		{
			if (arrayDecl())
			{
			}
			if (consume(RPAR))
			{
				if (exprCast())
				{
					return true;
				}
			}
			// else tkerr("expected ')'"); // not sure
		}
	}
	iTk = start;
	if (exprUnary())
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix // with error control
bool exprUnary()
{
	Token *start = iTk;
	if (consume(SUB))
	{
		if (exprUnary())
		{
			return true;
		}
	}
	else if (consume(NOT))
	{
		if (exprUnary())
		{
			return true;
		}
	}
	iTk = start;
	if (exprPostfix())
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprPostfix: exprPrimary exprPostfixPrime; exprPostfixPrime: LBRACKET expr RBRACKET exprPostfixPrime | DOT ID exprPostfixPrime | epsilon // with error control
bool exprPostfix()
{
	Token *start = iTk;
	if (exprPrimary())
	{
		if (exprPostfixPrime())
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprPostfixPrime()
{
	if (consume(LBRACKET))
	{
		if (expr())
		{
			if (consume(RBRACKET))
			{
				if (exprPostfixPrime())
				{
					return true;
				}
			}
		}
	}
	else if (consume(DOT))
	{
		if (consume(ID))
		{
			if (exprPostfixPrime())
			{
				return true;
			}
		}
	}
	return true;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR // with error control
bool exprPrimary()
{
	Token *start = iTk;
	if (consume(ID))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				while (consume(COMMA))
				{
					if (expr())
					{
					}
				}
			}
			if (consume(RPAR))
			{
			}
		}
	}
	else if (consume(INT))
	{
	}
	else if (consume(DOUBLE))
	{
	}
	else if (consume(CHAR))
	{
	}
	else if (consume(STRING))
	{
	}
	else if (consume(LPAR))
	{
		if (expr())
		{
			if (consume(RPAR))
			{
			}
		}
	}
	else
	{
		iTk = start;
		return false;
	}
	return true;
}

// parse function
void parse(Token *tokens)
{
	iTk = tokens;
	finalConsumedTk = tokens;
	if (!unit())
		tkerr("syntax error");
}

// to add: error treatment, parse function