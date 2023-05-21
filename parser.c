#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"

Token *iTk;				// the iterator in the tokens list
Token *consumedTk;		// the last consumed token
Token *finalConsumedTk; // the last consumed token before the last error

Symbol *owner; // the owner of the current function/struct

bool unit();
bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
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

// structDef: STRUCT ID[tkName] LACC { Symbol *s=findSymbolInDomain(symTable,tkName->text); if(s)tkerr("symbol redefinition: %s",tkName->text); s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT)); s->type.tb=TB_STRUCT; s->type.s=s; s->type.n=-1; pushDomain(); owner=s; } varDef* RACC SEMICOLON { owner=NULL; dropDomain(); } // with error control
bool structDef()
{
	Token *start = iTk;
	if (consume(STRUCT))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (consume(LACC))
			{
				Symbol *s = findSymbolInDomain(symTable, tkName->text);
				if (s)
					tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;
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
						owner = NULL;
						dropDomain();
						return true;
					}
					else
					{
						tkerr("expected ';'");
					}
				}
			}
		}
	}
	iTk = start;
	return false;
}

// varDef: {Type t;} typeBase[&t] ID[tkName] ( arrayDecl[&t] {if(t.n==0)tkerr("a vector variable must have a specified dimension");} )? SEMICOLON { Symbol *var=findSymbolInDomain(symTable,tkName->text); if(var)tkerr("symbol redefinition: %s",tkName->text); var=newSymbol(tkName->text,SK_VAR); var->type=t; var->owner=owner; addSymbolToDomain(symTable,var); if(owner){ switch(owner->kind){ case SK_FN: var->varIdx=symbolsLen(owner->fn.locals); addSymbolToList(&owner->fn.locals,dupSymbol(var)); break; case SK_STRUCT: var->varIdx=typeSize(&owner->type); addSymbolToList(&owner->structMembers,dupSymbol(var)); break; } }else{ var->varMem=safeAlloc(typeSize(&t)); } }
bool varDef()
{
	Token *start = iTk;
	Type t;
	if (typeBase(&t))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (arrayDecl(&t))
			{
				if (t.n == 0)
					tkerr("a vector variable must have a specified dimension");
			}
			if (consume(SEMICOLON))
			{
				Symbol *var = findSymbolInDomain(symTable, tkName->text);
				if (var)
					tkerr("symbol redefinition: %s", tkName->text);
				var = newSymbol(tkName->text, SK_VAR);
				var->type = t;
				var->owner = owner;
				addSymbolToDomain(symTable, var);
				if (owner)
				{
					switch (owner->kind)
					{
					case SK_FN:
						var->varIdx = symbolsLen(owner->fn.locals);
						addSymbolToList(&owner->fn.locals, dupSymbol(var));
						break;
					case SK_STRUCT:
						var->varIdx = typeSize(&owner->type);
						addSymbolToList(&owner->structMembers, dupSymbol(var));
						break;
					}
				}
				else
				{
					var->varMem = safeAlloc(typeSize(&t));
				}
				return true;
			}
		}
	}
	iTk = start;
	return false;
}

// typeBase[out Type *t]: {t->n=-1;} ( TYPE_INT {t->tb=TB_INT;} | TYPE_DOUBLE {t->tb=TB_DOUBLE;} | TYPE_CHAR {t->tb=TB_CHAR;} | STRUCT ID[tkName] { t->tb=TB_STRUCT; t->s=findSymbol(tkName->text); if(!t->s)tkerr("structura nedefinita: %s",tkName->text); } )
bool typeBase(Type *t)
{
	Token *start = iTk;
	t->n = -1;

	if (consume(TYPE_INT))
	{
		t->tb = TB_INT;
		return true;
	}
	else if (consume(TYPE_DOUBLE))
	{
		t->tb = TB_DOUBLE;
		return true;
	}
	else if (consume(TYPE_CHAR))
	{
		t->tb = TB_CHAR;
		return true;
	}
	else if (consume(STRUCT))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if (!t->s)
				tkerr("structura nedefinita: %s", tkName->text);
			return true;
		}
	}
	iTk = start;
	return false;
}

// arrayDecl[inout Type *t]: LBRACKET ( INT[tkSize] {t->n=tkSize->i;} | {t->n=0;} ) RBRACKET
bool arrayDecl(Type *t)
{
	Token *start = iTk;

	if (consume(LBRACKET))
	{
		if (consume(INT))
		{
			Token *tkSize = consumedTk;
			t->n = tkSize->i;
		}
		else
		{
			t->n = 0;
		}
		if (consume(RBRACKET))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

// fnDef: {Type t;} ( typeBase[&t] | VOID {t.tb=TB_VOID;} ) ID[tkName] LPAR {...} ( fnParam ( COMMA fnParam )* )? RPAR stmCompound[false] {...}
bool fnDef()
{
	Token *start = iTk;
	Type t;

	if (typeBase(&t) || consume(VOID))
	{
		if (consumedTk->code == VOID)
			t.tb = TB_VOID;
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (consume(LPAR))
			{
				Symbol *fn = findSymbolInDomain(symTable, tkName->text);
				if (fn)
					tkerr("symbol redefinition: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);
				fn->type = t;
				addSymbolToDomain(symTable, fn);
				owner = fn;
				pushDomain();
				if (fnParam())
				{
					while (consume(COMMA))
					{
						if (!fnParam())
						{
							tkerr("expected function parameter");
						}
					}
				}
				if (consume(RPAR))
				{
					if (stmCompound(false))
					{
						owner = NULL;
						dropDomain();
						return true;
					}
				}
			}
		}
	}
	iTk = start;
	return false;
}

// fnParam: {Type t;} typeBase[&t] ID[tkName] (arrayDecl[&t] {t.n=0;} )? {...}
bool fnParam()
{
	Token *start = iTk;
	Type t;
	if (typeBase(&t))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (arrayDecl(&t))
			{
				t.n = 0;
			}
			Symbol *param = findSymbolInDomain(symTable, tkName->text);
			if (param)
				tkerr("symbol redefinition: %s", tkName->text);
			param = newSymbol(tkName->text, SK_PARAM);
			param->type = t;
			param->owner = owner;
			param->paramIdx = symbolsLen(owner->fn.params);
			addSymbolToDomain(symTable, param);
			addSymbolToList(&owner->fn.params, dupSymbol(param));
			return true;
		}
	}
	iTk = start;
	return false;
}

// stm: stmCompound[true] | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON // with error control
bool stm()
{
	Token *start = iTk;
	if (stmCompound(true))
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

// stmCompound[in bool newDomain]: LACC {if(newDomain)pushDomain();} ( varDef | stm )* RACC {if(newDomain)dropDomain();}
bool stmCompound(bool newDomain)
{
	Token *start = iTk;
	if (consume(LACC))
	{
		if (newDomain)
			pushDomain();
		while (varDef() || stm())
		{
		}
		if (consume(RACC))
		{
			if (newDomain)
				dropDomain();
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

// exprCast: LPAR {Type t;} typeBase[&t] arrayDecl[&t]? RPAR exprCast | exprUnary
bool exprCast()
{
	Token *start = iTk;
	if (consume(LPAR))
	{
		Type t;
		if (typeBase(&t))
		{
			if (arrayDecl(&t))
			{
				if (consume(RPAR))
				{
					if (exprCast())
					{
						return true;
					}
				}
			}
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