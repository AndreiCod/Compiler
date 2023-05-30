#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "at.h"

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
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrime(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrime(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrime(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrime(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrime(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrime(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrime(Ret *r);
bool exprPrimary(Ret *r);

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
// stm: { Ret rCond, rExpr; }
// stmCompound[true] | IF LPAR expr[&rCond]
// {
// 	if (!canBeScalar(&rCond))
// 		tkerr("the if condition must be a scalar value");
// }
// RPAR stm(ELSE stm) ? | WHILE LPAR expr[&rCond]
// {
// 	if (!canBeScalar(&rCond))
// 		tkerr("the while condition must be a scalar value");
// }
// RPAR stm | RETURN(
// 			   expr[&rExpr] {
// 				   if (owner->type.tb == TB_VOID)
// 					   tkerr("a void function cannot return a value");
// 				   if (!canBeScalar(&rExpr))
// 					   tkerr("the return value must be a scalar value");
// 				   if (!convTo(&rExpr.type, &owner->type))tkerr("cannot convert the return expression
// type to the function return type");
// 			   } |
// 			   {if(owner->type.tb!=TB_VOID)tkerr("a non-void function must return a value"); }) SEMICOLON
// 	| expr[&rExpr]
// 	? SEMICOLON
bool stm()
{
	Ret rCond, rExpr;
	Token *start = iTk;
	if (stmCompound(true))
	{
		return true;
	}
	if (consume(IF))
	{
		if (consume(LPAR))
		{
			if (expr(&rCond))
			{
				if (!canBeScalar(&rCond))
					tkerr("the if condition must be a scalar value");

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
						{
							return true;
						}
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
			if (expr(&rCond))
			{
				if (!canBeScalar(&rCond))
					tkerr("the while condition must be a scalar value");

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
		if (expr(&rExpr))
		{
			if (owner->type.tb == TB_VOID)
				tkerr("a void function cannot return a value");
			if (!canBeScalar(&rExpr))
				tkerr("the return value must be a scalar value");
			if (!convTo(&rExpr.type, &owner->type))
				tkerr("cannot convert the return expression type to the function return type");
		}
		else
		{
			if (owner->type.tb != TB_VOID)
				tkerr("a non-void function must return a value");
		}
		if (consume(SEMICOLON))
		{
			return true;
		}
		else
			tkerr("expected semicolon");
	}
	expr(&rExpr);
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
		while (stm() || varDef())
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

// expr[out Ret *r]: exprAssign[r]
bool expr(Ret *r)
{
	Token *start = iTk;
	if (exprAssign(r))
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr // with error control
// exprAssign[out Ret * r]: { Ret rDst; }
// exprUnary[&rDst] ASSIGN exprAssign[r]
// {
// 	if (!rDst.lval)
// 		tkerr("the assign destination must be a left-value");
// 	if (rDst.ct)
// 		tkerr("the assign destination cannot be constant");
// 	if (!canBeScalar(&rDst))
// 		tkerr("the assign destination must be scalar");
// 	if (!canBeScalar(r))
// 		tkerr("the assign source must be scalar");
// 	if (!convTo(&r->type, &rDst.type))
// 		tkerr("the assign source cannot be converted to destination");
// 	r->lval = false;
// 	r->ct = true;
// }
// | exprOr[r]
bool exprAssign(Ret *r)
{
	Token *start = iTk;
	Ret rDst;
	if (exprUnary(&rDst))
	{
		if (consume(ASSIGN))
		{
			if (exprAssign(r))
			{
				if (!rDst.lval)
					tkerr("the assign destination must be a left-value");
				if (rDst.ct)
					tkerr("the assign destination cannot be constant");
				if (!canBeScalar(&rDst))
					tkerr("the assign destination must be scalar");
				if (!canBeScalar(r))
					tkerr("the assign source must be scalar");
				if (!convTo(&r->type, &rDst.type))
					tkerr("the assign source cannot be converted to destination");
				r->lval = false;
				r->ct = true;
				return true;
			}
		}
	}
	iTk = start;
	if (exprOr(r))
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprOr: exprAnd exprOrPrime; exprOrPrime: OR exprAnd exprOrPrime | epsilon // with error control
// exprOr[out Ret *r]: exprOr[r] OR {Ret right;} exprAnd[&right]
// {
// 	Type tDst;
// 	if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for ||");
//		*r=(Ret){{TB_INT,NULL,-1},false,true};
// }
// | exprAnd[r]
bool exprOr(Ret *r)
{
	Token *start = iTk;
	if (exprAnd(r))
	{
		if (exprOrPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprOrPrime(Ret *r)
{
	if (consume(OR))
	{
		Ret right;
		if (exprAnd(&right))
		{
			if (exprOrPrime(r))
			{
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for ||");
				*r = (Ret){{TB_INT, NULL, -1}, false, true};
				return true;
			}
		}
	}
	return true;
}

// exprAnd: exprEq exprAndPrime; exprAndPrime: AND exprEq exprAndPrime | epsilon // with error control
// exprAnd[out Ret *r]: exprAnd[r] AND {Ret right;} exprEq[&right]
// {
//	Type tDst;
//	if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for &&");
//	*r=(Ret){{TB_INT,NULL,-1},false,true};
// }
// | exprEq[r]
bool exprAnd(Ret *r)
{
	Token *start = iTk;
	if (exprEq(r))
	{
		if (exprAndPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprAndPrime(Ret *r)
{
	if (consume(AND))
	{
		Ret right;
		if (exprEq(&right))
		{
			if (exprAndPrime(r))
			{
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for &&");
				*r = (Ret){{TB_INT, NULL, -1}, false, true};
				return true;
			}
		}
	}
	return true;
}

// exprEq: exprRel exprEqPrime; exprEqPrime: EQUAL exprRel exprEqPrime | NOTEQ exprRel exprEqPrime | epsilon // with error control
// exprEq[out Ret * r] : exprEq[r](EQUAL | NOTEQ) { Ret right; }
// exprRel[&right]
// {
// 	Type tDst;
// 	if (!arithTypeTo(&r->type, &right.type, &tDst))
// 		tkerr("invalid operand type for == or !=");
// 	*r = (Ret){{TB_INT, NULL, -1}, false, true};
// }
// | exprRel[r]
bool exprEq(Ret *r)
{
	Token *start = iTk;
	if (exprRel(r))
	{
		if (exprEqPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprEqPrime(Ret *r)
{
	if (consume(EQUAL) || consume(NOTEQ))
	{
		Ret right;
		if (exprRel(&right))
		{
			if (exprEqPrime(r))
			{
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for == or !=");
				*r = (Ret){{TB_INT, NULL, -1}, false, true};
				return true;
			}
		}
	}
	return true;
}

// exprRel: exprAdd exprRelPrime; exprRelPrime: LESS exprAdd exprRelPrime | LESSEQ exprAdd exprRelPrime | GREATER exprAdd exprRelPrime | GREATEREQ exprAdd exprRelPrime | epsilon // with error control
// exprRel[out Ret * r] : exprRel[r](LESS | LESSEQ | GREATER | GREATEREQ) { Ret right; }
// exprAdd[&right]
// {
// 	Type tDst;
// 	if (!arithTypeTo(&r->type, &right.type, &tDst))
// 		tkerr("invalid operand type for <, <=, >, >=");
// 	*r = (Ret){{TB_INT, NULL, -1}, false, true};
// }
// | exprAdd[r]
bool exprRel(Ret *r)
{
	Token *start = iTk;
	if (exprAdd(r))
	{
		if (exprRelPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprRelPrime(Ret *r)
{
	if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ))
	{
		Ret right;
		if (exprAdd(&right))
		{
			if (exprRelPrime(r))
			{
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for <, <=, >, >=");
				*r = (Ret){{TB_INT, NULL, -1}, false, true};
				return true;
			}
		}
	}
	return true;
}

// exprAdd: exprMul exprAddPrime; exprAddPrime: ADD exprMul exprAddPrime | SUB exprMul exprAddPrime | epsilon // with error control
// exprAdd[out Ret * r] : exprAdd[r](ADD | SUB) { Ret right; }
// exprMul[&right]
// {
// 	Type tDst;
// 	if (!arithTypeTo(&r->type, &right.type, &tDst))
// 		tkerr("invalid operand type for + or -");
// 	*r = (Ret){tDst, false, true};
// }
// | exprMul[r]
bool exprAdd(Ret *r)
{
	Token *start = iTk;
	if (exprMul(r))
	{
		if (exprAddPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprAddPrime(Ret *r)
{
	if (consume(ADD) || consume(SUB))
	{
		Ret right;
		if (exprMul(&right))
		{
			if (exprAddPrime(r))
			{
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for + or -");
				*r = (Ret){tDst, false, true};
				return true;
			}
		}
	}
	return true;
}

// exprMul: exprCast exprMulPrime; exprMulPrime: MUL exprCast exprMulPrime | DIV exprCast exprMulPrime | epsilon // with error control
// exprMul[out Ret * r] : exprMul[r](MUL | DIV) { Ret right; }
// exprCast[&right]
// {
// 	Type tDst;
// 	if (!arithTypeTo(&r->type, &right.type, &tDst))
// 		tkerr("invalid operand type for * or /");
// 	*r = (Ret){tDst, false, true};
// }
// | exprCast[r]
bool exprMul(Ret *r)
{
	Token *start = iTk;
	if (exprCast(r))
	{
		if (exprMulPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprMulPrime(Ret *r)
{
	if (consume(MUL) || consume(DIV))
	{
		Ret right;
		if (exprCast(&right))
		{
			if (exprMulPrime(r))
			{
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for * or /");
				*r = (Ret){tDst, false, true};
				return true;
			}
		}
	}
	return true;
}

// exprCast: LPAR {Type t;} typeBase[&t] arrayDecl[&t]? RPAR exprCast | exprUnary
// exprCast[out Ret * r] : LPAR
// {
// 	Type t;
// 	Ret op;
// }
// typeBase[&t] arrayDecl[&t] ? RPAR
// exprCast[&op]
// {
// 	if (t.tb == TB_STRUCT)
// 		tkerr("cannot convert to a struct type");
// 	if (op.type.tb == TB_STRUCT)
// 		tkerr("cannot convert a struct");
// 	if (op.type.n >= 0 && t.n < 0)
// 		tkerr("an array can be converted only to another array");
// 	if (op.type.n < 0 && t.n >= 0)
// 		tkerr("a scalar can be converted only to another scalar");
// 	*r = (Ret){t, false, true};
// }
// | exprUnary[r]
bool exprCast(Ret *r)
{
	Token *start = iTk;
	if (consume(LPAR))
	{
		Type t;
		Ret op;
		if (typeBase(&t))
		{
			arrayDecl(&t);
			if (consume(RPAR))
			{
				if (exprCast(&op))
				{
					if (t.tb == TB_STRUCT)
						tkerr("cannot convert to a struct type");
					if (op.type.tb == TB_STRUCT)
						tkerr("cannot convert a struct");
					if (op.type.n >= 0 && t.n < 0)
						tkerr("an array can be converted only to another array");
					if (op.type.n < 0 && t.n >= 0)
						tkerr("a scalar can be converted only to another scalar");
					*r = (Ret){t, false, true};
					return true;
				}
			}
		}
	}
	iTk = start;
	if (exprUnary(r))
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix // with error control
// exprUnary[out Ret * r] : (SUB | NOT) exprUnary[r]
// {
// 	if (!canBeScalar(r))
// 		tkerr("unary - or ! must have a scalar operand");
// 	r->lval = false;
// 	r->ct = true;
// }
// | exprPostfix[r]
bool exprUnary(Ret *r)
{
	Token *start = iTk;
	if (consume(SUB) || consume(NOT))
	{
		if (exprUnary(r))
		{
			if (!canBeScalar(r))
				tkerr("unary - or ! must have a scalar operand");
			r->lval = false;
			r->ct = true;
			return true;
		}
	}
	iTk = start;
	if (exprPostfix(r))
	{
		return true;
	}
	iTk = start;
	return false;
}

// exprPostfix: exprPrimary exprPostfixPrime; exprPostfixPrime: LBRACKET expr RBRACKET exprPostfixPrime | DOT ID exprPostfixPrime | epsilon // with error control
// exprPostfix[out Ret * r] : exprPostfix[r] LBRACKET { Ret idx; }
// expr[&idx] RBRACKET
// {
// 	if (r->type.n < 0)
// 		tkerr("only an array can be indexed");
// 	Type tInt = {TB_INT, NULL, -1};
// 	if (!convTo(&idx.type, &tInt))
// 		tkerr("the index is not convertible to int");
// 	r->type.n = -1;
// 	r->lval = true;
// 	r->ct = false;
// }
// | exprPostfix[r] DOT ID[tkName]
// {
// 	if (r->type.tb != TB_STRUCT)
// 		tkerr("a field can only be selected from a struct");
// 	Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
// 	if (!s)
// 		tkerr("the structure %s does not have a field %s", r->type.s->name, tkName->text);
// 	*r = (Ret){s->type, true, s->type.n >= 0};
// }
// | exprPrimary[r]
bool exprPostfix(Ret *r)
{
	Token *start = iTk;
	if (exprPrimary(r))
	{
		if (exprPostfixPrime(r))
		{
			return true;
		}
	}
	iTk = start;
	return false;
}

bool exprPostfixPrime(Ret *r)
{
	if (consume(LBRACKET))
	{
		Ret idx;
		if (expr(&idx))
		{
			if (consume(RBRACKET))
			{
				if (r->type.n < 0)
						tkerr("only an array can be indexed");
					Type tInt = {TB_INT, NULL, -1};
					if (!convTo(&idx.type, &tInt))
						tkerr("the index is not convertible to int");
					r->type.n = -1;
					r->lval = true;
					r->ct = false;
				if (exprPostfixPrime(r))
				{
					if (r->type.n < 0)
						tkerr(iTk, "only an array can be indexed");
					Type tInt = {TB_INT, NULL, -1};
					if (!convTo(&idx.type, &tInt))
						tkerr(iTk, "the index is not convertible to int");
					r->type.n = -1;
					r->lval = true;
					r->ct = false;
					return true;
				}
			}
		}
	}
	else if (consume(DOT))
	{
		Token *tkName = iTk;
		if (consume(ID))
		{
			if (r->type.tb != TB_STRUCT)
					tkerr("a field can only be selected from a struct");
				Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
				if (!s)
					tkerr("the structure %s does not have a field %s", r->type.s->name, tkName->text);
				*r = (Ret){s->type, true, s->type.n >= 0};
			if (exprPostfixPrime(r))
			{

				return true;
			}
		}
	}
	return true;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR // with error control
// exprPrimary[out Ret * r] : ID[tkName]
// {
// 	Symbol *s = findSymbol(tkName->text);
// 	if (!s)
// 		tkerr("undefined id: %s", tkName->text);
// }
// ( LPAR
// {
// 	if (s->kind != SK_FN)
// 		tkerr("only a function can be called");
// 	Ret rArg;
// 	Symbol *param = s->fn.params;
// }
// ( expr[&rArg]
// {
// 	if (!param)
// 		tkerr("too many arguments in function call");
// 	if (!convTo(&rArg.type, &param->type))
// 		tkerr("in call, cannot convert the argument type to the parameter type");
// 	param = param->next;
// }
// ( COMMA expr[&rArg]
// {
// 	if (!param)
// 		tkerr("too many arguments in function call");
// 	if (!convTo(&rArg.type, &param->type))
// 		tkerr("in call, cannot convert the argument type to the parameter type");
// 	param = param->next;
// }
// )* )? RPAR
// {
// 	if (param)
// 		tkerr("too few arguments in function call");
// 	*r = (Ret){s->type, false, true};
// }
// |
// {
// 	if (s->kind == SK_FN)
// 		tkerr("a function can only be called");
// 	*r = (Ret){s->type, true, s->type.n >= 0};
// }
// )
// | INT {*r=(Ret){{TB_INT,NULL,-1},false,true};}
// | DOUBLE { *r = (Ret){{TB_DOUBLE, NULL, -1}, false, true}; }
// | CHAR { *r = (Ret){{TB_CHAR, NULL, -1}, false, true}; }
// | STRING { *r = (Ret){{TB_CHAR, NULL, 0}, false, true}; }
// | LPAR expr[r] RPAR
bool exprPrimary(Ret *r)
{
	Token *start = iTk;
	Token *tkName = iTk;
	if (consume(ID))
	{
		Symbol *s = findSymbol(tkName->text);
		if (!s)
			tkerr("undefined id: %s", tkName->text);
		if (consume(LPAR))
		{
			if (s->kind != SK_FN)
				tkerr("only a function can be called");
			Ret rArg;
			Symbol *param = s->fn.params;
			if (expr(&rArg))
			{
				if (!param)
					tkerr("too many arguments in function call");
				if (!convTo(&rArg.type, &param->type)) // aici moare
					tkerr("in call, cannot convert the argument type to the parameter type");
				param = param->next;
				while (consume(COMMA))
				{
					if (expr(&rArg))
					{
						if (!param)
							tkerr("too many arguments in function call");
						if (!convTo(&rArg.type, &param->type))
							tkerr("in call, cannot convert the argument type to the parameter type");
						param = param->next;
					}
					else
						tkerr("invalid expression after ,");
				}
			}
			if (consume(RPAR))
			{
				if (param)
					tkerr("too few arguments in function call");
				*r = (Ret){s->type, false, true};
				return true;
			}
			else
				tkerr("invalid expression after (");
		}
		else
		{
			if (s->kind == SK_FN)
				tkerr("a function can only be called");
			*r = (Ret){s->type, true, s->type.n >= 0};
		}
	}
	else if (consume(INT))
	{
		*r = (Ret){{TB_INT, NULL, -1}, false, true};
	}
	else if (consume(DOUBLE))
	{
		*r = (Ret){{TB_DOUBLE, NULL, -1}, false, true};
	}
	else if (consume(CHAR))
	{
		*r = (Ret){{TB_CHAR, NULL, -1}, false, true};
	}
	else if (consume(STRING))
	{
		*r = (Ret){{TB_CHAR, NULL, 0}, false, true};
	}
	else if (consume(LPAR))
	{
		if (expr(r))
		{
			if (consume(RPAR))
				return true;
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

