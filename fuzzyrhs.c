   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*            FUZZY RHS PARSING MODULE                 */
   /*******************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzyrhs.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzypsr.h"
#include "fuzzyutl.h"
#include "fuzzymod.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"
#include "pprint.h"
#include "expressn.h"
#include "exprnops.h"
#include "exprnpsr.h"
#include "evaluatn.h"
#include "extnfunc.h"

/******************************************************************
    FgetArray / FrtnArray - allocate/free double arrays
 ******************************************************************/

double *FgetArray(
  Environment *theEnv,
  int size)
{
   if (size <= 0) return NULL;
   return (double *) genalloc(theEnv, sizeof(double) * size);
}

void FrtnArray(
  Environment *theEnv,
  double *arr,
  int size)
{
   if (arr != NULL && size > 0)
     { genfree(theEnv, arr, sizeof(double) * size); }
}

/******************************************************************
    IgetArray / IrtnArray - allocate/free int arrays
 ******************************************************************/

int *IgetArray(
  Environment *theEnv,
  int size)
{
   if (size <= 0) return NULL;
   return (int *) genalloc(theEnv, sizeof(int) * size);
}

void IrtnArray(
  Environment *theEnv,
  int *arr,
  int size)
{
   if (arr != NULL && size > 0)
     { genfree(theEnv, arr, sizeof(int) * size); }
}

/******************************************************************
    CopyFuzzyValue - deep copy a fuzzy_value
 ******************************************************************/

struct fuzzy_value *CopyFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   struct fuzzy_value *newFv;
   int i;

   if (fv == NULL) return NULL;

   newFv = get_struct(theEnv, fuzzy_value);
   newFv->whichDeftemplate = fv->whichDeftemplate;

   if (fv->name != NULL)
     {
      newFv->name = (char *) genalloc(theEnv, strlen(fv->name) + 1);
      strcpy(newFv->name, fv->name);
     }
   else
     { newFv->name = NULL; }

   newFv->n = fv->n;
   newFv->maxn = fv->n;
   newFv->x = FgetArray(theEnv, fv->n);
   newFv->y = FgetArray(theEnv, fv->n);

   for (i = 0; i < fv->n; i++)
     {
      newFv->x[i] = fv->x[i];
      newFv->y[i] = fv->y[i];
     }

   return newFv;
}

/******************************************************************
    CompactFuzzyValue - reduce memory used by fuzzy_value arrays
 ******************************************************************/

void CompactFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   double *newx, *newy;
   int i;

   if (fv == NULL) return;

   if (fv->n < fv->maxn)
     {
      newx = FgetArray(theEnv, fv->n);
      newy = FgetArray(theEnv, fv->n);

      for (i = 0; i < fv->n; i++)
        {
         newx[i] = fv->x[i];
         newy[i] = fv->y[i];
        }

      FrtnArray(theEnv, fv->x, fv->maxn);
      FrtnArray(theEnv, fv->y, fv->maxn);

      fv->x = newx;
      fv->y = newy;
      fv->maxn = fv->n;
     }
}

/******************************************************************
    Local Internal Function Declarations
 ******************************************************************/

static struct fuzzy_value   *ParseLExpr(Environment *,const char *,
                                        struct token *,struct fuzzyLv *,int *);
static struct fuzzy_value   *ParseLTerm(Environment *,const char *,
                                        struct token *,struct fuzzyLv *,int *);
static struct fuzzy_value   *ParseModExpr(Environment *,const char *,
                                          struct token *,struct fuzzyLv *,int *);
static struct fuzzy_value   *PrimaryTerm(Environment *,const char *,
                                         struct token *,struct fuzzyLv *,int *);
static char                 *modifyName(Environment *,const char *,const char *);
static struct primary_term  *FindPrimaryTerm(struct fuzzyLv *,void *);
static Expression           *assertParseFuzzySet(Environment *,const char *,
                                                  struct token *,int *,
                                                  Deftemplate *,int,int *);
static Expression           *assertParseStandardSet(Environment *,const char *,
                                                     struct token *,int *,
                                                     Deftemplate *,int,int *,int);
static Expression           *assertParseSingletonSet(Environment *,const char *,
                                                      struct token *,int *,
                                                      Deftemplate *,int,int *);
static struct fuzzy_value   *convertStandardSet(Environment *,Expression *,int *);
static struct fuzzy_value   *convertSingletonSet(Environment *,Expression *,int *);
static void                  expressionToFloat(Environment *,Expression *,double *,int *);
static void                  expressionToInteger(Environment *,Expression *,int *,int *);

/******************************************************************
    modifyName - concatenate two strings with space separator
 ******************************************************************/

static char *modifyName(
  Environment *theEnv,
  const char *str1,
  const char *str2)
{
   int str1len = strlen(str1);
   int len = str1len + strlen(str2) + 2;
   char *temp = (char *) genalloc(theEnv, len);

   strcpy(temp, str1);
   temp[str1len] = ' ';
   temp[str1len + 1] = '\0';
   strcat(temp, str2);
   return temp;
}

/******************************************************************
    FindPrimaryTerm - search for a primary term by name
 ******************************************************************/

static struct primary_term *FindPrimaryTerm(
  struct fuzzyLv *lvp,
  void *pt_name)
{
   struct primary_term *ptptr;

   ptptr = lvp->primary_term_list;
   while (ptptr != NULL)
     {
      struct fuzzy_value *fvptr = ptptr->fuzzy_value_description->contents;
      if (fvptr != NULL && fvptr->name != NULL &&
          strcmp(fvptr->name, ((CLIPSLexeme *)pt_name)->contents) == 0)
        { return ptptr; }

      ptptr = ptptr->next;
     }
   return NULL;
}

/******************************************************************
    expressionToFloat - evaluate an expression to get a double
 ******************************************************************/

static void expressionToFloat(
  Environment *theEnv,
  Expression *exprPtr,
  double *result,
  int *error)
{
   UDFValue exprValue;

   EvaluateExpression(theEnv, exprPtr, &exprValue);

   if (exprPtr->type == FLOAT_TYPE)
     { *result = exprPtr->floatValue->contents; }
   else if (exprPtr->type == INTEGER_TYPE)
     { *result = (double) exprPtr->integerValue->contents; }
   else
     {
      EvaluateExpression(theEnv, exprPtr, &exprValue);
      if (exprValue.header->type == FLOAT_TYPE)
        { *result = exprValue.floatValue->contents; }
      else if (exprValue.header->type == INTEGER_TYPE)
        { *result = (double) exprValue.integerValue->contents; }
      else
        {
         *error = 1;
         WriteString(theEnv, STDERR, "Fuzzy set value (expecting FLOAT value)\n");
        }
     }
}

/******************************************************************
    expressionToInteger - evaluate an expression to get an int
 ******************************************************************/

static void expressionToInteger(
  Environment *theEnv,
  Expression *exprPtr,
  int *result,
  int *error)
{
   UDFValue exprValue;

   if (exprPtr->type == INTEGER_TYPE)
     { *result = (int) exprPtr->integerValue->contents; }
   else
     {
      EvaluateExpression(theEnv, exprPtr, &exprValue);
      if (exprValue.header->type == INTEGER_TYPE)
        { *result = (int) exprValue.integerValue->contents; }
      else
        {
         *error = 1;
         WriteString(theEnv, STDERR, "Fuzzy set internal evaluation (expecting int value)\n");
        }
     }
}

/******************************************************************
    PrimaryTerm - look up a primary term and return a copy
 ******************************************************************/

static struct fuzzy_value *PrimaryTerm(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct fuzzyLv *lvp,
  int *error)
{
   struct primary_term *pt;
   struct fuzzy_value *fv;

   if ((pt = FindPrimaryTerm(lvp, tempToken->value)) == NULL)
     {
      *error = 1;
      SyntaxErrorMessage(theEnv, "Fuzzy Expression (expecting a Primary Term or Modifier)");
      return NULL;
     }
   else
     {
      fv = CopyFuzzyValue(theEnv, pt->fuzzy_value_description->contents);
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);
      return fv;
     }
}

/******************************************************************
    ParseModExpr - parse modifier expressions
 ******************************************************************/

static struct fuzzy_value *ParseModExpr(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct fuzzyLv *lvp,
  int *error)
{
   struct modifierListItem *mptr;
   struct fuzzy_value *fvptr;
   char *tmpstr, *tmpstr2;

   /* next token must be a symbol -- modifier or primary term or [ */
   if (tempToken->tknType != SYMBOL_TOKEN)
     {
      *error = 1;
      SyntaxErrorMessage(theEnv, "Fuzzy Expression (expecting modifier, primary term or '[' )");
      return NULL;
     }

   /* check for [ bracket */
   if (strcmp(tempToken->lexemeValue->contents, "[") == 0)
     {
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);

      fvptr = ParseLExpr(theEnv, readSource, tempToken, lvp, error);

      if (*error)
        { return NULL; }

      /* expect closing ] */
      if (tempToken->tknType == SYMBOL_TOKEN &&
          strcmp(tempToken->lexemeValue->contents, "]") == 0)
        {
         SavePPBuffer(theEnv, " ");
         GetToken(theEnv, readSource, tempToken);

         /* save [ and ] in the fv name */
         tmpstr = modifyName(theEnv, "[", fvptr->name);
         tmpstr2 = modifyName(theEnv, tmpstr, "]");
         if (fvptr->name != NULL)
           { genfree(theEnv, fvptr->name, strlen(fvptr->name) + 1); }
         fvptr->name = tmpstr2;
         genfree(theEnv, tmpstr, strlen(tmpstr) + 1);

         return fvptr;
        }

      *error = 1;
      SyntaxErrorMessage(theEnv, "Fuzzy Expression (expecting ']' )");
      rtnFuzzyValue(theEnv, fvptr);
      return NULL;
     }

   /* check for a modifier */
   mptr = FindModifier(theEnv, tempToken->lexemeValue->contents);
   if (mptr != NULL)
     {
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);

      fvptr = ParseModExpr(theEnv, readSource, tempToken, lvp, error);
      if (*error)
        { return NULL; }

      /* apply the modifier function to the fuzzy value */
      ModifyFuzzyValue(theEnv, mptr, fvptr);
      return fvptr;
     }

   /* if not [ or modifier, it must be a primary term */
   return PrimaryTerm(theEnv, readSource, tempToken, lvp, error);
}

/******************************************************************
    ParseLTerm - parse AND terms
 ******************************************************************/

static struct fuzzy_value *ParseLTerm(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct fuzzyLv *lvp,
  int *error)
{
   struct fuzzy_value *fvLeft, *fvRight, *fvTemp;
   char *tmpstr, *tmpstr2;

   fvLeft = ParseModExpr(theEnv, readSource, tempToken, lvp, error);

   if (*error)
     { return NULL; }

   if ((tempToken->tknType == SYMBOL_TOKEN) &&
       ((strcmp(tempToken->lexemeValue->contents, "AND") == 0) ||
        (strcmp(tempToken->lexemeValue->contents, "and") == 0)))
     {
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);
      fvRight = ParseModExpr(theEnv, readSource, tempToken, lvp, error);

      if (*error)
        {
         rtnFuzzyValue(theEnv, fvLeft);
         return NULL;
        }

      /* perform AND (intersection) */
      fvTemp = fintersect(theEnv, fvLeft, fvRight);

      tmpstr = modifyName(theEnv, "AND", fvRight->name);
      tmpstr2 = modifyName(theEnv, fvLeft->name, tmpstr);
      if (fvTemp->name != NULL)
        { genfree(theEnv, fvTemp->name, strlen(fvTemp->name) + 1); }
      fvTemp->name = tmpstr2;
      genfree(theEnv, tmpstr, strlen(tmpstr) + 1);
      rtnFuzzyValue(theEnv, fvLeft);
      rtnFuzzyValue(theEnv, fvRight);
      fvLeft = fvTemp;
     }

   return fvLeft;
}

/******************************************************************
    ParseLExpr - parse OR expressions
 ******************************************************************/

static struct fuzzy_value *ParseLExpr(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct fuzzyLv *lvp,
  int *error)
{
   struct fuzzy_value *fvLeft, *fvRight, *fvTemp;
   char *tmpstr, *tmpstr2;

   fvLeft = ParseLTerm(theEnv, readSource, tempToken, lvp, error);

   if (*error)
     { return NULL; }

   if ((tempToken->tknType == SYMBOL_TOKEN) &&
       ((strcmp(tempToken->lexemeValue->contents, "OR") == 0) ||
        (strcmp(tempToken->lexemeValue->contents, "or") == 0)))
     {
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);
      fvRight = ParseLExpr(theEnv, readSource, tempToken, lvp, error);

      if (*error)
        {
         rtnFuzzyValue(theEnv, fvLeft);
         return NULL;
        }

      /* perform OR (union) */
      fvTemp = funion(theEnv, fvLeft, fvRight);

      tmpstr = modifyName(theEnv, "OR", fvRight->name);
      tmpstr2 = modifyName(theEnv, fvLeft->name, tmpstr);
      if (fvTemp->name != NULL)
        { genfree(theEnv, fvTemp->name, strlen(fvTemp->name) + 1); }
      fvTemp->name = tmpstr2;
      genfree(theEnv, tmpstr, strlen(tmpstr) + 1);
      rtnFuzzyValue(theEnv, fvLeft);
      rtnFuzzyValue(theEnv, fvRight);
      fvLeft = fvTemp;
     }

   return fvLeft;
}

/******************************************************************
    ParseLinguisticExpr

    Parses fuzzy expression with fuzzy terms, modifiers, AND,
    OR and brackets ([ and ]) ONLY.

    The BNF of the linguistic expressions is:

     <LExpr> ::= <LTerm> | <LTerm> OR <LExpr>
     <LTerm> ::= <modExpr> | <LTerm> AND <modExpr>
     <modExpr> ::= MODIFIER <modExpr> | <element>
     <element> ::= PRIMARY-TERM | [ <LExpr> ]

    Note: AND has higher precedence than OR.
 ******************************************************************/

struct fuzzy_value *ParseLinguisticExpr(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct fuzzyLv *lvp,
  int *error)
{
   struct fuzzy_value *fvptr;

   fvptr = ParseLExpr(theEnv, readSource, tempToken, lvp, error);

   if (*error)
     { return NULL; }

   if (tempToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      *error = 1;
      SyntaxErrorMessage(theEnv, "Fuzzy Expression (expecting ')' to terminate)");
      rtnFuzzyValue(theEnv, fvptr);
      return NULL;
     }

   PPBackup(theEnv); PPBackup(theEnv);
   SavePPBuffer(theEnv, ")");

   return fvptr;
}

/******************************************************************
    ModifyFuzzyValue - apply a modifier to a fuzzy value in place
 ******************************************************************/

void ModifyFuzzyValue(
  Environment *theEnv,
  struct modifierListItem *mptr,
  struct fuzzy_value *fv)
{
   char *tmpstr;

   if (fv == NULL) return;

   /* modify the name, e.g. cold --> very cold */
   tmpstr = modifyName(theEnv, mptr->name, fv->name);
   if (fv->name != NULL)
     { genfree(theEnv, fv->name, strlen(fv->name) + 1); }
   fv->name = tmpstr;

   /* apply the modifier function to the fuzzy set */
   executeModifyFunction(theEnv, fv, mptr);
}

/******************************************************************
    assertParseFuzzySet

    Parses assert of fuzzy sets. Expects current token to be LPAREN.
    Determines if it is a standard set (S, Z, PI) or a singleton set.
 ******************************************************************/

static Expression *assertParseFuzzySet(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  Deftemplate *theDeftemplate,
  int constantsOnly,
  int *onlyConstantsFound)
{
   Expression *parse_result;
   int function_type = -1;

   if (tempToken->tknType == LEFT_PARENTHESIS_TOKEN)
     {
      GetToken(theEnv, readSource, tempToken);
      if (tempToken->tknType == SYMBOL_TOKEN)
        {
         const char *tokenStr = tempToken->lexemeValue->contents;

         if (strcmp(tokenStr, "S") == 0 || strcmp(tokenStr, "s") == 0)
           { function_type = S_FUNCTION; }
         else if (strcmp(tokenStr, "Z") == 0 || strcmp(tokenStr, "z") == 0)
           { function_type = Z_FUNCTION; }
         else if (strcmp(tokenStr, "PI") == 0 || strcmp(tokenStr, "pi") == 0 ||
                  strcmp(tokenStr, "Pi") == 0)
           { function_type = PI_FUNCTION; }
        }
      if (function_type != -1)
        {
         parse_result = assertParseStandardSet(theEnv, readSource, tempToken, error,
                                               theDeftemplate, constantsOnly,
                                               onlyConstantsFound, function_type);
        }
      else
        {
         parse_result = assertParseSingletonSet(theEnv, readSource, tempToken, error,
                                                theDeftemplate, constantsOnly,
                                                onlyConstantsFound);
        }
     }
   else
     {
      *error = 1;
      SyntaxErrorMessage(theEnv, "Fuzzy Term (expecting Fuzzy Set description)");
      return NULL;
     }

   if (*error)
     { return NULL; }

   return parse_result;
}

/******************************************************************
    assertParseStandardSet

    Parses fuzzy sets of the form (fuzzyvar (S 4 8))
    Function_type has already been determined as one of PI_FUNCTION,
    S_FUNCTION, or Z_FUNCTION.
 ******************************************************************/

static Expression *assertParseStandardSet(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  Deftemplate *theDeftemplate,
  int constantsOnly,
  int *onlyConstantsFound,
  int function_type)
{
   Expression *top, *deft, *arg1, *arg2;

   *onlyConstantsFound = false;

   SavePPBuffer(theEnv, " ");

   top = get_struct(theEnv, expr);
   top->value = NULL;
   top->type = function_type;
   top->nextArg = NULL;
   top->argList = NULL;

   deft = GenConstant(theEnv, DEFTEMPLATE_PTR, (void *)theDeftemplate);
   top->argList = deft;

   /* get first parameter */
   GetToken(theEnv, readSource, tempToken);
   SavePPBuffer(theEnv, " ");

   arg1 = tokenToFloatExpression(theEnv, readSource, tempToken, error, constantsOnly);
   if (*error)
     {
      ReturnExpression(theEnv, top);
      return NULL;
     }
   deft->nextArg = arg1;

   /* get 2nd parameter */
   GetToken(theEnv, readSource, tempToken);
   arg2 = tokenToFloatExpression(theEnv, readSource, tempToken, error, constantsOnly);
   if (*error)
     {
      ReturnExpression(theEnv, top);
      return NULL;
     }
   arg1->nextArg = arg2;

   GetToken(theEnv, readSource, tempToken);
   if (tempToken->tknType == RIGHT_PARENTHESIS_TOKEN)
     {
      if (arg1->type == FLOAT_TYPE && arg2->type == FLOAT_TYPE)
        { *onlyConstantsFound = true; }
      GetToken(theEnv, readSource, tempToken);
      return top;
     }
   else
     {
      *error = 1;
      SyntaxErrorMessage(theEnv, "standard set (expecting ')' )");
      ReturnExpression(theEnv, top);
      return NULL;
     }
}

/******************************************************************
    assertParseSingletonSet

    Parses fuzzy sets of form (fuzzyvar (0 0) (5 .5) (7 1) (12 0))
 ******************************************************************/

static Expression *assertParseSingletonSet(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  Deftemplate *theDeftemplate,
  int constantsOnly,
  int *onlyConstantsFound)
{
   int count;
   Expression *top, *first, *next, *deft, *countExpr;

   *onlyConstantsFound = true;

   /* First token should be x coordinate */
   first = tokenToFloatExpression(theEnv, readSource, tempToken, error, constantsOnly);
   if (*error)
     {
      SyntaxErrorMessage(theEnv, "Singleton specification (Error in parsing Fuzzy Set x coordinate)");
      return NULL;
     }
   next = first;
   if (next->type != FLOAT_TYPE)
     { *onlyConstantsFound = false; }

   count = 0;

   while (true)
     {
      /* Get y coordinate */
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);
      next->nextArg = tokenToFloatExpression(theEnv, readSource, tempToken, error, constantsOnly);
      if (*error)
        {
         SyntaxErrorMessage(theEnv, "Singleton specification (Error in parsing Fuzzy Set y coordinate)");
         ReturnExpression(theEnv, first);
         return NULL;
        }
      next = next->nextArg;
      if (next->type != FLOAT_TYPE)
        { *onlyConstantsFound = false; }

      /* Get closing ')' for the (x,y) pair */
      GetToken(theEnv, readSource, tempToken);
      if (tempToken->tknType == RIGHT_PARENTHESIS_TOKEN)
        {
         count++;
         SavePPBuffer(theEnv, " ");
        }
      else
        {
         *error = 1;
         SyntaxErrorMessage(theEnv, "Singleton specification (Expected ')' )");
         ReturnExpression(theEnv, first);
         return NULL;
        }

      /* Next should be ')' (end of set) or '(' (start of next pair) */
      GetToken(theEnv, readSource, tempToken);
      if ((tempToken->tknType == RIGHT_PARENTHESIS_TOKEN) ||
          (tempToken->tknType == STOP_TOKEN))
        {
         top = get_struct(theEnv, expr);
         top->type = SINGLETON_EXPRESSION;
         top->value = NULL;
         top->nextArg = NULL;
         top->argList = NULL;

         deft = GenConstant(theEnv, DEFTEMPLATE_PTR, (void *)theDeftemplate);
         top->argList = deft;

         countExpr = GenConstant(theEnv, INTEGER_TYPE,
                                 (void *)CreateInteger(theEnv, (long long)count));
         deft->nextArg = countExpr;
         countExpr->nextArg = first;

         return top;
        }
      else if (tempToken->tknType != LEFT_PARENTHESIS_TOKEN)
        {
         *error = 1;
         SyntaxErrorMessage(theEnv, "Singleton specification (Expected '(' )");
         ReturnExpression(theEnv, first);
         return NULL;
        }

      /* Get next x coordinate */
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, tempToken);
      next->nextArg = tokenToFloatExpression(theEnv, readSource, tempToken, error, constantsOnly);
      if (*error)
        {
         SyntaxErrorMessage(theEnv, "Singleton specification (Error in parsing Fuzzy Set x coordinate)");
         ReturnExpression(theEnv, first);
         return NULL;
        }
      next = next->nextArg;
      if (next->type != FLOAT_TYPE)
        { *onlyConstantsFound = false; }
     }
}

/******************************************************************
    convertStandardSet

    Evaluates parameters of a standard fuzzy value and returns a
    fuzzy_value structure.
 ******************************************************************/

static struct fuzzy_value *convertStandardSet(
  Environment *theEnv,
  Expression *top,
  int *error)
{
   Expression *next;
   struct fuzzy_value *fv;
   double xtolerance;
   Deftemplate *deftPtr;
   struct fuzzyLv *fzLv;
   double from, to, alfa, beta, gamma;
   int function_type = top->type;

   /* get 1st parameter - deftemplate ptr */
   next = top->argList;
   deftPtr = (Deftemplate *)next->value;
   fzLv = deftPtr->fuzzyList;
   if (fzLv == NULL)
     {
      *error = 1;
      WriteString(theEnv, STDERR,
             "Standard Function (PI, S or Z) has no Fuzzy Deftemplate (possible internal error)\n");
      return NULL;
     }
   from = fzLv->from;
   to = fzLv->to;

   /* get 2nd parameter - alpha */
   next = next->nextArg;
   expressionToFloat(theEnv, next, &alfa, error);
   if (*error) return NULL;

   xtolerance = ((to - from) >= 1.0) ? FUZZY_TOLERANCE : (to - from) * FUZZY_TOLERANCE;

   if (function_type == PI_FUNCTION)
     {
      if (alfa < 0.0)
        {
         *error = 1;
         WriteString(theEnv, STDERR, "PI function 1st parameter must be >= 0\n");
         return NULL;
        }
      else
        { beta = alfa; }
     }
   else if (alfa < from)
     {
      if (from - alfa > xtolerance)
        {
         *error = 1;
         WriteString(theEnv, STDERR, "S or Z function 1st parameter out of range (too small)\n");
         return NULL;
        }
      alfa = from;
     }
   else if (alfa > to)
     {
      if (alfa - to > xtolerance)
        {
         *error = 1;
         WriteString(theEnv, STDERR, "S or Z function 1st parameter out of range (too large)\n");
         return NULL;
        }
      alfa = to;
     }

   /* get 3rd parameter */
   next = next->nextArg;
   expressionToFloat(theEnv, next, &gamma, error);
   if (*error) return NULL;

   if (function_type == PI_FUNCTION)
     {
      if ((gamma > to) || (gamma < from))
        {
         *error = 1;
         WriteString(theEnv, STDERR, "PI function produces x values out of range\n");
         return NULL;
        }
      else if ((gamma - beta) < from)
        {
         if (from - (gamma - beta) > xtolerance)
           {
            *error = 1;
            WriteString(theEnv, STDERR, "PI function produces x values too small\n");
            return NULL;
           }
         beta = gamma - from;
        }
      else if ((gamma + beta) > to)
        {
         if (gamma + beta - to > xtolerance)
           {
            *error = 1;
            WriteString(theEnv, STDERR, "PI function produces x values too large\n");
            return NULL;
           }
         beta = to - gamma;
        }
     }
   else if (gamma < alfa)
     {
      *error = 1;
      WriteString(theEnv, STDERR, "S or Z function 2nd parameter must be >= 1st parameter\n");
      return NULL;
     }
   else if (gamma > to)
     {
      if (gamma - to > xtolerance)
        {
         *error = 1;
         WriteString(theEnv, STDERR, "S or Z function 2nd parameter out of range (too large)\n");
         return NULL;
        }
      gamma = to;
     }

   /* Construct the fuzzy value */
   if (function_type != PI_FUNCTION)
     { beta = alfa; /* not used for S/Z but pass alfa as placeholder */ }

   fv = Get_S_Z_or_PI_FuzzyValue(theEnv, alfa, beta, gamma, function_type);
   fv->whichDeftemplate = deftPtr;

   return fv;
}

/******************************************************************
    convertSingletonSet

    Evaluates parameters of a singleton fuzzy set and returns a
    fuzzy_value structure.
 ******************************************************************/

static struct fuzzy_value *convertSingletonSet(
  Environment *theEnv,
  Expression *top,
  int *error)
{
   Expression *next;
   struct fuzzy_value *fv;
   int num, i, numpairs_retrieved;
   double newx, newy, previous;
   double from, to, xtolerance;
   Deftemplate *deftPtr;
   struct fuzzyLv *fzLv;

   /* get 1st parameter - deftemplate ptr */
   next = top->argList;
   deftPtr = (Deftemplate *)next->value;
   fzLv = deftPtr->fuzzyList;
   if (fzLv == NULL)
     {
      *error = 1;
      WriteString(theEnv, STDERR,
             "Singleton set has no Fuzzy Deftemplate (possible internal error)\n");
      return NULL;
     }
   from = fzLv->from;
   to = fzLv->to;

   /* get 2nd parameter - count */
   next = next->nextArg;
   expressionToInteger(theEnv, next, &num, error);
   if (*error) return NULL;

   fv = get_struct(theEnv, fuzzy_value);
   fv->name = (char *) genalloc(theEnv, 4);
   strcpy(fv->name, "???");
   fv->whichDeftemplate = deftPtr;
   fv->n = num;
   fv->maxn = num;
   fv->x = FgetArray(theEnv, num);
   fv->y = FgetArray(theEnv, num);

   previous = from - 1.0;
   next = next->nextArg;

   xtolerance = ((to - from) >= 1.0) ? FUZZY_TOLERANCE : (to - from) * FUZZY_TOLERANCE;

   i = 0;
   numpairs_retrieved = 0;

   while (numpairs_retrieved < num && next != NULL)
     {
      /* x coordinate */
      expressionToFloat(theEnv, next, &newx, error);
      if (!(*error))
        {
         if (newx > to)
           {
            if (newx - to > xtolerance)
              {
               *error = 1;
               WriteString(theEnv, STDERR, "X coordinate of Fuzzy Set out of range (too large)\n");
              }
            newx = to;
           }
         else if (newx < from)
           {
            if (from - newx > xtolerance)
              {
               *error = 1;
               WriteString(theEnv, STDERR, "X coordinate of Fuzzy Set out of range (too small)\n");
              }
            newx = from;
           }

         if (newx < previous)
           {
            if (previous - newx > FUZZY_TOLERANCE)
              {
               *error = 1;
               WriteString(theEnv, STDERR,
                      "(x,y) pairs should be in increasing x order in Fuzzy Set\n");
              }
            else
              { newx = previous; }
           }
        }
      if (*error)
        {
         rtnFuzzyValue(theEnv, fv);
         return NULL;
        }
      fv->x[i] = newx;
      previous = newx;

      /* y coordinate */
      next = next->nextArg;
      if (next == NULL)
        {
         *error = 1;
         WriteString(theEnv, STDERR, "Y coordinate of fuzzy set missing (possible internal error)\n");
         rtnFuzzyValue(theEnv, fv);
         return NULL;
        }
      expressionToFloat(theEnv, next, &newy, error);
      if (!(*error))
        {
         if (newy < 0.0)
           {
            if (newy < -FUZZY_TOLERANCE)
              {
               *error = 1;
               WriteString(theEnv, STDERR, "Fuzzy membership value (y coordinate) must be >= 0.0\n");
              }
            newy = 0.0;
           }
         if (newy > 1.0)
           {
            if (newy - 1.0 > FUZZY_TOLERANCE)
              {
               *error = 1;
               WriteString(theEnv, STDERR, "Fuzzy membership value (y coordinate) must be <= 1.0\n");
              }
            newy = 1.0;
           }
        }
      if (*error)
        {
         rtnFuzzyValue(theEnv, fv);
         return NULL;
        }

      /* if this point same as last don't store it */
      if (i == 0 || !FZ_EQUAL(newx, fv->x[i-1]) || !FZ_EQUAL(newy, fv->y[i-1]))
        {
         if ((i == 1 && newy == fv->y[0]) ||
             (i > 2 && newy == fv->y[i-1] && newy == fv->y[i-2]))
           {
            i--;
            fv->x[i] = newx;
            if (i == 0)
              { previous = from - 1.0; }
           }

         fv->y[i] = newy;
         i++;
        }

      /* handle multiple same x values */
      if (i > 2 && newx == fv->x[i-2] && newx == fv->x[i-3])
        {
         if ((newy > fv->y[i-2] && fv->y[i-2] > fv->y[i-3]) ||
             (newy < fv->y[i-2] && fv->y[i-2] < fv->y[i-3]) ||
             (i > 3 && newx == fv->x[i-4]))
           { i--; fv->y[i-1] = fv->y[i]; }
        }
      next = next->nextArg;
      numpairs_retrieved++;
     }

   if (numpairs_retrieved != num || next != NULL)
     {
      *error = 1;
      WriteString(theEnv, STDERR, "Fuzzy set - incorrect number of (x,y) pairs - internal error\n");
      rtnFuzzyValue(theEnv, fv);
      return NULL;
     }

   /* discard trailing duplicate y */
   if (i > 2 && fv->y[i-1] == fv->y[i-2])
     { i--; }

   fv->n = i;

   if ((num - i) > 5)
     { CompactFuzzyValue(theEnv, fv); }

   return fv;
}

/******************************************************************
    getConstantFuzzyValue

    Given an expression tree representing a singleton or standard
    fuzzy value, evaluate and return the fuzzy_value structure.
 ******************************************************************/

struct fuzzy_value *getConstantFuzzyValue(
  Environment *theEnv,
  Expression *top,
  int *error)
{
   struct fuzzy_value *new_fv = NULL;

   if (top->type == PI_FUNCTION ||
       top->type == Z_FUNCTION ||
       top->type == S_FUNCTION)
     {
      new_fv = convertStandardSet(theEnv, top, error);
      if (*error) return NULL;
     }
   else if (top->type == SINGLETON_EXPRESSION)
     {
      new_fv = convertSingletonSet(theEnv, top, error);
      if (*error) return NULL;
     }

   return new_fv;
}

/******************************************************************
    ParseAssertFuzzyFact

    Parses the fuzzy portion of an assert for a fuzzy deftemplate.
    Returns an Expression node wrapping the fuzzy value.
 ******************************************************************/

Expression *ParseAssertFuzzyFact(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  int endType,
  int constantsOnly,
  Deftemplate *theDeftemplate,
  int variablesAllowed)
{
   struct fuzzyLv *lvp = theDeftemplate->fuzzyList;
   Expression *next_one, *temp;
   struct fuzzy_value *fv;
   int onlyConstantsFound;

   (void) endType; /* always RPAREN */

   *error = 0;

   /* Space between template name and fuzzy set definition */
   SavePPBuffer(theEnv, " ");
   GetToken(theEnv, readSource, tempToken);

   /* Handle variables */
   if (tempToken->tknType == SF_VARIABLE_TOKEN || tempToken->tknType == GBL_VARIABLE_TOKEN)
     {
      struct token dummyToken;

      if (constantsOnly || !variablesAllowed)
        {
         *error = 1;
         SyntaxErrorMessage(theEnv, "deftemplate pattern (Variables not allowed)");
         return NULL;
        }

      GetToken(theEnv, readSource, &dummyToken);
      if (dummyToken.tknType != RIGHT_PARENTHESIS_TOKEN)
        {
         *error = 1;
         SyntaxErrorMessage(theEnv, "Fuzzy Expression (expecting ')' to terminate)");
         return NULL;
        }
      else
        { return GenConstant(theEnv, TokenTypeToType(tempToken->tknType), tempToken->value); }
     }
   /* Handle fuzzy set specified by ( or # ( */
   else if ((tempToken->tknType == LEFT_PARENTHESIS_TOKEN) ||
            ((tempToken->tknType == SYMBOL_TOKEN) &&
             (strcmp(tempToken->lexemeValue->contents, "#") == 0)))
     {
      if (tempToken->tknType == SYMBOL_TOKEN)
        {
         SavePPBuffer(theEnv, " ");
         GetToken(theEnv, readSource, tempToken);
        }

      next_one = assertParseFuzzySet(theEnv, readSource, tempToken, error,
                                     theDeftemplate, constantsOnly, &onlyConstantsFound);
      if (*error)
        { return NULL; }

      if (onlyConstantsFound)
        {
         fv = getConstantFuzzyValue(theEnv, next_one, error);
         if (*error)
           {
            ReturnExpression(theEnv, next_one);
            return NULL;
           }
         temp = get_struct(theEnv, expr);
         temp->argList = NULL;
         temp->nextArg = NULL;
         temp->type = FUZZY_VALUE_TYPE;
         temp->value = (void *)AddFuzzyValue(theEnv, fv);
         rtnFuzzyValue(theEnv, fv);
         ReturnExpression(theEnv, next_one);
         return temp;
        }
      else
        { return next_one; }
     }
   /* Otherwise try to parse a linguistic expression */
   else if ((fv = ParseLinguisticExpr(theEnv, readSource, tempToken, lvp, error)) == NULL)
     {
      *error = 1;
      return NULL;
     }
   else
     {
      next_one = get_struct(theEnv, expr);
      next_one->argList = NULL;
      next_one->nextArg = NULL;
      next_one->type = FUZZY_VALUE_TYPE;
      next_one->value = (void *)AddFuzzyValue(theEnv, fv);
      rtnFuzzyValue(theEnv, fv);
      return next_one;
     }
}

/******************************************************************
    tokenToFloatExpression

    Given the next token, parse constants, variables and functions
    and return appropriate Expression structures for numeric values.
 ******************************************************************/

Expression *tokenToFloatExpression(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  int constantsOnly)
{
   Expression *result = NULL;

   if (tempToken->tknType == FLOAT_TOKEN || tempToken->tknType == INTEGER_TOKEN)
     {
      /* Constant - convert INTEGER to FLOAT */
      if (tempToken->tknType == INTEGER_TOKEN)
        {
         result = GenConstant(theEnv, FLOAT_TYPE,
                              (void *)CreateFloat(theEnv,
                                  (double)tempToken->integerValue->contents));
        }
      else
        { result = GenConstant(theEnv, FLOAT_TYPE, tempToken->value); }

      return result;
     }

   /* Function call: = expr or ( expr ) */
   if ((tempToken->tknType == SYMBOL_TOKEN) ?
       (strcmp(tempToken->lexemeValue->contents, "=") == 0) :
       (tempToken->tknType == LEFT_PARENTHESIS_TOKEN))
     {
      if (constantsOnly)
        {
         SyntaxErrorMessage(theEnv, "numeric expression (Constants Only Allowed)");
         *error = 1;
         return NULL;
        }

#if (! RUN_TIME)
      if (tempToken->tknType == LEFT_PARENTHESIS_TOKEN)
        { result = Function1Parse(theEnv, readSource); }
      else
        { result = Function0Parse(theEnv, readSource); }
#endif
      if (result == NULL)
        { *error = 1; }
      else
        {
         if (result->type == FCALL)
           {
            unsigned int retType = ExpressionUnknownFunctionType(result);
            /* Check that function returns a numeric type */
            if (!(retType & (INTEGER_BIT | FLOAT_BIT)))
              {
               SyntaxErrorMessage(theEnv, "numeric expression (Expected numeric result from function)");
               *error = 1;
               ReturnExpression(theEnv, result);
               return NULL;
              }
           }
        }

      return result;
     }

   /* Variables */
   if ((tempToken->tknType == SF_VARIABLE_TOKEN)
#if DEFGLOBAL_CONSTRUCT
       || (tempToken->tknType == GBL_VARIABLE_TOKEN)
#endif
      )
     {
      if (constantsOnly)
        {
         *error = 1;
         return NULL;
        }

      return GenConstant(theEnv, TokenTypeToType(tempToken->tknType), tempToken->value);
     }

   /* Nothing matched - error */
   SyntaxErrorMessage(theEnv, "singleton or standard fuzzy set (Numeric expression expected)");
   *error = 1;
   return NULL;
}

#endif /* FUZZY_DEFTEMPLATES */
