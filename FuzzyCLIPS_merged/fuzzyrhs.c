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
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"
#include "pprint.h"

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
    ParseLinguisticExpr
 ******************************************************************/

struct fuzzy_value *ParseLinguisticExpr(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct fuzzyLv *lvp,
  int *error)
{
   /* TODO: Port full linguistic expression parser from original FuzzyCLIPS */
   *error = 1;
   WriteString(theEnv, STDERR, "ParseLinguisticExpr: Not yet fully implemented\n");
   return NULL;
}

/******************************************************************
    ParseAssertFuzzyFact
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
   /* TODO: Port from original FuzzyCLIPS */
   *error = 1;
   WriteString(theEnv, STDERR, "ParseAssertFuzzyFact: Not yet fully implemented\n");
   return NULL;
}

/******************************************************************
    getConstantFuzzyValue
 ******************************************************************/

struct fuzzy_value *getConstantFuzzyValue(
  Environment *theEnv,
  Expression *top,
  int *error)
{
   /* TODO: Port from original FuzzyCLIPS */
   *error = 0;
   return NULL;
}

/******************************************************************
    ModifyFuzzyValue - apply a modifier to a fuzzy value in place
 ******************************************************************/

void ModifyFuzzyValue(
  Environment *theEnv,
  struct modifierListItem *mptr,
  struct fuzzy_value *elem)
{
   /* TODO: Port from original FuzzyCLIPS */
}

/******************************************************************
    tokenToFloatExpression
 ******************************************************************/

Expression *tokenToFloatExpression(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  int constantsOnly)
{
   /* TODO: Port from original FuzzyCLIPS */
   *error = 0;
   return NULL;
}

#endif /* FUZZY_DEFTEMPLATES */
