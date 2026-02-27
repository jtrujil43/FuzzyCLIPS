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
    FgetArray / FrtnArray - allocate/free double arrays for fuzzy sets
 ******************************************************************/

double *FgetArray(
  Environment *theEnv,
  int size)
{
   double *arr;

   if (size <= 0) return NULL;
   arr = (double *) genalloc(theEnv, sizeof(double) * size);
   return arr;
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
   newFv->maxn = fv->n; /* compact copy */
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

struct fuzzy_value *CompactFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   double *newx, *newy;
   int i;

   if (fv == NULL) return NULL;

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

   return fv;
}

/******************************************************************
    ParseLinguisticExpr - parse a linguistic expression
 ******************************************************************/

struct fuzzy_value *ParseLinguisticExpr(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  Deftemplate *theDeftemplate,
  int *error)
{
   /* TODO: Port full linguistic expression parser from original FuzzyCLIPS */
   /* This needs to handle:
      - Primary term names (cold, warm, hot, etc.)
      - Hedges/modifiers (very, somewhat, more-or-less, etc.)
      - S-function, Z-function, PI-function expressions
      - Singleton set expressions like (1.0/2.0 0.5/4.0)
      - Combination with AND/OR
   */
   *error = 1;
   WriteString(theEnv, STDERR, "ParseLinguisticExpr: Not yet fully implemented\n");
   return NULL;
}

/******************************************************************
    ParseAssertFuzzyFact - parse fuzzy part of assert
 ******************************************************************/

struct fuzzy_value *ParseAssertFuzzyFact(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  int *error,
  int constantsOnly)
{
   /* TODO: Port from original FuzzyCLIPS
      Should handle both singleton form and linguistic form
   */
   *error = 1;
   WriteString(theEnv, STDERR, "ParseAssertFuzzyFact: Not yet fully implemented\n");
   return NULL;
}

#endif /* FUZZY_DEFTEMPLATES */
