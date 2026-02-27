   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY TEMPLATE PARSER MODULE               */
   /*******************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzypsr.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzyrhs.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"
#include "pprint.h"

/******************************************************************
    Global arrays for S, Z, PI function y values
 ******************************************************************/

#define ArraySIZE 101

static double S_array[ArraySIZE];
static double Z_array[ArraySIZE];
static double PI_array[ArraySIZE];

/******************************************************************
    sFunction - compute the S-function value
 ******************************************************************/

double sFunction(
  double x,
  double alfa,
  double beta,
  double gamma)
{
   double denom;

   if (x <= alfa) return 0.0;
   if (x >= gamma) return 1.0;
   if (x <= beta)
     {
      denom = (gamma - alfa);
      if (denom == 0.0) return 0.0;
      return 2.0 * ((x - alfa) / denom) * ((x - alfa) / denom);
     }
   else
     {
      denom = (gamma - alfa);
      if (denom == 0.0) return 1.0;
      return 1.0 - 2.0 * ((x - gamma) / denom) * ((x - gamma) / denom);
     }
}

/******************************************************************
    Init_S_Z_PI_yvalues - precompute S, Z, PI arrays
 ******************************************************************/

void Init_S_Z_PI_yvalues(
  Environment *theEnv)
{
   int i;
   double x;

   for (i = 0; i < ArraySIZE; i++)
     {
      x = (double)i / (double)(ArraySIZE - 1);
      S_array[i] = sFunction(x, 0.0, 0.5, 1.0);
      Z_array[i] = 1.0 - S_array[i];
     }

   for (i = 0; i < ArraySIZE; i++)
     {
      x = (double)i / (double)(ArraySIZE - 1);
      if (x <= 0.5)
        PI_array[i] = sFunction(x * 2.0, 0.0, 0.5, 1.0);
      else
        PI_array[i] = 1.0 - sFunction((x - 0.5) * 2.0, 0.0, 0.5, 1.0);
     }
}

/******************************************************************
    Get_S_Z_or_PI_FuzzyValue
 ******************************************************************/

struct fuzzy_value *Get_S_Z_or_PI_FuzzyValue(
  Environment *theEnv,
  double alfa,
  double beta,
  double gamma,
  int function_type)
{
   struct fuzzy_value *fv;
   int i;
   double range, x;

   fv = get_struct(theEnv, fuzzy_value);
   fv->whichDeftemplate = NULL;
   fv->name = NULL;
   fv->n = ArraySIZE;
   fv->maxn = ArraySIZE;
   fv->x = FgetArray(theEnv, ArraySIZE);
   fv->y = FgetArray(theEnv, ArraySIZE);

   range = gamma - alfa;

   for (i = 0; i < ArraySIZE; i++)
     {
      x = alfa + range * ((double)i / (double)(ArraySIZE - 1));
      fv->x[i] = x;
      switch (function_type)
        {
         case S_FUNCTION:
            fv->y[i] = S_array[i];
            break;
         case Z_FUNCTION:
            fv->y[i] = Z_array[i];
            break;
         case PI_FUNCTION:
            fv->y[i] = PI_array[i];
            break;
         default:
            fv->y[i] = 0.0;
            break;
        }
     }

   return fv;
}

/******************************************************************
    ParseFuzzyTemplate - parse a fuzzy deftemplate
 ******************************************************************/

struct fuzzyLv *ParseFuzzyTemplate(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError)
{
   /* TODO: Port full parser from original FuzzyCLIPS */
   *DeftemplateError = 1;
   WriteString(theEnv, STDERR, "ParseFuzzyTemplate: Not yet fully implemented\n");
   return NULL;
}

/******************************************************************
    RtnFuzzyTemplate - free a fuzzyLv structure
 ******************************************************************/

void RtnFuzzyTemplate(
  Environment *theEnv,
  struct fuzzyLv *lv)
{
   if (lv == NULL) return;
   /* TODO: Free primary_term_list items */
   rtn_struct(theEnv, fuzzyLv, lv);
}

/******************************************************************
    rtnFuzzyValue - free a fuzzy_value structure
 ******************************************************************/

void rtnFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   if (fv == NULL) return;

   if (fv->x != NULL) FrtnArray(theEnv, fv->x, fv->maxn);
   if (fv->y != NULL) FrtnArray(theEnv, fv->y, fv->maxn);
   if (fv->name != NULL)
     { genfree(theEnv, fv->name, strlen(fv->name) + 1); }

   rtn_struct(theEnv, fuzzy_value, fv);
}

/******************************************************************
    InstallFuzzyValue / DeinstallFuzzyValue
 ******************************************************************/

void InstallFuzzyValue(
  Environment *theEnv,
  void *fv)
{
   if (fv != NULL)
     { IncrementFuzzyValueCount(fv); }
}

void DeinstallFuzzyValue(
  Environment *theEnv,
  void *fv)
{
   if (fv != NULL)
     {
      CLIPSFuzzyValue *fvhn = (CLIPSFuzzyValue *) fv;
      ReleaseFuzzyValue(theEnv, fvhn);
     }
}

/******************************************************************
    InstallFuzzyTemplate / DeinstallFuzzyTemplate
 ******************************************************************/

void InstallFuzzyTemplate(
  Environment *theEnv,
  Deftemplate *theDeftemplate)
{
   /* TODO: Port from original FuzzyCLIPS */
}

void DeinstallFuzzyTemplate(
  Environment *theEnv,
  struct fuzzyLv *fzTemplate)
{
   /* TODO: Port from original FuzzyCLIPS */
}

#endif /* FUZZY_DEFTEMPLATES */
