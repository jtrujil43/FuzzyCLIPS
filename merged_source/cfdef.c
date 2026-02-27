   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*           CERTAINTY FACTORS MODULE                  */
   /*******************************************************/

#include "setup.h"

#if CERTAINTY_FACTORS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "constant.h"
#include "envrnmnt.h"
#include "cfdef.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "pprint.h"

/******************************************************************
    CF Threshold global - facts below this CF are removed
 ******************************************************************/

static double CFthreshold = 0.0;

/******************************************************************
    InitializeCF - register certainty factor commands
 ******************************************************************/

void InitializeCF(
  Environment *theEnv)
{
   AddUDF(theEnv, "get-threshold", "d", 0, 0, NULL,
          getCFthreshold, "getCFthreshold", NULL);

   AddUDF(theEnv, "set-threshold", "d", 1, 1, "ld",
          setCFthreshold, "setCFthreshold", NULL);

   AddUDF(theEnv, "get-cf", "d", 1, 1, "*",
          getCFaliasCommand, "getCFaliasCommand", NULL);

   CFthreshold = 0.0;
}

/******************************************************************
    getCFthreshold / setCFthreshold
 ******************************************************************/

void getCFthreshold(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, CFthreshold);
}

void setCFthreshold(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   UDFValue theArg;
   double oldThreshold = CFthreshold;

   if (!UDFFirstArgument(context, NUMBER_BITS, &theArg))
     { return; }

   CFthreshold = CVCoerceToFloat(&theArg);

   if (CFthreshold < 0.0 || CFthreshold > 1.0)
     {
      WriteString(theEnv, STDERR, "Threshold must be between 0.0 and 1.0\n");
      CFthreshold = oldThreshold;
     }

   returnValue->floatValue = CreateFloat(theEnv, oldThreshold);
}

/******************************************************************
    getCFaliasCommand
 ******************************************************************/

void getCFaliasCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   /* TODO: Port from original - gets the CF of a fact */
   returnValue->floatValue = CreateFloat(theEnv, 1.0);
}

/******************************************************************
    combineCF - combine two certainty factors
    Uses the formula:
      if both >= 0: CF1 + CF2*(1 - CF1)
      if both < 0: CF1 + CF2*(1 + CF1)
      otherwise: (CF1 + CF2) / (1 - min(|CF1|, |CF2|))
 ******************************************************************/

double combineCF(
  double cf1,
  double cf2)
{
   double minAbs;

   if (cf1 >= 0.0 && cf2 >= 0.0)
     { return cf1 + cf2 * (1.0 - cf1); }
   else if (cf1 < 0.0 && cf2 < 0.0)
     { return cf1 + cf2 * (1.0 + cf1); }
   else
     {
      minAbs = fabs(cf1);
      if (fabs(cf2) < minAbs) minAbs = fabs(cf2);
      if (minAbs == 1.0) return 0.0;
      return (cf1 + cf2) / (1.0 - minAbs);
     }
}

/******************************************************************
    computeRuleCF - compute the CF of a rule's RHS
 ******************************************************************/

double computeRuleCF(
  double ruleCF,
  double factCF)
{
   return ruleCF * factCF;
}

/******************************************************************
    computeStdConclCF - compute CF for a standard conclusion
 ******************************************************************/

double computeStdConclCF(
  double ruleCF,
  double *factCFs,
  int numFacts)
{
   double minCF;
   int i;

   if (numFacts <= 0) return ruleCF;

   minCF = factCFs[0];
   for (i = 1; i < numFacts; i++)
     {
      if (factCFs[i] < minCF)
        minCF = factCFs[i];
     }

   return ruleCF * minCF;
}

/******************************************************************
    cfRHSParse - parse the CF specification from rule RHS
    Returns CF value or -1.0 on error
    Looks for (CF <number>) token
 ******************************************************************/

double cfRHSParse(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken)
{
   /* TODO: Port full parser from original FuzzyCLIPS */
   return 1.0; /* default CF is 1.0 */
}

/******************************************************************
    GetCFaliasPtr
 ******************************************************************/

double GetCF(
  void *theFact)
{
   /* TODO: Access the CF field from the fact structure */
   return 1.0;
}

#endif /* CERTAINTY_FACTORS */
