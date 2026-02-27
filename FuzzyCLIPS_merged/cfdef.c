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
    Globals
 ******************************************************************/

double Threshold_CF = 0.0;

/******************************************************************
    InitializeCF - register certainty factor commands
 ******************************************************************/

void InitializeCF(
  Environment *theEnv)
{
   AddUDF(theEnv, "get-threshold", "d", 0, 0, NULL,
          get_threshold, "get_threshold", NULL);

   AddUDF(theEnv, "set-threshold", "d", 1, 1, "ld",
          set_threshold, "set_threshold", NULL);

   AddUDF(theEnv, "get-cf", "d", 1, 1, "*",
          getcf, "getcf", NULL);

   AddUDF(theEnv, "unthreshold", "v", 0, 0, NULL,
          unthreshold, "unthreshold", NULL);

   AddUDF(theEnv, "enable-rule-cf-calculation", "v", 0, 0, NULL,
          enable_rule_cf_calculation, "enable_rule_cf_calculation", NULL);

   AddUDF(theEnv, "disable-rule-cf-calculation", "v", 0, 0, NULL,
          disable_rule_cf_calculation, "disable_rule_cf_calculation", NULL);

   Threshold_CF = 0.0;
}

/******************************************************************
    get_threshold / set_threshold
 ******************************************************************/

void get_threshold(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, Threshold_CF);
}

void set_threshold(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   UDFValue theArg;
   double oldThreshold = Threshold_CF;

   if (!UDFFirstArgument(context, NUMBER_BITS, &theArg))
     { return; }

   Threshold_CF = CVCoerceToFloat(&theArg);

   if (Threshold_CF < 0.0 || Threshold_CF > 1.0)
     {
      WriteString(theEnv, STDERR, "Threshold must be between 0.0 and 1.0\n");
      Threshold_CF = oldThreshold;
     }

   returnValue->floatValue = CreateFloat(theEnv, oldThreshold);
}

void unthreshold(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   Threshold_CF = 0.0;
}

/******************************************************************
    getcf - get the CF of a fact
 ******************************************************************/

void getcf(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   /* TODO: Port from original - gets the CF of a fact */
   returnValue->floatValue = CreateFloat(theEnv, 1.0);
}

/******************************************************************
    enable/disable rule CF calculation
 ******************************************************************/

void enable_rule_cf_calculation(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   /* TODO: Enable CF calculation for rules */
}

void disable_rule_cf_calculation(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
{
   /* TODO: Disable CF calculation for rules */
}

/******************************************************************
    combineCF - combine two certainty factors
 ******************************************************************/

static double combineCF(
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
    computeStdConclCF
 ******************************************************************/

double computeStdConclCF(
  Environment *theEnv,
  double theRuleCF,
  struct partialMatch *binds)
{
   /* TODO: Port full computation from original FuzzyCLIPS */
   return theRuleCF;
}

#if FUZZY_DEFTEMPLATES
double computeFuzzyCrispConclCF(
  Environment *theEnv,
  Defrule *theRule,
  struct partialMatch *binds)
{
   /* TODO: Port from original FuzzyCLIPS */
   return 1.0;
}
#endif

/******************************************************************
    changeCFofNewFact / changeCFofExistingFact / changeCFofNewVsExistingFact
 ******************************************************************/

void changeCFofNewFact(
  Environment *theEnv,
  Fact *newfact)
{
   /* TODO: Port from original */
}

void changeCFofExistingFact(
  Environment *theEnv,
  Fact *fact1,
  Fact *fact2)
{
   /* Use CF combination formula */
   double cf1, cf2;
   cf1 = fact1->certaintyFactor;
   cf2 = fact2->certaintyFactor;
   fact1->certaintyFactor = combineCF(cf1, cf2);
}

void changeCFofNewVsExistingFact(
  Environment *theEnv,
  Fact *fact1,
  Fact *fact2)
{
   /* TODO: Port from original */
}

/******************************************************************
    Error reporting
 ******************************************************************/

void cfInformationError(
  Environment *theEnv,
  const char *msg)
{
   WriteString(theEnv, STDERR, "Certainty factor error: ");
   WriteString(theEnv, STDERR, msg);
   WriteString(theEnv, STDERR, "\n");
}

void cfRangeError(
  Environment *theEnv)
{
   WriteString(theEnv, STDERR, "Certainty factor out of range (must be -1.0 to 1.0)\n");
}

void cfNonNumberError(
  Environment *theEnv)
{
   WriteString(theEnv, STDERR, "Certainty factor must be a numeric value\n");
}

void printCF(
  Environment *theEnv,
  const char *logicalName,
  double cf)
{
   char buffer[64];
   snprintf(buffer, sizeof(buffer), " CF %.2f", cf);
   WriteString(theEnv, logicalName, buffer);
}

/******************************************************************
    ParseDeclareUncertainty
 ******************************************************************/

Expression *ParseDeclareUncertainty(
  Environment *theEnv,
  const char *readSource,
  const char *ruleName,
  int *error,
  double *cfVALUE)
{
   /* TODO: Port parser from original FuzzyCLIPS */
   *cfVALUE = 1.0;
   *error = 0;
   return NULL;
}

/******************************************************************
    Fuzzy similarity measures (when FUZZY_DEFTEMPLATES)
 ******************************************************************/

#if FUZZY_DEFTEMPLATES

double possibility(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   /* TODO: Port from original */
   return 0.0;
}

double necessity(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   /* TODO: Port from original */
   return 0.0;
}

double similarity(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   /* TODO: Port from original */
   return 0.0;
}

#endif /* FUZZY_DEFTEMPLATES */

#endif /* CERTAINTY_FACTORS */
