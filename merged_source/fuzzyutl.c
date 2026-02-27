   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*            FUZZY UTILITIES MODULE                   */
   /*******************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzyutl.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzyrhs.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "symbol.h"
#include "factmngr.h"

int saveFactsInProgress = 0;

/***** FZ_EQUAL: Test if two doubles are equal within fuzzy tolerance *****/

int FZ_EQUAL(
  double f1,
  double f2)
{
   double diff = f1 - f2;
   if (diff < 0.0) diff = -diff;
   return (diff < FUZZY_TOLERANCE);
}

/***** fcompliment: Compute complement of a fuzzy value *****/

void fcompliment(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   int i;
   for (i = 0; i < fv->n; i++)
     { fv->y[i] = 1.0 - fv->y[i]; }
}

/***** funion: Compute union of two fuzzy values *****/

struct fuzzy_value *funion(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   /* TODO: Port full union algorithm from original FuzzyCLIPS */
   struct fuzzy_value *result;

   result = CopyFuzzyValue(theEnv, f1);
   return result;
}

/***** fintersect: Compute intersection of two fuzzy values *****/

struct fuzzy_value *fintersect(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   /* TODO: Port full intersection algorithm from original FuzzyCLIPS */
   struct fuzzy_value *result;

   result = CopyFuzzyValue(theEnv, f1);
   return result;
}

/***** max_of_min: Compute maximum of minimum membership values *****/

double max_of_min(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   /* TODO: Port from original FuzzyCLIPS */
   return 0.0;
}

/***** maxmin_intersect *****/

double maxmin_intersect(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2,
  int DoIntersect,
  struct fuzzy_value **intersectSet)
{
   /* TODO: Port from original FuzzyCLIPS */
   return 0.0;
}

/***** nonintersectiontest *****/

int nonintersectiontest(
  double *Ax, double *Ay,
  double *Bx, double *By,
  int Asize, int Bsize)
{
   /* TODO: Port from original FuzzyCLIPS */
   return 0;
}

/***** computeFuzzyConsequence *****/

void computeFuzzyConsequence(
  Environment *theEnv,
  Fact *new_fact)
{
   /* TODO: Port from original FuzzyCLIPS */
}

/***** changeValueOfFuzzySlots *****/

void changeValueOfFuzzySlots(
  Environment *theEnv,
  Fact *fact1,
  Fact *fact2)
{
   /* TODO: Port from original FuzzyCLIPS */
}

/***** PrintFuzzyTemplateFact *****/

void PrintFuzzyTemplateFact(
  Environment *theEnv,
  const char *logName,
  struct fuzzy_value *fv
#if CERTAINTY_FACTORS
  ,double CF
#endif
  )
{
   if (fv == NULL) return;
   if (fv->name != NULL)
     { WriteString(theEnv, logName, fv->name); }
   else
     { PrintFuzzySet(theEnv, logName, fv); }
#if CERTAINTY_FACTORS
   {
     char buf[64];
     snprintf(buf, sizeof(buf), " CF %.2f", CF);
     WriteString(theEnv, logName, buf);
   }
#endif
}

/***** PrintFuzzySet *****/

void PrintFuzzySet(
  Environment *theEnv,
  const char *logName,
  struct fuzzy_value *fv)
{
   int i;
   char buf[128];

   if (fv == NULL) return;

   WriteString(theEnv, logName, "( ");
   for (i = 0; i < fv->n; i++)
     {
      snprintf(buf, sizeof(buf), "%g/%g ", fv->y[i], fv->x[i]);
      WriteString(theEnv, logName, buf);
     }
   WriteString(theEnv, logName, ")");
}

#endif /* FUZZY_DEFTEMPLATES */
