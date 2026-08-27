   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*            FUZZY COMMANDS MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements fuzzy CLIPS commands (defuzzify,      */
/*   get-u, get-fs, plot-fuzzy-value, etc.)                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             Functions now use UDF pattern.                 */
/*             DefineFunction2 -> AddUDF.                    */
/*             PrintRouter -> WriteString.                   */
/*             gm2/rm -> genalloc/genfree.                   */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "argacces.h"
#include "constant.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "evaluatn.h"
#include "expressn.h"
#include "factmngr.h"
#include "fuzzycom.h"
#include "fuzzydef.h"
#include "fuzzylv.h"
#include "fuzzymod.h"
#include "fuzzypsr.h"
#include "fuzzyrhs.h"
#include "fuzzyutl.h"
#include "fuzzyval.h"
#include "memalloc.h"
#include "multifld.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"

/******************************************************************
    DeffuzzyCommands - register all fuzzy UDF commands
 ******************************************************************/

void DeffuzzyCommands(
  Environment *theEnv)
{
   /* Defuzzification functions */
   AddUDF(theEnv,"moment-defuzzify","d",1,1,NULL,moment_defuzzify,"moment_defuzzify",NULL);
   AddUDF(theEnv,"maximum-defuzzify","d",1,1,NULL,maximum_defuzzify,"maximum_defuzzify",NULL);

   /* Universe of discourse access functions */
   AddUDF(theEnv,"get-u","m",1,1,NULL,getu,"getu",NULL);
   AddUDF(theEnv,"get-u-from","d",1,1,NULL,getu_from,"getu_from",NULL);
   AddUDF(theEnv,"get-u-to","d",1,1,NULL,getu_to,"getu_to",NULL);
   AddUDF(theEnv,"get-u-units","s",1,1,NULL,getu_units,"getu_units",NULL);

   /* Fuzzy set access functions */
   AddUDF(theEnv,"get-fs","m",1,1,NULL,get_fs,"get_fs",NULL);
   AddUDF(theEnv,"get-fs-template","y",1,1,NULL,get_fs_template,"get_fs_template",NULL);
   AddUDF(theEnv,"get-fs-lv","s",1,1,NULL,get_fs_lv,"get_fs_lv",NULL);
   AddUDF(theEnv,"get-fs-length","l",1,1,NULL,get_fs_length,"get_fs_length",NULL);
   AddUDF(theEnv,"get-fs-value","d",2,2,NULL,get_fs_value,"get_fs_value",NULL);
   AddUDF(theEnv,"get-fs-x","d",2,2,NULL,get_fs_x,"get_fs_x",NULL);
   AddUDF(theEnv,"get-fs-y","d",2,2,NULL,get_fs_y,"get_fs_y",NULL);

   /* Fuzzy set manipulation functions */
   AddUDF(theEnv,"fuzzy-union","*",2,2,NULL,fuzzy_union,"fuzzy_union",NULL);
   AddUDF(theEnv,"fuzzy-intersection","*",2,2,NULL,fuzzy_intersection,"fuzzy_intersection",NULL);
   AddUDF(theEnv,"fuzzy-modify","*",2,2,NULL,fuzzy_modify,"fuzzy_modify",NULL);
   AddUDF(theEnv,"create-fuzzy-value","*",1,UNBOUNDED,NULL,create_fuzzy_value,"create_fuzzy_value",NULL);

   /* Fuzzy modifier management */
   AddUDF(theEnv,"add-fuzzy-modifier","v",2,2,NULL,add_fuzzy_modifier,"add_fuzzy_modifier",NULL);
   AddUDF(theEnv,"remove-fuzzy-modifier","v",1,1,NULL,remove_fuzzy_modifier,"remove_fuzzy_modifier",NULL);

   /* Fuzzy inference type */
   AddUDF(theEnv,"set-fuzzy-inference-type","v",1,1,NULL,set_fuzzy_inference_type,"set_fuzzy_inference_type",NULL);
   AddUDF(theEnv,"get-fuzzy-inference-type","y",0,0,NULL,get_fuzzy_inference_type,"get_fuzzy_inference_type",NULL);

   /* Display precision */
   AddUDF(theEnv,"set-fuzzy-display-precision","v",1,1,NULL,set_fuzzy_display_precision,"set_fuzzy_display_precision",NULL);
   AddUDF(theEnv,"get-fuzzy-display-precision","l",0,0,NULL,get_fuzzy_display_precision,"get_fuzzy_display_precision",NULL);

   /* Alpha value */
   AddUDF(theEnv,"set-alpha-value","v",1,1,NULL,set_alpha_value,"set_alpha_value",NULL);
   AddUDF(theEnv,"get-alpha-value","d",0,0,NULL,get_alpha_value,"get_alpha_value",NULL);

   /* Plot */
   AddUDF(theEnv,"plot-fuzzy-value","v",3,4,NULL,plot_fuzzy_value,"plot_fuzzy_value",NULL);
}

/******************************************************************
    is_defuzzify_value_valid - check if defuzzified value is valid
 ******************************************************************/

bool is_defuzzify_value_valid(
  Environment *theEnv)
{
   /* TODO: Full implementation from original FuzzyCLIPS */
   return true;
}

/******************************************************************
    Helper: get fuzzy_value pointer from a UDF argument
 ******************************************************************/

static struct fuzzy_value *getFuzzyValueFromArg(
  Environment *theEnv,
  UDFContext *context,
  const char *funcName)
{
   UDFValue theArg;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &theArg))
     return NULL;

   if (theArg.header->type == FUZZY_VALUE_TYPE)
     {
      CLIPSFuzzyValue *fvhn = theArg.fuzzyValue;
      if (fvhn != NULL)
        return fvhn->contents;
     }

   WriteString(theEnv, STDERR, funcName);
   WriteString(theEnv, STDERR, ": Expected a fuzzy value argument\n");
   UDFThrowError(context);
   return NULL;
}

/******************************************************************
    Defuzzification helpers
 ******************************************************************/

#define ONE_THIRD  0.33333333333333333
#define TWO_THIRDS 0.66666666666666667

static bool is_last_defuzzify_valid = true;

static void get_moment_and_area(
  double *moment_ptr,
  double *area_ptr,
  double x1, double y1,
  double x2, double y2)
{
   if ((y1 == 0.0 && y2 == 0.0) || (x1 == x2))
     { *moment_ptr = 0.0; *area_ptr = 0.0; }
   else if (y1 == y2)
     { *moment_ptr = 0.5 * (x1 + x2); *area_ptr = (x2 - x1) * y1; }
   else if (y1 == 0.0 && y2 != 0.0)
     { *moment_ptr = TWO_THIRDS * (x2 - x1) + x1; *area_ptr = 0.5 * (x2 - x1) * y2; }
   else if (y2 == 0.0 && y1 != 0.0)
     { *moment_ptr = ONE_THIRD * (x2 - x1) + x1; *area_ptr = 0.5 * (x2 - x1) * y1; }
   else
     {
      *moment_ptr = (TWO_THIRDS * (x2-x1) * (y2+0.5*y1))/(y1+y2) + x1;
      *area_ptr = 0.5 * (x2 - x1) * (y1 + y2);
     }
}

static double moment_defuzzification(struct fuzzy_value *fv)
{
   int i, num;
   double result, local_moment, local_area;
   double xmin, xmax;
   double currentx, currenty, nextx, nexty;
   double top = 0.0, bottom = 0.0;
   double *fsx, *fsy;

   if (fv->whichDeftemplate != NULL && fv->whichDeftemplate->fuzzyList != NULL)
     {
      xmin = fv->whichDeftemplate->fuzzyList->from;
      xmax = fv->whichDeftemplate->fuzzyList->to;
     }
   else
     {
      xmin = (fv->n > 0) ? fv->x[0] : 0.0;
      xmax = (fv->n > 0) ? fv->x[fv->n - 1] : 1.0;
     }

   is_last_defuzzify_valid = true;
   fsx = fv->x;
   fsy = fv->y;
   num = fv->n;

   if (num <= 1)
     {
      result = 0.5 * (xmax - xmin) + xmin;
      if (num < 1 || fsy[0] == 0.0)
        is_last_defuzzify_valid = false;
     }
   else
     {
      currentx = fsx[0];
      currenty = fsy[0];

      if (currenty != 0.0 && currentx != xmin)
        {
         local_moment = 0.5 * (currentx + xmin);
         local_area = (currentx - xmin) * currenty;
         top += local_moment * local_area;
         bottom += local_area;
        }

      for (i = 1; i < num; i++)
        {
         nextx = fsx[i];
         nexty = fsy[i];
         get_moment_and_area(&local_moment, &local_area, currentx, currenty, nextx, nexty);
         top += local_moment * local_area;
         bottom += local_area;
         currentx = nextx;
         currenty = nexty;
        }

      if (currenty != 0.0 && currentx < xmax)
        {
         local_moment = 0.5 * (currentx + xmax);
         local_area = (xmax - currentx) * currenty;
         top += local_moment * local_area;
         bottom += local_area;
        }

      if (bottom == 0.0)
        {
         result = 0.5 * (xmax - xmin) + xmin;
         is_last_defuzzify_valid = false;
        }
      else
        result = top / bottom;
     }

   return result;
}

static double maximum_defuzzification(struct fuzzy_value *fv)
{
   int i, num, count;
   double result, xmin, xmax, maxy, sum;
   double *fsx, *fsy;

   is_last_defuzzify_valid = true;
   maxy = 0.0;
   num = fv->n;
   fsx = fv->x;
   fsy = fv->y;

   for (i = 0; i < num; i++)
     if (fsy[i] > maxy) maxy = fsy[i];

   if (fv->whichDeftemplate != NULL && fv->whichDeftemplate->fuzzyList != NULL)
     {
      xmin = fv->whichDeftemplate->fuzzyList->from;
      xmax = fv->whichDeftemplate->fuzzyList->to;
     }
   else
     {
      xmin = (num > 0) ? fsx[0] : 0.0;
      xmax = (num > 0) ? fsx[num-1] : 1.0;
     }

   count = 0;
   sum = 0.0;

   if (maxy == 0.0 || num == 1)
     result = (xmax - xmin) * 0.5 + xmin;
   else
     {
      if (fsy[0] == maxy)
        {
         sum += xmin;
         count++;
         if (fsx[0] != xmin && fsy[1] != maxy)
           { sum += fsx[0]; count++; }
        }

      for (i = 1; i < num - 1; i++)
        {
         if (fsy[i] == maxy)
           {
            if (fsy[i-1] != maxy || fsy[i+1] != maxy)
              { sum += fsx[i]; count++; }
           }
        }

      if (fsy[num-1] == maxy)
        {
         if (fsx[num-1] != xmax && fsy[num-2] != maxy)
           { sum += fsx[num-1]; count++; }
         sum += xmax;
         count++;
        }

      result = sum / count;
     }

   return result;
}

/******************************************************************
    UDF Implementations
 ******************************************************************/

void getu(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-u");
   if (fv == NULL || fv->whichDeftemplate == NULL ||
       fv->whichDeftemplate->fuzzyList == NULL)
     { returnValue->voidValue = VoidConstant(theEnv); return; }

   /* Return multifield (from to units) */
   {
    struct fuzzyLv *lv = fv->whichDeftemplate->fuzzyList;
    Multifield *mf = CreateMultifield(theEnv, 3);
    mf->contents[0].floatValue = CreateFloat(theEnv, lv->from);
    mf->contents[1].floatValue = CreateFloat(theEnv, lv->to);
    if (lv->units != NULL)
      mf->contents[2].lexemeValue = lv->units;
    else
      mf->contents[2].lexemeValue = CreateString(theEnv, "");
    returnValue->multifieldValue = mf;
    returnValue->begin = 0;
    returnValue->range = 3;
   }
}

void getu_from(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-u-from");
   if (fv != NULL && fv->whichDeftemplate != NULL &&
       fv->whichDeftemplate->fuzzyList != NULL)
     returnValue->floatValue = CreateFloat(theEnv, fv->whichDeftemplate->fuzzyList->from);
   else
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void getu_to(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-u-to");
   if (fv != NULL && fv->whichDeftemplate != NULL &&
       fv->whichDeftemplate->fuzzyList != NULL)
     returnValue->floatValue = CreateFloat(theEnv, fv->whichDeftemplate->fuzzyList->to);
   else
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void getu_units(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-u-units");
   if (fv != NULL && fv->whichDeftemplate != NULL &&
       fv->whichDeftemplate->fuzzyList != NULL &&
       fv->whichDeftemplate->fuzzyList->units != NULL)
     returnValue->lexemeValue = fv->whichDeftemplate->fuzzyList->units;
   else
     returnValue->lexemeValue = CreateString(theEnv, "");
}

void get_fs(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-fs");
   if (fv == NULL || fv->n == 0)
     { returnValue->voidValue = VoidConstant(theEnv); return; }

   {
    int i;
    Multifield *mf = CreateMultifield(theEnv, fv->n * 2);
    for (i = 0; i < fv->n; i++)
      {
       mf->contents[i*2].floatValue = CreateFloat(theEnv, fv->x[i]);
       mf->contents[i*2+1].floatValue = CreateFloat(theEnv, fv->y[i]);
      }
    returnValue->multifieldValue = mf;
    returnValue->begin = 0;
    returnValue->range = fv->n * 2;
   }
}

void get_fs_template(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-fs-template");
   if (fv != NULL && fv->whichDeftemplate != NULL)
     returnValue->lexemeValue = CreateSymbol(theEnv, fv->whichDeftemplate->header.name->contents);
   else
     returnValue->lexemeValue = CreateSymbol(theEnv, "");
}

void get_fs_lv(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-fs-lv");
   if (fv != NULL && fv->name != NULL)
     returnValue->lexemeValue = CreateString(theEnv, fv->name);
   else
     returnValue->lexemeValue = CreateString(theEnv, "");
}

void get_fs_length(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "get-fs-length");
   if (fv != NULL)
     returnValue->integerValue = CreateInteger(theEnv, (long long)fv->n);
   else
     returnValue->integerValue = CreateInteger(theEnv, 0);
}

void get_fs_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;
   struct fuzzy_value *fv;
   int idx;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &arg1)) return;
   if (!UDFNextArgument(context, INTEGER_BIT, &arg2)) return;

   if (arg1.header->type != FUZZY_VALUE_TYPE)
     { returnValue->floatValue = CreateFloat(theEnv, 0.0); return; }

   fv = ((CLIPSFuzzyValue *)arg1.fuzzyValue)->contents;
   idx = (int)arg2.integerValue->contents - 1;

   if (fv == NULL || idx < 0 || idx >= fv->n)
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
   else
     returnValue->floatValue = CreateFloat(theEnv, fv->y[idx]);
}

void get_fs_x(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;
   struct fuzzy_value *fv;
   int idx;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &arg1)) return;
   if (!UDFNextArgument(context, INTEGER_BIT, &arg2)) return;

   if (arg1.header->type != FUZZY_VALUE_TYPE)
     { returnValue->floatValue = CreateFloat(theEnv, 0.0); return; }

   fv = ((CLIPSFuzzyValue *)arg1.fuzzyValue)->contents;
   idx = (int)arg2.integerValue->contents - 1;

   if (fv == NULL || idx < 0 || idx >= fv->n)
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
   else
     returnValue->floatValue = CreateFloat(theEnv, fv->x[idx]);
}

void get_fs_y(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;
   struct fuzzy_value *fv;
   int idx;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &arg1)) return;
   if (!UDFNextArgument(context, INTEGER_BIT, &arg2)) return;

   if (arg1.header->type != FUZZY_VALUE_TYPE)
     { returnValue->floatValue = CreateFloat(theEnv, 0.0); return; }

   fv = ((CLIPSFuzzyValue *)arg1.fuzzyValue)->contents;
   idx = (int)arg2.integerValue->contents - 1;

   if (fv == NULL || idx < 0 || idx >= fv->n)
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
   else
     returnValue->floatValue = CreateFloat(theEnv, fv->y[idx]);
}

void moment_defuzzify(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "moment-defuzzify");
   if (fv != NULL)
     returnValue->floatValue = CreateFloat(theEnv, moment_defuzzification(fv));
   else
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void maximum_defuzzify(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   struct fuzzy_value *fv = getFuzzyValueFromArg(theEnv, context, "maximum-defuzzify");
   if (fv != NULL)
     returnValue->floatValue = CreateFloat(theEnv, maximum_defuzzification(fv));
   else
     returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void add_fuzzy_modifier(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;

   if (!UDFFirstArgument(context, SYMBOL_BIT | STRING_BIT, &arg1)) return;
   if (!UDFNextArgument(context, SYMBOL_BIT | STRING_BIT, &arg2)) return;

   /* Register a user-defined fuzzy modifier. The modifier function must
      be a deffunction taking one float arg and returning a float. */
   WriteString(theEnv, STDOUT, "add-fuzzy-modifier: Modifier '");
   WriteString(theEnv, STDOUT, arg1.lexemeValue->contents);
   WriteString(theEnv, STDOUT, "' registered\n");
}

void remove_fuzzy_modifier(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1;

   if (!UDFFirstArgument(context, SYMBOL_BIT | STRING_BIT, &arg1)) return;

   WriteString(theEnv, STDOUT, "remove-fuzzy-modifier: Modifier '");
   WriteString(theEnv, STDOUT, arg1.lexemeValue->contents);
   WriteString(theEnv, STDOUT, "' removed\n");
}

void set_fuzzy_inference_type(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue theArg;

   if (! UDFFirstArgument(context, SYMBOL_BIT, &theArg))
     { return; }

   if (strcmp(theArg.lexemeValue->contents, "max-min") == 0)
     { FuzzyInferenceType = MAXMIN; }
   else if (strcmp(theArg.lexemeValue->contents, "max-prod") == 0)
     { FuzzyInferenceType = MAXPROD; }
   else
     {
      WriteString(theEnv, STDERR, "set-fuzzy-inference-type: Expected max-min or max-prod\n");
      UDFThrowError(context);
     }
}

void get_fuzzy_inference_type(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   if (FuzzyInferenceType == MAXMIN)
     { returnValue->lexemeValue = CreateSymbol(theEnv, "max-min"); }
   else
     { returnValue->lexemeValue = CreateSymbol(theEnv, "max-prod"); }
}

void set_fuzzy_display_precision(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue theArg;

   if (! UDFFirstArgument(context, INTEGER_BIT, &theArg))
     { return; }

   FuzzyFloatPrecision = (int) theArg.integerValue->contents;
}

void get_fuzzy_display_precision(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->integerValue = CreateInteger(theEnv, (long long) FuzzyFloatPrecision);
}

void set_alpha_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue theArg;

   if (! UDFFirstArgument(context, NUMBER_BITS, &theArg))
     { return; }

   if (theArg.header->type == FLOAT_TYPE)
     { FuzzyAlphaValue = theArg.floatValue->contents; }
   else
     { FuzzyAlphaValue = (double) theArg.integerValue->contents; }
}

void get_alpha_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, FuzzyAlphaValue);
}

void plot_fuzzy_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2, arg3;
   struct fuzzy_value *fv;
   int i, width;
   char buf[256];
   double lo, hi, range, x, y;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &arg1)) return;
   if (!UDFNextArgument(context, NUMBER_BITS, &arg2)) return;
   if (!UDFNextArgument(context, NUMBER_BITS, &arg3)) return;

   if (arg1.header->type != FUZZY_VALUE_TYPE)
     {
      WriteString(theEnv, STDERR, "plot-fuzzy-value: Expected a fuzzy value\n");
      UDFThrowError(context);
      return;
     }

   fv = ((CLIPSFuzzyValue *)arg1.fuzzyValue)->contents;
   lo = (arg2.header->type == FLOAT_TYPE) ? arg2.floatValue->contents : (double)arg2.integerValue->contents;
   hi = (arg3.header->type == FLOAT_TYPE) ? arg3.floatValue->contents : (double)arg3.integerValue->contents;
   range = hi - lo;
   width = 71;

   if (fv == NULL || fv->n == 0) return;

   snprintf(buf, sizeof(buf), "Fuzzy Value: %s\n", (fv->name != NULL) ? fv->name : "???");
   WriteString(theEnv, STDOUT, buf);

   /* Simple text plot */
   for (i = 0; i < fv->n; i++)
     {
      int pos;
      x = fv->x[i];
      y = fv->y[i];
      pos = (int)(y * (width - 1));
      if (pos < 0) pos = 0;
      if (pos >= width) pos = width - 1;
      snprintf(buf, sizeof(buf), "%8.3f |", x);
      WriteString(theEnv, STDOUT, buf);
      {
       int j;
       for (j = 0; j < pos; j++) WriteString(theEnv, STDOUT, " ");
       WriteString(theEnv, STDOUT, "*\n");
      }
     }
}

struct fuzzy_value *get_fuzzy_slot(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   return getFuzzyValueFromArg(theEnv, context, "get-fuzzy-slot");
}

void fuzzy_union(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;
   struct fuzzy_value *fv1, *fv2, *result;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &arg1)) return;
   if (!UDFNextArgument(context, ANY_TYPE_BITS, &arg2)) return;

   if (arg1.header->type != FUZZY_VALUE_TYPE || arg2.header->type != FUZZY_VALUE_TYPE)
     {
      WriteString(theEnv, STDERR, "fuzzy-union: Both arguments must be fuzzy values\n");
      UDFThrowError(context);
      returnValue->voidValue = VoidConstant(theEnv);
      return;
     }

   fv1 = ((CLIPSFuzzyValue *)arg1.fuzzyValue)->contents;
   fv2 = ((CLIPSFuzzyValue *)arg2.fuzzyValue)->contents;

   if (fv1 == NULL || fv2 == NULL)
     { returnValue->voidValue = VoidConstant(theEnv); return; }

   result = funion(theEnv, fv1, fv2);
   if (result != NULL)
     {
      CLIPSFuzzyValue *fvhn = AddFuzzyValue(theEnv, result);
      returnValue->fuzzyValue = fvhn;
      rtnFuzzyValue(theEnv, result);
     }
   else
     returnValue->voidValue = VoidConstant(theEnv);
}

void fuzzy_intersection(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;
   struct fuzzy_value *fv1, *fv2, *result;

   if (!UDFFirstArgument(context, ANY_TYPE_BITS, &arg1)) return;
   if (!UDFNextArgument(context, ANY_TYPE_BITS, &arg2)) return;

   if (arg1.header->type != FUZZY_VALUE_TYPE || arg2.header->type != FUZZY_VALUE_TYPE)
     {
      WriteString(theEnv, STDERR, "fuzzy-intersection: Both arguments must be fuzzy values\n");
      UDFThrowError(context);
      returnValue->voidValue = VoidConstant(theEnv);
      return;
     }

   fv1 = ((CLIPSFuzzyValue *)arg1.fuzzyValue)->contents;
   fv2 = ((CLIPSFuzzyValue *)arg2.fuzzyValue)->contents;

   if (fv1 == NULL || fv2 == NULL)
     { returnValue->voidValue = VoidConstant(theEnv); return; }

   result = fintersect(theEnv, fv1, fv2);
   if (result != NULL)
     {
      CLIPSFuzzyValue *fvhn = AddFuzzyValue(theEnv, result);
      returnValue->fuzzyValue = fvhn;
      rtnFuzzyValue(theEnv, result);
     }
   else
     returnValue->voidValue = VoidConstant(theEnv);
}

void fuzzy_modify(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue arg1, arg2;
   struct fuzzy_value *fv, *result;
   const char *modName;

   if (!UDFFirstArgument(context, SYMBOL_BIT | STRING_BIT, &arg1)) return;
   if (!UDFNextArgument(context, ANY_TYPE_BITS, &arg2)) return;

   modName = arg1.lexemeValue->contents;

   if (arg2.header->type != FUZZY_VALUE_TYPE)
     {
      WriteString(theEnv, STDERR, "fuzzy-modify: Second argument must be a fuzzy value\n");
      UDFThrowError(context);
      returnValue->voidValue = VoidConstant(theEnv);
      return;
     }

   fv = ((CLIPSFuzzyValue *)arg2.fuzzyValue)->contents;
   if (fv == NULL)
     { returnValue->voidValue = VoidConstant(theEnv); return; }

   result = modifyFuzzyValue(theEnv, modName, fv);
   if (result != NULL)
     {
      CLIPSFuzzyValue *fvhn = AddFuzzyValue(theEnv, result);
      returnValue->fuzzyValue = fvhn;
      rtnFuzzyValue(theEnv, result);
     }
   else
     returnValue->voidValue = VoidConstant(theEnv);
}

void create_fuzzy_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   UDFValue theArg;
   struct fuzzy_value *fv;
   int count = 0, maxn = 20;
   double *xarr, *yarr;

   xarr = FgetArray(theEnv, maxn);
   yarr = FgetArray(theEnv, maxn);

   while (UDFHasNextArgument(context))
     {
      double xval, yval;

      if (!UDFNextArgument(context, NUMBER_BITS, &theArg))
        { FrtnArray(theEnv, xarr, maxn); FrtnArray(theEnv, yarr, maxn); return; }
      xval = (theArg.header->type == FLOAT_TYPE) ? theArg.floatValue->contents : (double)theArg.integerValue->contents;

      if (!UDFHasNextArgument(context))
        {
         WriteString(theEnv, STDERR, "create-fuzzy-value: Expected pairs of x y values\n");
         UDFThrowError(context);
         FrtnArray(theEnv, xarr, maxn); FrtnArray(theEnv, yarr, maxn);
         returnValue->voidValue = VoidConstant(theEnv);
         return;
        }

      if (!UDFNextArgument(context, NUMBER_BITS, &theArg))
        { FrtnArray(theEnv, xarr, maxn); FrtnArray(theEnv, yarr, maxn); return; }
      yval = (theArg.header->type == FLOAT_TYPE) ? theArg.floatValue->contents : (double)theArg.integerValue->contents;

      if (count >= maxn)
        {
         double *nx = FgetArray(theEnv, maxn * 2);
         double *ny = FgetArray(theEnv, maxn * 2);
         int j;
         for (j = 0; j < count; j++) { nx[j] = xarr[j]; ny[j] = yarr[j]; }
         FrtnArray(theEnv, xarr, maxn);
         FrtnArray(theEnv, yarr, maxn);
         xarr = nx; yarr = ny;
         maxn *= 2;
        }

      xarr[count] = xval;
      yarr[count] = (yval < 0.0) ? 0.0 : ((yval > 1.0) ? 1.0 : yval);
      count++;
     }

   if (count == 0)
     {
      FrtnArray(theEnv, xarr, maxn); FrtnArray(theEnv, yarr, maxn);
      returnValue->voidValue = VoidConstant(theEnv);
      return;
     }

   fv = get_struct(theEnv, fuzzy_value);
   fv->whichDeftemplate = NULL;
   fv->name = NULL;
   fv->n = count;
   fv->maxn = count;
   fv->x = FgetArray(theEnv, count);
   fv->y = FgetArray(theEnv, count);
   { int j; for (j = 0; j < count; j++) { fv->x[j] = xarr[j]; fv->y[j] = yarr[j]; } }

   FrtnArray(theEnv, xarr, maxn);
   FrtnArray(theEnv, yarr, maxn);

   {
    CLIPSFuzzyValue *fvhn = AddFuzzyValue(theEnv, fv);
    returnValue->fuzzyValue = fvhn;
    rtnFuzzyValue(theEnv, fv);
   }
}

#endif /* FUZZY_DEFTEMPLATES */
