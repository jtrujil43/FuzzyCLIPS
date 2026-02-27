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
    Stub implementations for all UDF functions.
    These are placeholders that need the full logic
    ported from the original FuzzyCLIPS fuzzycom.c
 ******************************************************************/

void getu(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
   returnValue->voidValue = VoidConstant(theEnv);
}

void getu_from(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void getu_to(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void getu_units(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->lexemeValue = CreateString(theEnv, "");
}

void get_fs(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->voidValue = VoidConstant(theEnv);
}

void get_fs_template(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->lexemeValue = CreateSymbol(theEnv, "");
}

void get_fs_lv(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->lexemeValue = CreateString(theEnv, "");
}

void get_fs_length(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->integerValue = CreateInteger(theEnv, 0);
}

void get_fs_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void get_fs_x(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void get_fs_y(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void moment_defuzzify(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port moment defuzzification from original FuzzyCLIPS */
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void maximum_defuzzify(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port maximum defuzzification from original FuzzyCLIPS */
   returnValue->floatValue = CreateFloat(theEnv, 0.0);
}

void add_fuzzy_modifier(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
}

void remove_fuzzy_modifier(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
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
   /* TODO: Port plot function from original FuzzyCLIPS */
   WriteString(theEnv, STDOUT, "plot-fuzzy-value: Not yet fully implemented\n");
}

struct fuzzy_value *get_fuzzy_slot(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
   return NULL;
}

void fuzzy_union(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
   returnValue->voidValue = VoidConstant(theEnv);

}

void fuzzy_intersection(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
   returnValue->voidValue = VoidConstant(theEnv);

}

void fuzzy_modify(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
   returnValue->voidValue = VoidConstant(theEnv);

}

void create_fuzzy_value(Environment *theEnv, UDFContext *context, UDFValue *returnValue)
{
   /* TODO: Port from original FuzzyCLIPS */
   returnValue->voidValue = VoidConstant(theEnv);

}

#endif /* FUZZY_DEFTEMPLATES */
