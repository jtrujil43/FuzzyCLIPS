   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*            FUZZY DEFINITIONS MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Initialization of fuzzy reasoning system.        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             All functions now take Environment *.          */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzydef.h"
#include "fuzzycom.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzypsr.h"
#include "fuzzymod.h"
#include "constant.h"

#include <stdio.h>

/******************************************************************
    Global Internal Variable Declarations
 ******************************************************************/

   int      FuzzyInferenceType;
   int      FuzzyFloatPrecision;
   double   FuzzyAlphaValue;

/******************************************************************
    FUNCTIONS THAT INITIALIZE FUZZY LOGIC CONSTRUCT
 ******************************************************************/

void InitializeFuzzy(
  Environment *theEnv)
{
  DeffuzzyCommands(theEnv);       /* in fuzzycom.c */
  Init_S_Z_PI_yvalues(theEnv);    /* in fuzzypsr.c */
  initFuzzyModifierList(theEnv);  /* in fuzzymod.c */

  FuzzyInferenceType = MAXMIN;  /* inference type 'max-min' by default */
  FuzzyFloatPrecision = 4;      /* default precision for printing fuzzy set values */
  FuzzyAlphaValue = 0.0;        /* default alpha cut for pattern matching */
}

#endif /* FUZZY_DEFTEMPLATES */
