   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY DEFINITIONS HEADER FILE              */
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
/*             Removed globle/LOCALE macros.                  */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzydef

#pragma once

#define _H_fuzzydef

#include "setup.h"

#if FUZZY_DEFTEMPLATES

   void                           InitializeFuzzy(Environment *);

   /* Fuzzy global state - stored per-environment in future */
   extern int            FuzzyInferenceType;
   extern int            FuzzyFloatPrecision;
   extern double         FuzzyAlphaValue;

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzydef */
