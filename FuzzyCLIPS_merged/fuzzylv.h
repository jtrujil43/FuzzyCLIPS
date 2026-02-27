   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*      LINGUISTIC VARIABLE STRUCTURE HEADER FILE      */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines the fuzzyLv (linguistic variable)        */
/*   structure and primary_term structure for fuzzy          */
/*   deftemplates.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             CLIPSLexeme replaces symbolHashNode.           */
/*             CLIPSFuzzyValue replaces FUZZY_VALUE_HN.      */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzylv

#pragma once

#define _H_fuzzylv

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#include "extnfunc.h"
#include "fuzzyval.h"

struct primary_term;

/******************************************************************/
/* LINGUISTIC VARIABLE STRUCTURE:                                 */
/******************************************************************/
struct fuzzyLv
  {
   /* Universe of Discourse */
   double from;
   double to;
   CLIPSLexeme *units;
   /* Primary Terms allowed */
   struct primary_term *primary_term_list;
  };

/******************************************************************/
/* PRIMARY_TERM STRUCTURE:                                        */
/******************************************************************/
struct primary_term
  {
   struct CLIPSFuzzyValue *fuzzy_value_description;
   struct primary_term *next;
  };

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzylv */
