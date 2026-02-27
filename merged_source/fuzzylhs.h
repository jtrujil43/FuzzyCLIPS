   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY LHS PATTERN HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses fuzzy patterns on the LHS of rules.       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzylhs

#pragma once

#define _H_fuzzylhs

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzyval.h"
#include "fuzzylv.h"
#include "scanner.h"

   struct fuzzy_value            *GetFuzzyLHSPattern(Environment *,
                                                     const char *readSource,
                                                     struct token *theToken,
                                                     struct fuzzyLv *fzTemplate,
                                                     int *error);

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzylhs */
