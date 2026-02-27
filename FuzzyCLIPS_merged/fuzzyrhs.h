   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY RHS PARSER HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parsing of fuzzy fact assertions (RHS), fuzzy    */
/*   value construction, linguistic expression parsing,      */
/*   and array memory management for fuzzy values.           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzyrhs

#pragma once

#define _H_fuzzyrhs

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzymod.h"
#include "scanner.h"
#include "expressn.h"
#include "tmpltdef.h"

   Expression                    *ParseAssertFuzzyFact(Environment *,
                                                       const char *readSource,
                                                       struct token *tempToken,
                                                       int *error,int endType,
                                                       int constantsOnly,
                                                       Deftemplate *theDeftemplate,
                                                       int variablesAllowed);
   struct fuzzy_value            *ParseLinguisticExpr(Environment *,
                                                      const char *readSource,
                                                      struct token *tempToken,
                                                      struct fuzzyLv *lvp,
                                                      int *error);
   struct fuzzy_value            *CopyFuzzyValue(Environment *,struct fuzzy_value *fv);
   void                          CompactFuzzyValue(Environment *,struct fuzzy_value *fv);
   struct fuzzy_value            *getConstantFuzzyValue(Environment *,Expression *top,
                                                        int *error);
   void                          ModifyFuzzyValue(Environment *,
                                                  struct modifierListItem *mptr,
                                                  struct fuzzy_value *elem);
   double                        *FgetArray(Environment *,int length);
   void                          FrtnArray(Environment *,double *p,int length);
   int                           *IgetArray(Environment *,int length);
   void                          IrtnArray(Environment *,int *p,int length);
   Expression                    *tokenToFloatExpression(Environment *,
                                                        const char *readSource,
                                                        struct token *tempToken,
                                                        int *error,int constantsOnly);

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzyrhs */
