   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          CERTAINTY FACTOR HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Certainty factor support for facts and rules.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             All functions now take Environment *.          */
/*                                                           */
/*************************************************************/

#ifndef _H_cfdef

#pragma once

#define _H_cfdef

#include "setup.h"

#if CERTAINTY_FACTORS

#include "factmngr.h"
#include "expressn.h"
#include "match.h"
#include "ruledef.h"

#if FUZZY_DEFTEMPLATES
#include "fuzzyval.h"
#endif

   double                         possibility(Environment *,struct fuzzy_value *f1,
                                              struct fuzzy_value *f2);
   double                         necessity(Environment *,struct fuzzy_value *f1,
                                            struct fuzzy_value *f2);
   double                         similarity(Environment *,struct fuzzy_value *f1,
                                             struct fuzzy_value *f2);
   void                           InitializeCF(Environment *);
   Expression                    *ParseDeclareUncertainty(Environment *,
                                                          const char *readSource,
                                                          const char *ruleName,
                                                          int *error,
                                                          double *cfVALUE);
   double                         computeStdConclCF(Environment *,double theRuleCF,
                                                    struct partialMatch *binds);
#if FUZZY_DEFTEMPLATES
   double                         computeFuzzyCrispConclCF(Environment *,
                                                           Defrule *theRule,
                                                           struct partialMatch *binds);
#endif
   void                           changeCFofNewFact(Environment *,Fact *newfact);
   void                           changeCFofExistingFact(Environment *,Fact *fact1,
                                                         Fact *fact2);
   void                           changeCFofNewVsExistingFact(Environment *,Fact *fact1,
                                                              Fact *fact2);
   void                           cfInformationError(Environment *,const char *);
   void                           cfRangeError(Environment *);
   void                           cfNonNumberError(Environment *);
   void                           printCF(Environment *,const char *logicalName,double cf);
   void                           getcf(Environment *,UDFContext *,UDFValue *);
   void                           set_threshold(Environment *,UDFContext *,UDFValue *);
   void                           unthreshold(Environment *,UDFContext *,UDFValue *);
   void                           get_threshold(Environment *,UDFContext *,UDFValue *);
   void                           enable_rule_cf_calculation(Environment *,UDFContext *,UDFValue *);
   void                           disable_rule_cf_calculation(Environment *,UDFContext *,UDFValue *);

   extern double                  Threshold_CF;

#endif /* CERTAINTY_FACTORS */

#endif /* _H_cfdef */
