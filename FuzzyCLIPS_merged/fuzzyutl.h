   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY UTILITIES HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Fuzzy set operations (union, intersection,       */
/*   complement), fuzzy consequence computation, and         */
/*   printing of fuzzy values/sets.                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzyutl

#pragma once

#define _H_fuzzyutl

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzyval.h"
#include "factmngr.h"

   void                           fcompliment(Environment *,struct fuzzy_value *fv);
   struct fuzzy_value            *funion(Environment *,struct fuzzy_value *f1,
                                        struct fuzzy_value *f2);
   int                            nonintersectiontest(double *Ax,double *Ay,
                                                      double *Bx,double *By,
                                                      int Asize,int Bsize);
   void                           computeFuzzyConsequence(Environment *,Fact *new_fact);
   void                           changeValueOfFuzzySlots(Environment *,Fact *fact1,
                                                          Fact *fact2);
   void                           PrintFuzzyTemplateFact(Environment *,const char *logName,
                                                         struct fuzzy_value *fv
#if CERTAINTY_FACTORS
                                                         ,double CF
#endif
                                                        );
   void                           PrintFuzzySet(Environment *,const char *logName,
                                                struct fuzzy_value *fv);
   double                         maxmin_intersect(Environment *,struct fuzzy_value *f1,
                                                   struct fuzzy_value *f2,
                                                   int DoIntersect,
                                                   struct fuzzy_value **intersectSet);
   struct fuzzy_value            *fintersect(Environment *,struct fuzzy_value *f1,
                                            struct fuzzy_value *f2);
   double                         max_of_min(Environment *,struct fuzzy_value *f1,
                                             struct fuzzy_value *f2);
   int                            FZ_EQUAL(double f1,double f2);

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzyutl */
