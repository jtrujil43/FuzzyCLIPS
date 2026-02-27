   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY TEMPLATE PARSER HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parsing of fuzzy deftemplate definitions,        */
/*   S/Z/PI functions, and fuzzy value install/deinstall.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzypsr

#pragma once

#define _H_fuzzypsr

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzylv.h"
#include "fuzzyval.h"
#include "scanner.h"
#include "tmpltdef.h"

   struct fuzzyLv                *ParseFuzzyTemplate(Environment *,const char *readSource,
                                                     struct token *inputToken,
                                                     int *DeftemplateError);
   void                          RtnFuzzyTemplate(Environment *,struct fuzzyLv *lv);
   void                          rtnFuzzyValue(Environment *,struct fuzzy_value *fv);
   void                          InstallFuzzyValue(Environment *,void *fv);
   void                          DeinstallFuzzyValue(Environment *,void *fv);
   void                          InstallFuzzyTemplate(Environment *,Deftemplate *theDeftemplate);
   void                          DeinstallFuzzyTemplate(Environment *,struct fuzzyLv *fzTemplate);
   double                        sFunction(double x,double alfa,double beta,double gamma);
   void                          Init_S_Z_PI_yvalues(Environment *);
   struct fuzzy_value            *Get_S_Z_or_PI_FuzzyValue(Environment *,double alfa,
                                                           double beta,double gamma,
                                                           int function_type);

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzypsr */
