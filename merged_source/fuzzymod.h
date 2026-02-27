   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*             FUZZY MODIFIER HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Management of fuzzy modifiers (hedges) such as   */
/*   "very", "not", "more-or-less", "somewhat", etc.         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             All functions now take Environment *.          */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzymod

#pragma once

#define _H_fuzzymod

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzyval.h"
#include "dffnxfun.h"
#include "extnfunc.h"

/* structures for the list of modifier functions */

struct modifierListItem
  {
   char *name;   /* name of the modifier */
   void (*modfunc)(Environment *,struct fuzzy_value *fv);
   struct functionDefinition *modClipsfunc;
#if DEFFUNCTION_CONSTRUCT
   Deffunction *modDeffunc;
#endif
   struct modifierListItem *next;
  };

   void                          initFuzzyModifierList(Environment *);
   void                          executeModifyFunction(Environment *,struct fuzzy_value *,
                                                       struct modifierListItem *);
   int                           AddFuzzyModifier(Environment *,const char *,
                                                  void (*)(Environment *,struct fuzzy_value *),
                                                  struct functionDefinition *
#if DEFFUNCTION_CONSTRUCT
                                                  ,Deffunction *
#endif
                                                  );
   void                          RemoveFuzzyModifier(Environment *,const char *);
   struct modifierListItem       *FindModifier(Environment *,const char *mod_name);

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzymod */
