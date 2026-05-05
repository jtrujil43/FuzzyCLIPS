   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*            FUZZY MODIFIER MODULE                    */
   /*******************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzymod.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzyrhs.h"
#include "fuzzyutl.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "symbol.h"
#include "pprint.h"

/******************************************************************
    Global modifier list
 ******************************************************************/

static struct modifierListItem *modifierList = NULL;

/******************************************************************
    initFuzzyModifierList - set up default modifiers
 ******************************************************************/

void initFuzzyModifierList(
  Environment *theEnv)
{
   /* Initialize the built-in fuzzy modifiers.
      Each modifier has a name and an associated function that operates
      on fuzzy_value y-arrays. The modifyFuzzyValue() function in this
      file handles dispatch to built-in modifiers by name. The modifier
      list is used for user-defined and some built-in modifiers that
      need explicit registration.
   */
   modifierList = NULL;

   /* Register built-in modifiers */
   AddFuzzyModifier(theEnv, "not", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "very", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "somewhat", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "more-or-less", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "extremely", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "intensify", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "slightly", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "norm", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "plus", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
   AddFuzzyModifier(theEnv, "minus", NULL, NULL
#if DEFFUNCTION_CONSTRUCT
     , NULL
#endif
     );
}

/******************************************************************
    lookupModifier - find a modifier in the modifier list
 ******************************************************************/

struct modifierListItem *lookupModifier(
  Environment *theEnv,
  const char *modName)
{
   struct modifierListItem *item;

   item = modifierList;
   while (item != NULL)
     {
      if (strcmp(item->name, modName) == 0)
        return item;
      item = item->next;
     }

   return NULL;
}

/******************************************************************
    getModifierList - return the modifier list head
 ******************************************************************/

struct modifierListItem *getModifierList(
  Environment *theEnv)
{
   return modifierList;
}

/******************************************************************
    setModifierList - set the modifier list head
 ******************************************************************/

void setModifierList(
  Environment *theEnv,
  struct modifierListItem *list)
{
   modifierList = list;
}

/******************************************************************
    FindModifier - find a modifier by name (public API)
 ******************************************************************/

struct modifierListItem *FindModifier(
  Environment *theEnv,
  const char *mod_name)
{
   return lookupModifier(theEnv, mod_name);
}

/******************************************************************
    AddFuzzyModifier - add a modifier to the list
 ******************************************************************/

int AddFuzzyModifier(
  Environment *theEnv,
  const char *name,
  void (*modfunc)(Environment *, struct fuzzy_value *),
  struct functionDefinition *modClipsfunc
#if DEFFUNCTION_CONSTRUCT
  ,Deffunction *modDeffunc
#endif
  )
{
   struct modifierListItem *item;

   /* Check if already exists */
   if (lookupModifier(theEnv, name) != NULL)
     return 0;

   item = (struct modifierListItem *) genalloc(theEnv,
            sizeof(struct modifierListItem));
   item->name = (char *) genalloc(theEnv, strlen(name) + 1);
   strcpy(item->name, name);
   item->modfunc = modfunc;
   item->modClipsfunc = modClipsfunc;
#if DEFFUNCTION_CONSTRUCT
   item->modDeffunc = modDeffunc;
#endif
   item->next = modifierList;
   modifierList = item;
   return 1;
}

/******************************************************************
    RemoveFuzzyModifier - remove a modifier from the list
 ******************************************************************/

void RemoveFuzzyModifier(
  Environment *theEnv,
  const char *name)
{
   struct modifierListItem *item, *prev;

   prev = NULL;
   item = modifierList;
   while (item != NULL)
     {
      if (strcmp(item->name, name) == 0)
        {
         if (prev == NULL)
           modifierList = item->next;
         else
           prev->next = item->next;
         genfree(theEnv, item->name, strlen(item->name) + 1);
         genfree(theEnv, item, sizeof(struct modifierListItem));
         return;
        }
      prev = item;
      item = item->next;
     }
}

/******************************************************************
    executeModifyFunction - apply a modifier's function to a fuzzy value
 ******************************************************************/

void executeModifyFunction(
  Environment *theEnv,
  struct fuzzy_value *fv,
  struct modifierListItem *mptr)
{
   if (mptr == NULL || fv == NULL) return;

   /* If there's a direct C function pointer, use it */
   if (mptr->modfunc != NULL)
     {
      mptr->modfunc(theEnv, fv);
      return;
     }

   /* Otherwise use the built-in name-based dispatch */
   /* Apply the modifier in-place by modifying y values */
   if (strcmp(mptr->name, "not") == 0)
     { fcompliment(theEnv, fv); }
   else if (strcmp(mptr->name, "very") == 0)
     { int i; for (i = 0; i < fv->n; i++) fv->y[i] = fv->y[i] * fv->y[i]; }
   else if (strcmp(mptr->name, "somewhat") == 0 ||
            strcmp(mptr->name, "more-or-less") == 0)
     { int i; for (i = 0; i < fv->n; i++) fv->y[i] = sqrt(fv->y[i]); }
   else if (strcmp(mptr->name, "extremely") == 0)
     { int i; for (i = 0; i < fv->n; i++) fv->y[i] = fv->y[i] * fv->y[i] * fv->y[i]; }
   else if (strcmp(mptr->name, "intensify") == 0)
     {
      int i; double y;
      for (i = 0; i < fv->n; i++)
        {
         y = fv->y[i];
         if (y <= 0.5) fv->y[i] = 2.0 * y * y;
         else fv->y[i] = 1.0 - 2.0 * (1.0 - y) * (1.0 - y);
        }
     }
   else if (strcmp(mptr->name, "slightly") == 0)
     {
      int i; double y;
      /* slightly = intensify(complement(x)) */
      for (i = 0; i < fv->n; i++) fv->y[i] = 1.0 - fv->y[i];
      for (i = 0; i < fv->n; i++)
        {
         y = fv->y[i];
         if (y <= 0.5) fv->y[i] = 2.0 * y * y;
         else fv->y[i] = 1.0 - 2.0 * (1.0 - y) * (1.0 - y);
        }
     }
   else if (strcmp(mptr->name, "norm") == 0)
     {
      int i; double maxY = 0.0;
      for (i = 0; i < fv->n; i++)
        if (fv->y[i] > maxY) maxY = fv->y[i];
      if (maxY > 0.0)
        for (i = 0; i < fv->n; i++) fv->y[i] = fv->y[i] / maxY;
     }
   else if (strcmp(mptr->name, "plus") == 0)
     { int i; for (i = 0; i < fv->n; i++) fv->y[i] = pow(fv->y[i], 1.25); }
   else if (strcmp(mptr->name, "minus") == 0)
     { int i; for (i = 0; i < fv->n; i++) fv->y[i] = pow(fv->y[i], 0.75); }
}

/******************************************************************
    concentrateFuzzyValue - apply "very" hedge
    (square the membership values)
 ******************************************************************/

struct fuzzy_value *concentrateFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   struct fuzzy_value *newFv;
   int i;

   newFv = CopyFuzzyValue(theEnv, fv);
   if (newFv == NULL) return NULL;

   for (i = 0; i < newFv->n; i++)
     { newFv->y[i] = newFv->y[i] * newFv->y[i]; }

   return newFv;
}

/******************************************************************
    dilateFuzzyValue - apply "somewhat" hedge
    (sqrt the membership values)
 ******************************************************************/

struct fuzzy_value *dilateFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   struct fuzzy_value *newFv;
   int i;

   newFv = CopyFuzzyValue(theEnv, fv);
   if (newFv == NULL) return NULL;

   for (i = 0; i < newFv->n; i++)
     { newFv->y[i] = sqrt(newFv->y[i]); }

   return newFv;
}

/******************************************************************
    intensifyFuzzyValue - apply intensification
    if y <= 0.5: y = 2*y*y; else: y = 1 - 2*(1-y)*(1-y)
 ******************************************************************/

struct fuzzy_value *intensifyFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   struct fuzzy_value *newFv;
   int i;
   double y;

   newFv = CopyFuzzyValue(theEnv, fv);
   if (newFv == NULL) return NULL;

   for (i = 0; i < newFv->n; i++)
     {
      y = newFv->y[i];
      if (y <= 0.5)
        newFv->y[i] = 2.0 * y * y;
      else
        newFv->y[i] = 1.0 - 2.0 * (1.0 - y) * (1.0 - y);
     }

   return newFv;
}

/******************************************************************
    modifyFuzzyValue - apply a modifier to a fuzzy value
 ******************************************************************/

struct fuzzy_value *modifyFuzzyValue(
  Environment *theEnv,
  const char *modifierName,
  struct fuzzy_value *fv)
{
   if (strcmp(modifierName, "not") == 0)
     {
      struct fuzzy_value *newFv = CopyFuzzyValue(theEnv, fv);
      if (newFv != NULL) fcompliment(theEnv, newFv);
      return newFv;
     }
   else if (strcmp(modifierName, "very") == 0)
     { return concentrateFuzzyValue(theEnv, fv); }
   else if (strcmp(modifierName, "somewhat") == 0 ||
            strcmp(modifierName, "more-or-less") == 0)
     { return dilateFuzzyValue(theEnv, fv); }
   else if (strcmp(modifierName, "extremely") == 0)
     {
      struct fuzzy_value *newFv = CopyFuzzyValue(theEnv, fv);
      int i;
      if (newFv != NULL)
        {
         for (i = 0; i < newFv->n; i++)
           { newFv->y[i] = newFv->y[i] * newFv->y[i] * newFv->y[i]; }
        }
      return newFv;
     }
   else if (strcmp(modifierName, "intensify") == 0)
     { return intensifyFuzzyValue(theEnv, fv); }

   /* If no built-in match, check user-defined modifiers */
   /* TODO: look up modifier in modifier list and apply */
   WriteString(theEnv, STDERR, "Unknown fuzzy modifier: ");
   WriteString(theEnv, STDERR, modifierName);
   WriteString(theEnv, STDERR, "\n");
   return NULL;
}

#endif /* FUZZY_DEFTEMPLATES */
