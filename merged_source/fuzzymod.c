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
   /* The default modifiers: very, somewhat, more-or-less, etc.
      In original FuzzyCLIPS these were set up with internal
      modifier functions. Here we initialize the basic set.
   */
   modifierList = NULL;

   /* TODO: Port full modifier initialization from original FuzzyCLIPS
      Default modifiers include:
       - "not"        -> complement operation
       - "very"       -> concentration (square the membership values)
       - "somewhat"   -> dilation (sqrt of membership values)
       - "more-or-less" -> same as somewhat
       - "slightly"   -> intensify & complement operations
       - "extremely"  -> cube of membership values
       - "above"      -> above a certain point
       - "below"      -> below a certain point
       - "intensify"  -> intensification
       - "norm"       -> normalize
       - "plus"       -> slightly concentrate
       - "minus"      -> slightly dilate
   */
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
      if (newFv != NULL) fcompliment(newFv);
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
