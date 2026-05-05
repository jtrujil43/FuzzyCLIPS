   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY TEMPLATE PARSER MODULE               */
   /*******************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzypsr.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzyrhs.h"
#include "fuzzyutl.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"
#include "pprint.h"

/******************************************************************
    Global arrays for S, Z, PI function y values
 ******************************************************************/

#define ArraySIZE 101

static double S_array[ArraySIZE];
static double Z_array[ArraySIZE];
static double PI_array[ArraySIZE];

/******************************************************************
    sFunction - compute the S-function value
 ******************************************************************/

double sFunction(
  double x,
  double alfa,
  double beta,
  double gamma)
{
   double denom;

   if (x <= alfa) return 0.0;
   if (x >= gamma) return 1.0;
   if (x <= beta)
     {
      denom = (gamma - alfa);
      if (denom == 0.0) return 0.0;
      return 2.0 * ((x - alfa) / denom) * ((x - alfa) / denom);
     }
   else
     {
      denom = (gamma - alfa);
      if (denom == 0.0) return 1.0;
      return 1.0 - 2.0 * ((x - gamma) / denom) * ((x - gamma) / denom);
     }
}

/******************************************************************
    Init_S_Z_PI_yvalues - precompute S, Z, PI arrays
 ******************************************************************/

void Init_S_Z_PI_yvalues(
  Environment *theEnv)
{
   int i;
   double x;

   for (i = 0; i < ArraySIZE; i++)
     {
      x = (double)i / (double)(ArraySIZE - 1);
      S_array[i] = sFunction(x, 0.0, 0.5, 1.0);
      Z_array[i] = 1.0 - S_array[i];
     }

   for (i = 0; i < ArraySIZE; i++)
     {
      x = (double)i / (double)(ArraySIZE - 1);
      if (x <= 0.5)
        PI_array[i] = sFunction(x * 2.0, 0.0, 0.5, 1.0);
      else
        PI_array[i] = 1.0 - sFunction((x - 0.5) * 2.0, 0.0, 0.5, 1.0);
     }
}

/******************************************************************
    Get_S_Z_or_PI_FuzzyValue
 ******************************************************************/

struct fuzzy_value *Get_S_Z_or_PI_FuzzyValue(
  Environment *theEnv,
  double alfa,
  double beta,
  double gamma,
  int function_type)
{
   struct fuzzy_value *fv;
   int i;
   double range, x;

   fv = get_struct(theEnv, fuzzy_value);
   fv->whichDeftemplate = NULL;
   fv->name = NULL;
   fv->n = ArraySIZE;
   fv->maxn = ArraySIZE;
   fv->x = FgetArray(theEnv, ArraySIZE);
   fv->y = FgetArray(theEnv, ArraySIZE);

   range = gamma - alfa;

   for (i = 0; i < ArraySIZE; i++)
     {
      x = alfa + range * ((double)i / (double)(ArraySIZE - 1));
      fv->x[i] = x;
      switch (function_type)
        {
         case S_FUNCTION:
            fv->y[i] = S_array[i];
            break;
         case Z_FUNCTION:
            fv->y[i] = Z_array[i];
            break;
         case PI_FUNCTION:
            fv->y[i] = PI_array[i];
            break;
         default:
            fv->y[i] = 0.0;
            break;
        }
     }

   return fv;
}

/******************************************************************
    Static helper function declarations for the parser
 ******************************************************************/

static struct fuzzyLv       *parseUniverse(Environment *theEnv, const char *readSource,
                                           struct token *inputToken, int *DeftemplateError);
static void                  parsePrimaryTermList(Environment *theEnv, const char *readSource,
                                           struct token *inputToken, int *DeftemplateError,
                                           struct fuzzyLv *new_lv);
static struct primary_term  *parsePrimaryTerm(Environment *theEnv, const char *readSource,
                                           struct token *inputToken, int *DeftemplateError,
                                           struct fuzzyLv *new_lv);
static struct fuzzy_value   *parseTemplateFuzzyValue(Environment *theEnv, const char *readSource,
                                           struct token *inputToken, int *DeftemplateError,
                                           struct fuzzyLv *new_lv);
static struct fuzzy_value   *parseSingletonFuzzyValue(Environment *theEnv, const char *readSource,
                                           struct token *inputToken, int *DeftemplateError,
                                           struct fuzzyLv *u);
static struct fuzzy_value   *parseStandardFuzzyValue(Environment *theEnv, const char *readSource,
                                           struct token *inputToken, int *DeftemplateError,
                                           struct fuzzyLv *u);
static void                  rtnPrimaryTermList(Environment *theEnv, struct primary_term *pt);

/******************************************************************
    parseUniverse - parse the universe of discourse
 ******************************************************************/

static struct fuzzyLv *parseUniverse(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError)
{
   double f, t;
   struct fuzzyLv *u;

   SavePPBuffer(theEnv, " ");

   f = (inputToken->tknType == FLOAT_TOKEN) ?
       ((CLIPSFloat *)inputToken->value)->contents :
       (double)((CLIPSInteger *)inputToken->value)->contents;

   GetToken(theEnv, readSource, inputToken);
   if (inputToken->tknType != FLOAT_TOKEN && inputToken->tknType != INTEGER_TOKEN)
     {
      SyntaxErrorMessage(theEnv, "Deftemplate: Number Expected ('to' part of Universe)");
      *DeftemplateError = true;
      return NULL;
     }

   t = (inputToken->tknType == FLOAT_TOKEN) ?
       ((CLIPSFloat *)inputToken->value)->contents :
       (double)((CLIPSInteger *)inputToken->value)->contents;

   if (f > t)
     {
      SyntaxErrorMessage(theEnv, "Deftemplate: Invalid interval for Universe of Discourse");
      *DeftemplateError = true;
      return NULL;
     }

   u = get_struct(theEnv, fuzzyLv);
   u->from = f;
   u->to = t;
   u->units = NULL;
   u->primary_term_list = NULL;

   SavePPBuffer(theEnv, " ");
   GetToken(theEnv, readSource, inputToken);
   if ((inputToken->tknType == STRING_TOKEN) || (inputToken->tknType == SYMBOL_TOKEN))
     {
      u->units = (CLIPSLexeme *)inputToken->value;
      PPCRAndIndent(theEnv);
      SavePPBuffer(theEnv, " ");
      GetToken(theEnv, readSource, inputToken);
     }
   else
     {
      PPBackup(theEnv);
      PPCRAndIndent(theEnv);
      SavePPBuffer(theEnv, " ");
      SavePPBuffer(theEnv, inputToken->printForm);
     }

   return u;
}

/******************************************************************
    parsePrimaryTermList - parse the list of primary terms
 ******************************************************************/

static void parsePrimaryTermList(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError,
  struct fuzzyLv *new_lv)
{
   struct primary_term *last_one, *next_one, *assert_list;

   last_one = assert_list = NULL;

   while ((next_one = parsePrimaryTerm(theEnv, readSource, inputToken,
                                       DeftemplateError, new_lv)) != NULL)
     {
      /* make sure not defining same term a second time */
      {
       char *thisName;
       struct primary_term *ptPtr = assert_list;

       thisName = (ValueToFuzzyValue(next_one->fuzzy_value_description))->name;

       while (ptPtr != NULL)
         {
          if (strcmp(thisName, (ValueToFuzzyValue(ptPtr->fuzzy_value_description))->name) == 0)
            {
             *DeftemplateError = true;
             SyntaxErrorMessage(theEnv, "Deftemplate (duplicate TERM being defined)");
             rtn_struct(theEnv, primary_term, next_one);
             rtnPrimaryTermList(theEnv, assert_list);
             return;
            }
          ptPtr = ptPtr->next;
         }
      }

      if (last_one == NULL)
        {
         assert_list = next_one;
         new_lv->primary_term_list = assert_list;
        }
      else
        { last_one->next = next_one; }
      last_one = next_one;
     }

   if (*DeftemplateError || (assert_list == NULL))
     {
      if (assert_list == NULL)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (At least one primary term must be defined)");
        }
      else
        {
         rtnPrimaryTermList(theEnv, assert_list);
         new_lv->primary_term_list = NULL;
        }
     }
}

/******************************************************************
    parsePrimaryTerm - parse a single primary term definition
 ******************************************************************/

static struct primary_term *parsePrimaryTerm(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError,
  struct fuzzyLv *new_lv)
{
   struct primary_term *ptr;
   const char *pt_name;
   struct fuzzy_value *fuzzy_value_dsc;

   GetToken(theEnv, readSource, inputToken);

   if (inputToken->tknType == RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      PPBackup(theEnv);
      SavePPBuffer(theEnv, " )");
      return NULL;
     }

   if (inputToken->tknType != LEFT_PARENTHESIS_TOKEN)
     {
      SyntaxErrorMessage(theEnv, "Deftemplate (Expected primary term)");
      *DeftemplateError = true;
      return NULL;
     }

   GetToken(theEnv, readSource, inputToken);

   if (inputToken->tknType == SYMBOL_TOKEN)
     { pt_name = ((CLIPSLexeme *)inputToken->value)->contents; }
   else
     {
      SyntaxErrorMessage(theEnv, "Deftemplate (Expected primary term name)");
      *DeftemplateError = true;
      return NULL;
     }

   SavePPBuffer(theEnv, " ");
   fuzzy_value_dsc = parseTemplateFuzzyValue(theEnv, readSource, inputToken,
                                             DeftemplateError, new_lv);

   if (fuzzy_value_dsc != NULL && inputToken->tknType == RIGHT_PARENTHESIS_TOKEN)
     {
      ptr = get_struct(theEnv, primary_term);
      if (fuzzy_value_dsc->name != NULL)
        genfree(theEnv, fuzzy_value_dsc->name, strlen(fuzzy_value_dsc->name) + 1);
      fuzzy_value_dsc->name = (char *)genalloc(theEnv, strlen(pt_name) + 1);
      strcpy(fuzzy_value_dsc->name, pt_name);
      ptr->fuzzy_value_description = AddFuzzyValue(theEnv, fuzzy_value_dsc);
      /* AddFuzzyValue makes a copy so return this one */
      rtnFuzzyValue(theEnv, fuzzy_value_dsc);
      ptr->next = NULL;
      PPCRAndIndent(theEnv);
      SavePPBuffer(theEnv, "  ");
      return ptr;
     }
   else
     {
      *DeftemplateError = true;
      rtnFuzzyValue(theEnv, fuzzy_value_dsc);
      SyntaxErrorMessage(theEnv, "Deftemplate (expected ')' )");
      return NULL;
     }
}

/******************************************************************
    parseTemplateFuzzyValue - determine fuzzy value type and parse
 ******************************************************************/

static struct fuzzy_value *parseTemplateFuzzyValue(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError,
  struct fuzzyLv *new_lv)
{
   struct fuzzy_value *fv_ptr = NULL;

   GetToken(theEnv, readSource, inputToken);

   if (inputToken->tknType == LEFT_PARENTHESIS_TOKEN)
     {
      if (new_lv == NULL)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (Missing universe of discourse description)");
         return NULL;
        }
      GetToken(theEnv, readSource, inputToken);
      if (inputToken->tknType == FLOAT_TOKEN || inputToken->tknType == INTEGER_TOKEN)
        { fv_ptr = parseSingletonFuzzyValue(theEnv, readSource, inputToken, DeftemplateError, new_lv); }
      else
        { fv_ptr = parseStandardFuzzyValue(theEnv, readSource, inputToken, DeftemplateError, new_lv); }
     }
   else
     {
      if (new_lv == NULL || new_lv->primary_term_list == NULL)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (Expecting linguistic expression and no terms defined");
        }
      else
        {
         fv_ptr = ParseLinguisticExpr(theEnv, readSource, inputToken, new_lv, DeftemplateError);
         if (*DeftemplateError == true)
           SyntaxErrorMessage(theEnv, "Deftemplate (Fuzzy set description or linguistic expression expected)");
        }
     }

   if (*DeftemplateError == true)
     {
      rtnFuzzyValue(theEnv, fv_ptr);
      return NULL;
     }
   else
     { return fv_ptr; }
}

/******************************************************************
    parseSingletonFuzzyValue - parse (x y)(x y)... style fuzzy set
 ******************************************************************/

static struct fuzzy_value *parseSingletonFuzzyValue(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError,
  struct fuzzyLv *u)
{
   struct fuzzy_value *fv;
   double previous;
   int i, count, maxlength = 10, increment = 10;
   double newx, newy, *tempx, *tempy;
   double xtolerance;

   fv = get_struct(theEnv, fuzzy_value);
   fv->whichDeftemplate = NULL;
   fv->name = NULL;
   fv->x = FgetArray(theEnv, maxlength);
   fv->y = FgetArray(theEnv, maxlength);
   fv->maxn = maxlength;

   xtolerance = ((u->to - u->from) >= 1.0) ? FUZZY_TOLERANCE
                                            : (u->to - u->from) * FUZZY_TOLERANCE;

   previous = u->from - 1.0;
   count = 0;

   while (inputToken->tknType == FLOAT_TOKEN || inputToken->tknType == INTEGER_TOKEN)
     {
      /* Token should be x coordinate */
      newx = (inputToken->tknType == FLOAT_TOKEN) ?
             ((CLIPSFloat *)inputToken->value)->contents :
             (double)((CLIPSInteger *)inputToken->value)->contents;

      if (newx < u->from)
        {
         if (u->from - newx > xtolerance)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (X value out of range (too small))");
            rtnFuzzyValue(theEnv, fv);
            return NULL;
           }
         newx = u->from;
        }
      else if (newx > u->to)
        {
         if (newx - u->to > xtolerance)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (X value out of range (too large))");
            rtnFuzzyValue(theEnv, fv);
            return NULL;
           }
         newx = u->to;
        }

      if (newx < previous)
        {
         if (previous - newx > FUZZY_TOLERANCE)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (Singleton x values must be in increasing order)");
            rtnFuzzyValue(theEnv, fv);
            return NULL;
           }
         newx = previous;
        }
      SavePPBuffer(theEnv, " ");
      previous = newx;

      /* Get the next token: y coordinate */
      GetToken(theEnv, readSource, inputToken);

      if (inputToken->tknType == FLOAT_TOKEN || inputToken->tknType == INTEGER_TOKEN)
        {
         newy = (inputToken->tknType == FLOAT_TOKEN) ?
                ((CLIPSFloat *)inputToken->value)->contents :
                (double)((CLIPSInteger *)inputToken->value)->contents;
        }
      else
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (Number expected)");
         rtnFuzzyValue(theEnv, fv);
         return NULL;
        }

      if (newy < 0.0)
        {
         if (newy < -FUZZY_TOLERANCE)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (Membership value must be >= 0.0)");
            rtnFuzzyValue(theEnv, fv);
            return NULL;
           }
         newy = 0.0;
        }
      else if (newy > 1.0)
        {
         if (newy - 1.0 > FUZZY_TOLERANCE)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (Membership must be <= 1.0)");
            rtnFuzzyValue(theEnv, fv);
            return NULL;
           }
         newy = 1.0;
        }

      /* Get the closing ')' for this pair */
      GetToken(theEnv, readSource, inputToken);
      if (inputToken->tknType == RIGHT_PARENTHESIS_TOKEN)
        {
         /* if this point same as last don't store it */
         if (count == 0 || !FZ_EQUAL(newx, fv->x[count-1]) || !FZ_EQUAL(newy, fv->y[count-1]))
           {
            /* if last 2 pts have same y value as this new one then
               replace the last one with this new one
               OR if only 1st point received and y value same replace */
            if ((count > 2 && newy == fv->y[count-1] && newy == fv->y[count-2]) ||
                (count == 1 && newy == fv->y[0]))
              {
               count--;
               if (count == 0) previous = u->from - 1.0;
              }

            /* Store new (x,y) re-allocating if necessary */
            if (count == maxlength)
              {
               tempx = FgetArray(theEnv, maxlength + increment);
               tempy = FgetArray(theEnv, maxlength + increment);
               for (i = 0; i < maxlength; i++)
                 {
                  tempx[i] = fv->x[i];
                  tempy[i] = fv->y[i];
                 }
               FrtnArray(theEnv, fv->x, maxlength);
               FrtnArray(theEnv, fv->y, maxlength);
               fv->x = tempx;
               fv->y = tempy;
               maxlength += increment;
               fv->maxn = maxlength;
              }
            fv->x[count] = newx;
            fv->y[count] = newy;
            count++;

            /* if last 3 points all have same x values and all y values
               are in increasing or decreasing order, replace 2nd with last */
            if (count > 2 && newx == fv->x[count-2] && newx == fv->x[count-3])
              {
               if ((newy > fv->y[count-2] && fv->y[count-2] > fv->y[count-3]) ||
                   (newy < fv->y[count-2] && fv->y[count-2] < fv->y[count-3]) ||
                   (count > 3 && newx == fv->x[count-4]))
                 { count--; fv->y[count-1] = fv->y[count]; }
              }
           }
        }
      else
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate ( ')' expected)");
         rtnFuzzyValue(theEnv, fv);
         return NULL;
        }
      SavePPBuffer(theEnv, " ");

      /* Get next token: closing ')' or opening '(' for next pair */
      GetToken(theEnv, readSource, inputToken);
      if ((inputToken->tknType == RIGHT_PARENTHESIS_TOKEN) ||
          (inputToken->tknType == STOP_TOKEN))
        {
         fv->n = count;
         return fv;
        }
      else if (inputToken->tknType != LEFT_PARENTHESIS_TOKEN)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate ( '(' expected)");
         rtnFuzzyValue(theEnv, fv);
         return NULL;
        }
      else
        { GetToken(theEnv, readSource, inputToken); }
     }

   *DeftemplateError = true;
   SyntaxErrorMessage(theEnv, "Deftemplate (Number expected)");
   rtnFuzzyValue(theEnv, fv);
   return NULL;
}

/******************************************************************
    parseStandardFuzzyValue - parse S/Z/PI standard functions
 ******************************************************************/

static struct fuzzy_value *parseStandardFuzzyValue(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError,
  struct fuzzyLv *u)
{
   struct fuzzy_value *fv;
   double alfa, beta, gamma;
   const char *name;
   int z_function = false, s_function = false, pi_function = false;
   double xtolerance;

   if ((inputToken->tknType != SYMBOL_TOKEN) && (inputToken->tknType != STRING_TOKEN))
     {
      *DeftemplateError = true;
      SyntaxErrorMessage(theEnv, "Deftemplate (Fuzzy standard function expected)");
      return NULL;
     }

   name = ((CLIPSLexeme *)inputToken->value)->contents;

   if ((strcmp(name, "S") == 0) || (strcmp(name, "s") == 0))
     s_function = true;
   else if ((strcmp(name, "Z") == 0) || (strcmp(name, "z") == 0))
     z_function = true;
   else if ((strcmp(name, "PI") == 0) || (strcmp(name, "pi") == 0))
     pi_function = true;

   if (!s_function && !z_function && !pi_function)
     {
      *DeftemplateError = true;
      SyntaxErrorMessage(theEnv, "Deftemplate (Fuzzy standard function name expected)");
      return NULL;
     }

   SavePPBuffer(theEnv, " ");

   xtolerance = ((u->to - u->from) >= 1.0) ? FUZZY_TOLERANCE
                                            : (u->to - u->from) * FUZZY_TOLERANCE;

   /* get first parameter */
   GetToken(theEnv, readSource, inputToken);
   if (inputToken->tknType != FLOAT_TOKEN && inputToken->tknType != INTEGER_TOKEN)
     {
      *DeftemplateError = true;
      SyntaxErrorMessage(theEnv, "Deftemplate (Number expected)");
      return NULL;
     }
   SavePPBuffer(theEnv, " ");
   alfa = (inputToken->tknType == FLOAT_TOKEN) ?
          ((CLIPSFloat *)inputToken->value)->contents :
          (double)((CLIPSInteger *)inputToken->value)->contents;

   if (pi_function)
     {
      if (alfa < 0.0)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (PI function 1st parameter must be >= 0)");
         return NULL;
        }
      else
        { beta = alfa; }
     }
   else if (alfa < u->from)
     {
      if (u->from - alfa > xtolerance)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (s or z function 1st parameter out of range (too small))");
         return NULL;
        }
      alfa = u->from;
     }
   else if (alfa > u->to)
     {
      if (alfa - u->to > xtolerance)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (s or z function 1st parameter out of range (too large))");
         return NULL;
        }
      alfa = u->to;
     }

   /* get 2nd parameter */
   GetToken(theEnv, readSource, inputToken);
   if (inputToken->tknType != FLOAT_TOKEN && inputToken->tknType != INTEGER_TOKEN)
     {
      *DeftemplateError = true;
      SyntaxErrorMessage(theEnv, "Deftemplate (Number expected for standard function parameter)");
      return NULL;
     }
   SavePPBuffer(theEnv, " ");
   gamma = (inputToken->tknType == FLOAT_TOKEN) ?
           ((CLIPSFloat *)inputToken->value)->contents :
           (double)((CLIPSInteger *)inputToken->value)->contents;

   if (pi_function)
     {
      if ((gamma > u->to) || (gamma < u->from))
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (pi function produces x values out of range)");
         return NULL;
        }
      else if ((gamma - beta) < u->from)
        {
         if (u->from - (gamma - beta) > xtolerance)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (pi function produces x values too small)");
            return NULL;
           }
         beta = gamma - u->from;
        }
      else if ((gamma + beta) > u->to)
        {
         if (gamma + beta - u->to > xtolerance)
           {
            *DeftemplateError = true;
            SyntaxErrorMessage(theEnv, "Deftemplate (pi function produces x values too large)");
            return NULL;
           }
         beta = u->to - gamma;
        }
     }
   else if (gamma < alfa)
     {
      *DeftemplateError = true;
      SyntaxErrorMessage(theEnv, "Deftemplate (s or z function 2nd parameter must be >= 1st parameter)");
      return NULL;
     }
   else if (gamma > u->to)
     {
      if (gamma - u->to > xtolerance)
        {
         *DeftemplateError = true;
         SyntaxErrorMessage(theEnv, "Deftemplate (S or Z function 2nd parameter out of range (too large))");
         return NULL;
        }
      gamma = u->to;
     }

   GetToken(theEnv, readSource, inputToken);
   if (inputToken->tknType == RIGHT_PARENTHESIS_TOKEN)
     {
      int ftype;

      if (s_function)       ftype = S_FUNCTION;
      else if (pi_function) ftype = PI_FUNCTION;
      else                  ftype = Z_FUNCTION;

      fv = Get_S_Z_or_PI_FuzzyValue(theEnv, alfa, beta, gamma, ftype);

      GetToken(theEnv, readSource, inputToken);
      return fv;
     }
   else
     {
      *DeftemplateError = true;
      SyntaxErrorMessage(theEnv, "Deftemplate ( ')' expected)");
      return NULL;
     }
}

/******************************************************************
    rtnPrimaryTermList - free a list of primary_term structs
 ******************************************************************/

static void rtnPrimaryTermList(
  Environment *theEnv,
  struct primary_term *pt)
{
   struct primary_term *this_one;

   while (pt != NULL)
     {
      this_one = pt;
      pt = pt->next;
      rtn_struct(theEnv, primary_term, this_one);
     }
}

/******************************************************************
    ParseFuzzyTemplate - parse a fuzzy deftemplate
 ******************************************************************/

struct fuzzyLv *ParseFuzzyTemplate(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  int *DeftemplateError)
{
   struct fuzzyLv *new_lv = NULL;

   /*========================================*/
   /* Parse the universe of discourse        */
   /*========================================*/

   if (inputToken->tknType == FLOAT_TOKEN || inputToken->tknType == INTEGER_TOKEN)
     {
      new_lv = parseUniverse(theEnv, readSource, inputToken, DeftemplateError);
      if (*DeftemplateError == true)
        { return NULL; }
     }

   /*========================================*/
   /* Check that next token is a '('.        */
   /*========================================*/

   if (inputToken->tknType != LEFT_PARENTHESIS_TOKEN)
     {
      SyntaxErrorMessage(theEnv, "Deftemplate (Expecting Fuzzy Term List)");
      *DeftemplateError = true;
      rtn_struct(theEnv, fuzzyLv, new_lv);
      return NULL;
     }
   else
     {
      parsePrimaryTermList(theEnv, readSource, inputToken, DeftemplateError, new_lv);
      if (*DeftemplateError == true)
        {
         rtn_struct(theEnv, fuzzyLv, new_lv);
         return NULL;
        }
     }

   GetToken(theEnv, readSource, inputToken);

   if (inputToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      SyntaxErrorMessage(theEnv, "Deftemplate (Closing ')' for deftemplate expected)");
      *DeftemplateError = true;
      rtnPrimaryTermList(theEnv, new_lv->primary_term_list);
      rtn_struct(theEnv, fuzzyLv, new_lv);
      return NULL;
     }

   PPBackup(theEnv);
   SavePPBuffer(theEnv, "\n)\n");

   return new_lv;
}

/******************************************************************
    RtnFuzzyTemplate - free a fuzzyLv structure
 ******************************************************************/

void RtnFuzzyTemplate(
  Environment *theEnv,
  struct fuzzyLv *lv)
{
   struct primary_term *pt, *next;

   if (lv == NULL) return;

   pt = lv->primary_term_list;
   while (pt != NULL)
     {
      next = pt->next;
      rtn_struct(theEnv, primary_term, pt);
      pt = next;
     }
   rtn_struct(theEnv, fuzzyLv, lv);
}

/******************************************************************
    rtnFuzzyValue - free a fuzzy_value structure
 ******************************************************************/

void rtnFuzzyValue(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   if (fv == NULL) return;

   if (fv->x != NULL) FrtnArray(theEnv, fv->x, fv->maxn);
   if (fv->y != NULL) FrtnArray(theEnv, fv->y, fv->maxn);
   if (fv->name != NULL)
     { genfree(theEnv, fv->name, strlen(fv->name) + 1); }

   rtn_struct(theEnv, fuzzy_value, fv);
}

/******************************************************************
    InstallFuzzyValue / DeinstallFuzzyValue
 ******************************************************************/

void InstallFuzzyValue(
  Environment *theEnv,
  void *fv)
{
   if (fv != NULL)
     { IncrementFuzzyValueCount(fv); }
}

void DeinstallFuzzyValue(
  Environment *theEnv,
  void *fv)
{
   if (fv != NULL)
     {
      CLIPSFuzzyValue *fvhn = (CLIPSFuzzyValue *) fv;
      ReleaseFuzzyValue(theEnv, fvhn);
     }
}

/******************************************************************
    InstallFuzzyTemplate / DeinstallFuzzyTemplate
 ******************************************************************/

void InstallFuzzyTemplate(
  Environment *theEnv,
  Deftemplate *theDeftemplate)
{
   struct primary_term *pt;
   struct fuzzyLv *fzTemplate = theDeftemplate->fuzzyList;

   if (fzTemplate != NULL)
     {
      if (fzTemplate->units != NULL)
        IncrementLexemeCount(fzTemplate->units);

      pt = fzTemplate->primary_term_list;
      while (pt != NULL)
        {
         ((CLIPSFuzzyValue *)pt->fuzzy_value_description)->contents->whichDeftemplate = theDeftemplate;
         InstallFuzzyValue(theEnv, (void *)pt->fuzzy_value_description);
         theDeftemplate->busyCount--;
         pt = pt->next;
        }
     }
}

void DeinstallFuzzyTemplate(
  Environment *theEnv,
  struct fuzzyLv *fzTemplate)
{
   struct primary_term *pt, *this_pt;

   if (fzTemplate != NULL)
     {
      if (fzTemplate->units != NULL)
        ReleaseLexeme(theEnv, fzTemplate->units);

      pt = fzTemplate->primary_term_list;
      while (pt != NULL)
        {
         DeinstallFuzzyValue(theEnv, (void *)pt->fuzzy_value_description);
         ((CLIPSFuzzyValue *)pt->fuzzy_value_description)->contents->whichDeftemplate->busyCount++;
         this_pt = pt;
         pt = pt->next;
         rtn_struct(theEnv, primary_term, this_pt);
        }

      rtn_struct(theEnv, fuzzyLv, fzTemplate);
     }
}

#endif /* FUZZY_DEFTEMPLATES */
