   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*             FUZZY LHS PATTERN MODULE                */
   /*******************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <string.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzylhs.h"
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
    GetFuzzyLHSPattern - parse a fuzzy LHS pattern

    A fuzzy LHS pattern is a linguistic expression (with modifiers
    and primary terms) that specifies what fuzzy value to match.
    The parsing is the same as ParseLinguisticExpr - it handles
    modifiers, primary terms, AND, OR, and brackets.

    Returns a fuzzy_value on success, NULL on error.
 ******************************************************************/

struct fuzzy_value *GetFuzzyLHSPattern(
  Environment *theEnv,
  const char *readSource,
  struct token *theToken,
  struct fuzzyLv *fzTemplate,
  int *error)
{
   struct fuzzy_value *fv;

   *error = 0;

   if (fzTemplate == NULL || fzTemplate->primary_term_list == NULL)
     {
      *error = 1;
      WriteString(theEnv, STDERR,
        "GetFuzzyLHSPattern: No primary terms defined for this template\n");
      return NULL;
     }

   /* The current token should already be positioned at the start
      of the linguistic expression. Use ParseLinguisticExpr which
      handles the full recursive descent grammar:
        <LExpr> ::= <LTerm> | <LTerm> OR <LExpr>
        <LTerm> ::= <modExpr> | <LTerm> AND <modExpr>
        <modExpr> ::= MODIFIER <modExpr> | <element>
        <element> ::= PRIMARY-TERM | [ <LExpr> ]
   */
   fv = ParseLinguisticExpr(theEnv, readSource, theToken, fzTemplate, error);

   if (*error)
     {
      if (fv != NULL) rtnFuzzyValue(theEnv, fv);
      return NULL;
     }

   return fv;
}

#endif /* FUZZY_DEFTEMPLATES */
