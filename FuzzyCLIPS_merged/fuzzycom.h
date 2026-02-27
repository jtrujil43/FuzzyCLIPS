   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*           FUZZY COMMANDS HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements fuzzy CLIPS commands like defuzzify,  */
/*   get-u, get-fs, plot-fuzzy-value, etc.                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             All functions now take Environment *.          */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzycom

#pragma once

#define _H_fuzzycom

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include "fuzzyval.h"

   void                           DeffuzzyCommands(Environment *);
   bool                           is_defuzzify_value_valid(Environment *);

   void                           getu(Environment *,UDFContext *,UDFValue *);
   void                           getu_from(Environment *,UDFContext *,UDFValue *);
   void                           getu_to(Environment *,UDFContext *,UDFValue *);
   void                           getu_units(Environment *,UDFContext *,UDFValue *);
   void                           get_fs(Environment *,UDFContext *,UDFValue *);
   void                           get_fs_template(Environment *,UDFContext *,UDFValue *);
   void                           get_fs_lv(Environment *,UDFContext *,UDFValue *);
   void                           get_fs_length(Environment *,UDFContext *,UDFValue *);
   void                           get_fs_value(Environment *,UDFContext *,UDFValue *);
   void                           get_fs_x(Environment *,UDFContext *,UDFValue *);
   void                           get_fs_y(Environment *,UDFContext *,UDFValue *);
   void                           moment_defuzzify(Environment *,UDFContext *,UDFValue *);
   void                           maximum_defuzzify(Environment *,UDFContext *,UDFValue *);
   void                           add_fuzzy_modifier(Environment *,UDFContext *,UDFValue *);
   void                           remove_fuzzy_modifier(Environment *,UDFContext *,UDFValue *);
   void                           set_fuzzy_inference_type(Environment *,UDFContext *,UDFValue *);
   void                           get_fuzzy_inference_type(Environment *,UDFContext *,UDFValue *);
   void                           set_fuzzy_display_precision(Environment *,UDFContext *,UDFValue *);
   void                           get_fuzzy_display_precision(Environment *,UDFContext *,UDFValue *);
   void                           set_alpha_value(Environment *,UDFContext *,UDFValue *);
   void                           get_alpha_value(Environment *,UDFContext *,UDFValue *);
   void                           plot_fuzzy_value(Environment *,UDFContext *,UDFValue *);
   struct fuzzy_value            *get_fuzzy_slot(Environment *,UDFContext *,UDFValue *);
   void                           fuzzy_union(Environment *,UDFContext *,UDFValue *);
   void                           fuzzy_intersection(Environment *,UDFContext *,UDFValue *);
   void                           fuzzy_modify(Environment *,UDFContext *,UDFValue *);
   void                           create_fuzzy_value(Environment *,UDFContext *,UDFValue *);

#endif /* FUZZY_DEFTEMPLATES */

#endif /* _H_fuzzycom */
