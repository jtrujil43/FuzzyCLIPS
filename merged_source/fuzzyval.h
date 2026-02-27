   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*          FUZZY VALUE STRUCTURE HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines the fuzzy_value structure used to        */
/*   represent fuzzy sets as membership functions.           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             Removed VOID/globle, added Environment *.     */
/*                                                           */
/*************************************************************/

#ifndef _H_fuzzyval

#pragma once

#define _H_fuzzyval

struct fuzzy_value;

#include "symbol.h"

/*********************************************************************/
/* FUZZY_VALUE STRUCTURE:                                            */
/*  pointer to the deftemplate (fuzzy) associated with fuzzy value   */
/*  name of the fuzzy value (linguistic expression pointer)          */
/*  maxn  - size of x and y arrays                                   */
/*  n     - number of elements in x and y in use                     */
/*  x,y   - the membership values                                    */
/*********************************************************************/
struct fuzzy_value
  {
   struct deftemplate *whichDeftemplate; /* the template (fuzzy) */
   char *name;      /* the fuzzy value linguistic */
                    /* expression eg. "very cold" */
   int maxn;
   int n;
   double *x;
   double *y;
  };

#endif /* _H_fuzzyval */
