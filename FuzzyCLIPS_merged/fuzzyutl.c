   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*        FuzzyCLIPS Version 6.42a  02/26/26           */
   /*                                                     */
   /*            FUZZY UTILITIES MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Fuzzy set operations (complement, union,         */
/*   intersection), compositional rule of inference,         */
/*   max-of-min operations, global contribution.             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                                                           */
/* Revision History:                                         */
/*      6.42a: Ported to CLIPS 6.42 API.                     */
/*             All functions now take Environment *.          */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if FUZZY_DEFTEMPLATES

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "constant.h"
#include "envrnmnt.h"
#include "fuzzyutl.h"
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "fuzzyrhs.h"
#include "fuzzydef.h"
#include "memalloc.h"
#include "prntutil.h"
#include "router.h"
#include "symbol.h"
#include "factmngr.h"
#include "tmpltdef.h"

#if CERTAINTY_FACTORS
#include "cfdef.h"
#endif

/******************************************************************
    Local Internal Function Declarations
 ******************************************************************/

static struct fuzzy_value *horizontal_union(Environment *,struct fuzzy_value *fv,
                                            double yvalue);
static struct fuzzy_value *horizontal_intersection(Environment *,struct fuzzy_value *fv,
                                            double yvalue);
static struct fuzzy_value *max_prod_scale(Environment *,struct fuzzy_value *fv,
                                            double yvalue);
static struct fuzzy_value *horizontal_union_or_intersection(Environment *,struct fuzzy_value *fv,
                                  double yvalue, int unionFlg);
static void     concatenate(double *set1x, double *set1y, int length1,
                            double *set2x, double *set2y, int length2,
                            struct fuzzy_value *newValue);
static int      line_segment_intersection(double xP, double yP, double xQ, double yQ,
                            double xR, double yR, double xS, double yS,
                            double *x, double *y);
static void     STORE_THE_POINT(Environment *,struct fuzzy_value *result,
                            double xval, double yval);
static double   findOverlapMaxYvalue(double ay1, double ay2,
                            double by1, double by2);

/******************************************************************
    Global Variables
 ******************************************************************/

int saveFactsInProgress = 0;

/******************************************************************
    FZ_EQUAL - test two doubles for equality within fuzzy tolerance
 ******************************************************************/

int FZ_EQUAL(
  double f1,
  double f2)
{
   return (fabs(f1 - f2) < FUZZY_TOLERANCE);
}

/******************************************************************
    fcompliment - complement of fuzzy set (changes in place)
 ******************************************************************/

void fcompliment(
  Environment *theEnv,
  struct fuzzy_value *fv)
{
   int i;
   for (i = 0; i < fv->n; i++)
     { fv->y[i] = 1.0 - fv->y[i]; }
}

/******************************************************************
    line_segment_intersection

    Calculates the intersection of 2 line segments AB and CD.
    Returns: 0 if no intersection
             1 if intersection exists
             2 if segments are collinear
 ******************************************************************/

static int line_segment_intersection(
  double Ax, double Ay, double Bx, double By,
  double Cx, double Cy, double Dx, double Dy,
  double *X, double *Y)
{
   int numer2Positive, denomPositive;
   double denominator;
   double numerator1, numerator2;
   double r;

   double BymAy = By - Ay;
   double BxmAx = Bx - Ax;
   double DxmCx = Dx - Cx;
   double DymCy = Dy - Cy;
   double AymCy = Ay - Cy;
   double AxmCx = Ax - Cx;

   denominator = (BxmAx)*(DymCy) - (BymAy)*(DxmCx);
   numerator1 =  (AymCy)*(DxmCx) - (AxmCx)*(DymCy);

   if (fabs(denominator) < 1.0e-12)
     {
      if (numerator1 == 0.0)
        return 2;  /* collinear */
      else
        return 0;  /* parallel */
     }

   r = numerator1 / denominator;

   if (r < 0 || r > 1)
     return 0;

   numerator2 = (AymCy)*(BxmAx) - (AxmCx)*(BymAy);

   if (numerator2 != 0.0)
     {
      numer2Positive = (numerator2 > 0.0);
      denomPositive = (denominator > 0.0);
      if ((numer2Positive != denomPositive) ||
          (numer2Positive && (numerator2 > denominator)) ||
          (!numer2Positive && (numerator2 < denominator)))
        return 0;
     }

   if (BxmAx == 0.0)
     *X = Ax;
   else if (DxmCx == 0.0)
     *X = Cx;
   else
     *X = Ax + r*(BxmAx);

   if (Cy == Dy)
     *Y = Cy;
   else if (By == Ay)
     *Y = Ay;
   else
     *Y = Ay + r*(BymAy);

   return 1;
}

/******************************************************************
    STORE_THE_POINT - insert points into result array for
    union/intersection computations with deduplication logic
 ******************************************************************/

static void STORE_THE_POINT(
  Environment *theEnv,
  struct fuzzy_value *result,
  double xval,
  double yval)
{
   int i = result->n;
   double *resultX = result->x;
   double *resultY = result->y;

   if (i >= result->maxn)
     {
      WriteString(theEnv, STDERR, "[FUZZYUTL] STORE_THE_POINT: array overflow\n");
      return;
     }

   if (yval > 1.0) yval = 1.0;
   else if (yval < 0.0) yval = 0.0;

   if ((i != 0) && (xval < resultX[i-1]))
     {
      if ((resultX[i-1] - xval) > FUZZY_TOLERANCE)
        {
         WriteString(theEnv, STDERR, "[FUZZYUTL] Attempt to insert point with smaller x value\n");
         return;
        }
      xval = resultX[i-1];
     }

   /* If this point same as last one then do not insert */
   if ((i > 0) && FZ_EQUAL(xval, resultX[i-1]) && FZ_EQUAL(yval, resultY[i-1]))
     return;

   /* Handle 3+ points with same x value */
   if ((i > 2) && (xval == resultX[i-1]) && (xval == resultX[i-2]))
     {
      if ((yval < resultY[i-1] && resultY[i-1] < resultY[i-2]) ||
          (yval > resultY[i-1] && resultY[i-1] > resultY[i-2]) ||
          (i > 3 && xval == resultX[i-4]))
        {
         resultY[i-1] = yval;
         return;
        }
     }

   /* If inserting 2nd point with same y value as 1st, just update x */
   if ((i == 1) && (yval == resultY[0]))
     {
      resultX[0] = xval;
      return;
     }

   /* If last 2 points have same y value as this one, replace last x */
   if ((i > 2) && (yval == resultY[i-1]) && (yval == resultY[i-2]))
     {
      resultX[i-1] = xval;
      return;
     }

   resultX[i] = xval;
   resultY[i] = yval;
   result->n = ++i;
}

/******************************************************************
    findOverlapMaxYvalue - find max y of overlap of two vertical
    line segments going in opposite directions
 ******************************************************************/

static double findOverlapMaxYvalue(
  double ay1, double ay2,
  double by1, double by2)
{
   double maxa, mina, maxb, minb;

   /* same direction? */
   if (((ay2 < ay1) && (by2 < by1)) ||
       ((ay2 > ay1) && (by2 > by1)))
     return -1.0;

   if (ay2 < ay1)
     { maxa = ay1; mina = ay2; }
   else
     { maxa = ay2; mina = ay1; }

   if (by2 < by1)
     { maxb = by1; minb = by2; }
   else
     { maxb = by2; minb = by1; }

   if ((mina > maxb) || (minb > maxa))
     return -1.0;

   if (maxa > maxb)
     return maxa;
   else
     return maxb;
}

/******************************************************************
    max_of_min - returns maximum y value of intersection of two
    fuzzy sets
 ******************************************************************/

double max_of_min(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   return maxmin_intersect(theEnv, f1, f2, false, NULL);
}

/******************************************************************
    fintersect - returns the intersection of two fuzzy sets
 ******************************************************************/

struct fuzzy_value *fintersect(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   struct fuzzy_value *intersectSet;
   maxmin_intersect(theEnv, f1, f2, true, &intersectSet);
   return intersectSet;
}

/******************************************************************
    maxmin_intersect - compute max-of-min and optionally the
    intersection set of two fuzzy values
 ******************************************************************/

double maxmin_intersect(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2,
  int DoIntersect,
  struct fuzzy_value **intersectSet)
{
   double *Ax, *Ay, *Bx, *By;
   int Alength, Blength;
   double lastAx, lastAy, lastBx, lastBy;
   double currentAx, currentBx, currentAy, currentBy;
   double max, maxmin, Xmax;
   double X, Y;
   int Aindex, Bindex;
   int intersectFlag;
   int i, size;
   struct fuzzy_value *result;

   Ax = f1->x; Ay = f1->y; Alength = f1->n;
   Bx = f2->x; By = f2->y; Blength = f2->n;

   /* Both sets have only 1 member - constant fuzzy sets */
   if (Alength == 1 && Blength == 1)
     {
      if (Ay[0] < By[0])
        {
         if (DoIntersect) *intersectSet = CopyFuzzyValue(theEnv, f1);
         return Ay[0];
        }
      else
        {
         if (DoIntersect) *intersectSet = CopyFuzzyValue(theEnv, f2);
         return By[0];
        }
     }

   /* One set has 1 element */
   if (Alength == 1)
     {
      max = By[0];
      for (i = 1; i < Blength; i++)
        if (By[i] > max) max = By[i];
      if (max < Ay[0])
        { if (DoIntersect) *intersectSet = CopyFuzzyValue(theEnv, f2); }
      else
        {
         max = Ay[0];
         if (DoIntersect) *intersectSet = horizontal_intersection(theEnv, f2, max);
        }
      return max;
     }

   if (Blength == 1)
     {
      max = Ay[0];
      for (i = 1; i < Alength; i++)
        if (Ay[i] > max) max = Ay[i];
      if (max < By[0])
        { if (DoIntersect) *intersectSet = CopyFuzzyValue(theEnv, f1); }
      else
        {
         max = By[0];
         if (DoIntersect) *intersectSet = horizontal_intersection(theEnv, f1, max);
        }
      return max;
     }

   /* No intersection test */
   if (nonintersectiontest(Ax, Ay, Bx, By, Alength, Blength))
     {
      if (DoIntersect)
        {
         result = get_struct(theEnv, fuzzy_value);
         result->name = NULL;
         result->whichDeftemplate = f1->whichDeftemplate;
         result->x = FgetArray(theEnv, 1);
         result->y = FgetArray(theEnv, 1);
         result->n = 1;
         result->maxn = 1;
         result->x[0] = Ax[0];
         result->y[0] = 0.0;
         *intersectSet = result;
        }
      return 0.0;
     }

   /* Allocate result array */
   if (DoIntersect)
     {
      result = get_struct(theEnv, fuzzy_value);
      size = (Alength*3-1)/2 + (Blength*3-1)/2 + 1;
      result->x = FgetArray(theEnv, size);
      result->y = FgetArray(theEnv, size);
      result->n = 0;
      result->maxn = size;
      result->name = NULL;
      result->whichDeftemplate = f1->whichDeftemplate;
     }

   /* Set up starting points */
   lastAy = Ay[0];
   lastBy = By[0];

   if (Ax[0] == Bx[0])
     { lastAx = Ax[0]; lastBx = Bx[0]; Aindex = 1; Bindex = 1; }
   else if (Ax[0] < Bx[0])
     { lastAx = Ax[0]; lastBx = Ax[0]; Aindex = 1; Bindex = 0; }
   else
     { lastBx = Bx[0]; lastAx = Bx[0]; Aindex = 0; Bindex = 1; }

   if (lastAy <= lastBy)
     {
      maxmin = lastAy;
      if (DoIntersect) STORE_THE_POINT(theEnv, result, lastAx, lastAy);
     }
   else
     {
      maxmin = lastBy;
      if (DoIntersect) STORE_THE_POINT(theEnv, result, lastBx, lastBy);
     }

   Xmax = (Ax[Alength-1] >= Bx[Blength-1]) ? Ax[Alength-1] : Bx[Blength-1];

   currentAx = Ax[Aindex]; currentBx = Bx[Bindex];
   currentAy = Ay[Aindex]; currentBy = By[Bindex];

   while (Bindex < Blength || Aindex < Alength)
     {
      if (!DoIntersect && maxmin == 1.0)
        return 1.0;

      if (lastAx == lastBx && lastAy == lastBy)
        {
         if (currentAx == currentBx)
           {
            if (DoIntersect)
              {
               if (currentAy > currentBy)
                 STORE_THE_POINT(theEnv, result, currentBx, currentBy);
               else
                 STORE_THE_POINT(theEnv, result, currentAx, currentAy);
              }
            Aindex++; Bindex++;

            if (currentAy == currentBy)
              {
               if (currentAy > maxmin) maxmin = currentAy;
               if (!DoIntersect && (Aindex >= Alength || Bindex >= Blength))
                 return maxmin;
              }
            else if (currentAy < currentBy)
              {
               if (currentAy > maxmin) maxmin = currentAy;
               if (!DoIntersect && (Aindex >= Alength)) return maxmin;
              }
            else
              {
               if (currentBy > maxmin) maxmin = currentBy;
               if (!DoIntersect && (Bindex >= Blength)) return maxmin;
              }

            if (Aindex <= Alength)
              {
               lastAx = currentAx; lastAy = currentAy;
               if (Aindex == Alength) currentAx = Xmax;
               else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
              }
            if (Bindex <= Blength)
              {
               lastBx = currentBx; lastBy = currentBy;
               if (Bindex == Blength) currentBx = Xmax;
               else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
              }
           }
         else
           {
            double dAydBx, dBydAx;

            if (currentAx == lastAx)
              { dAydBx = (currentAy > lastAy) ? 1.0 : -1.0; dBydAx = 0.0; }
            else if (currentBx == lastBx)
              { dAydBx = 0.0; dBydAx = (currentBy > lastBy) ? 1.0 : -1.0; }
            else
              { dAydBx = (currentAy-lastAy)*(currentBx-lastBx);
                dBydAx = (currentBy-lastBy)*(currentAx-lastAx); }

            if (currentAx < currentBx)
              {
               Aindex++;
               if (dAydBx == dBydAx)
                 { lastBy = currentAy; lastBx = currentAx; }
               if (dAydBx <= dBydAx)
                 {
                  if (currentAy > maxmin) maxmin = currentAy;
                  if (DoIntersect) STORE_THE_POINT(theEnv, result, currentAx, currentAy);
                  else if (Aindex >= Alength) return maxmin;
                 }
               lastAy = currentAy; lastAx = currentAx;
               if (Aindex >= Alength) currentAx = Xmax;
               else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
              }
            else
              {
               Bindex++;
               if (dAydBx == dBydAx)
                 { lastAy = currentBy; lastAx = currentBx; }
               if (dBydAx <= dAydBx)
                 {
                  if (currentBy > maxmin) maxmin = currentBy;
                  if (DoIntersect) STORE_THE_POINT(theEnv, result, currentBx, currentBy);
                  else if (Bindex >= Blength) return maxmin;
                 }
               lastBy = currentBy; lastBx = currentBx;
               if (Bindex >= Blength) currentBx = Xmax;
               else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
              }
           }
        }
      else /* not same last points */
        {
         if (currentAx == currentBx && currentAy == currentBy)
           {
            Aindex++; Bindex++;
            if (currentAy > maxmin) maxmin = currentAy;
            if (DoIntersect) STORE_THE_POINT(theEnv, result, currentBx, currentBy);
            else if (Aindex >= Alength || Bindex >= Blength) return maxmin;

            if (Aindex <= Alength)
              {
               lastAx = currentAx; lastAy = currentAy;
               if (Aindex == Alength) currentAx = Xmax;
               else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
              }
            if (Bindex <= Blength)
              {
               lastBx = currentBx; lastBy = currentBy;
               if (Bindex == Blength) currentBx = Xmax;
               else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
              }
           }
         else
           {
            intersectFlag = line_segment_intersection(
                              lastAx, lastAy, currentAx, currentAy,
                              lastBx, lastBy, currentBx, currentBy,
                              &X, &Y);

            if (intersectFlag == 1)
              {
               if (Y > maxmin) maxmin = Y;
               lastAx = X; lastAy = Y;
               lastBx = X; lastBy = Y;

               if (currentAx == X && currentAy == Y)
                 {
                  Aindex++;
                  if (!DoIntersect && (Aindex >= Alength)) return maxmin;
                  if (Aindex <= Alength)
                    {
                     if (Aindex == Alength) currentAx = Xmax;
                     else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
                    }
                 }
               if (currentBx == X && currentBy == Y)
                 {
                  Bindex++;
                  if (!DoIntersect && (Bindex >= Blength)) return maxmin;
                  if (Bindex <= Blength)
                    {
                     if (Bindex == Blength) currentBx = Xmax;
                     else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
                    }
                 }
               if (DoIntersect) STORE_THE_POINT(theEnv, result, X, Y);
              }
            else /* NO intersection or collinear */
              {
               if (currentAx == currentBx)
                 {
                  if (intersectFlag == 2)
                    {
                     double overlapY = findOverlapMaxYvalue(lastAy, currentAy, lastBy, currentBy);
                     if (overlapY >= 0.0)
                       {
                        if (DoIntersect) STORE_THE_POINT(theEnv, result, currentAx, overlapY);
                        if (overlapY > maxmin) maxmin = overlapY;
                       }
                    }

                  if (DoIntersect)
                    {
                     if (currentAy > currentBy)
                       STORE_THE_POINT(theEnv, result, currentBx, currentBy);
                     else
                       STORE_THE_POINT(theEnv, result, currentAx, currentAy);
                    }
                  Aindex++; Bindex++;

                  if (currentAy == currentBy)
                    {
                     if (currentAy > maxmin) maxmin = currentAy;
                     if (!DoIntersect && (Aindex >= Alength || Bindex >= Blength))
                       return maxmin;
                    }
                  else if (currentAy < currentBy)
                    {
                     if (currentAy > maxmin) maxmin = currentAy;
                     if (!DoIntersect && (Aindex >= Alength)) return maxmin;
                    }
                  else
                    {
                     if (currentBy > maxmin) maxmin = currentBy;
                     if (!DoIntersect && (Bindex >= Blength)) return maxmin;
                    }

                  if (Bindex <= Blength)
                    {
                     lastBx = currentBx; lastBy = currentBy;
                     if (Bindex == Blength) currentBx = Xmax;
                     else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
                    }
                  if (Aindex <= Alength)
                    {
                     lastAx = currentAx; lastAy = currentAy;
                     if (Aindex == Alength) currentAx = Xmax;
                     else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
                    }
                 }
               else if (currentAx < currentBx)
                 {
                  double dABydBx, dBydABx;
                  if (intersectFlag == 2)
                    { dABydBx = 0.0; dBydABx = 0.0; }
                  else
                    {
                     dABydBx = (currentAy-lastBy)*(currentBx-lastBx);
                     dBydABx = (currentBy-lastBy)*(currentAx-lastBx);
                    }
                  Aindex++;
                  if (dABydBx <= dBydABx)
                    {
                     if (currentAy > maxmin) maxmin = currentAy;
                     if (DoIntersect) STORE_THE_POINT(theEnv, result, currentAx, currentAy);
                     else if (Aindex >= Alength) return maxmin;
                     if (dABydBx == dBydABx)
                       { lastBy = currentAy; lastBx = currentAx; }
                    }
                  lastAy = currentAy; lastAx = currentAx;
                  if (Aindex >= Alength) currentAx = Xmax;
                  else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
                 }
               else /* currentAx > currentBx */
                 {
                  double dBAydAx, dAydBAx;
                  if (intersectFlag == 2)
                    { dBAydAx = 0.0; dAydBAx = 0.0; }
                  else
                    {
                     dBAydAx = (currentBy-lastAy)*(currentAx-lastAx);
                     dAydBAx = (currentAy-lastAy)*(currentBx-lastAx);
                    }
                  Bindex++;
                  if (dBAydAx <= dAydBAx)
                    {
                     if (currentBy > maxmin) maxmin = currentBy;
                     if (DoIntersect) STORE_THE_POINT(theEnv, result, currentBx, currentBy);
                     else if (Bindex >= Blength) return maxmin;
                     if (dBAydAx == dAydBAx)
                       { lastAy = currentBy; lastAx = currentBx; }
                    }
                  lastBy = currentBy; lastBx = currentBx;
                  if (Bindex >= Blength) currentBx = Xmax;
                  else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
                 }
              }
           }
        }
     } /* end while */

   if (currentAy <= currentBy && currentAy > maxmin)
     maxmin = currentAy;
   else if (currentBy > maxmin)
     maxmin = currentBy;

   if (DoIntersect)
     {
      i = result->n;
      if (currentAy < currentBy)
        { Y = currentAy; X = currentAx; }
      else
        { Y = currentBy; X = currentBx; }

      if (Y != result->y[i-1])
        {
         if (i > 1 && (X == result->x[i-1] && X == result->x[i-2]))
           i--;
         result->x[i] = X;
         result->y[i] = Y;
         result->n = ++i;
        }

      while ((i > 1) && (result->y[i-1] == result->y[i-2]))
        { result->n--; i--; }

      if ((result->maxn - result->n) > 5)
        CompactFuzzyValue(theEnv, result);

      *intersectSet = result;
     }

   return maxmin;
}

/******************************************************************
    nonintersectiontest - returns true if the intersection set of
    A and B is NULL, or if either set has all zero y values
 ******************************************************************/

int nonintersectiontest(
  double *Ax, double *Ay,
  double *Bx, double *By,
  int Asize, int Bsize)
{
   int count, i;
   int firstAisnonzero, firstBisnonzero;
   int lastAisnonzero, lastBisnonzero;

   if (Asize > 1 && Bsize > 1)
     {
      int firstAiszero = (Ay[0] == 0.0);
      int lastBiszero = (By[Bsize-1] == 0.0);

      if ((Bx[Bsize-1] < Ax[0]) && firstAiszero && lastBiszero)
        return true;

      if ((Bx[Bsize-1] == Ax[0]) && firstAiszero && lastBiszero)
        {
         firstAisnonzero = false;
         lastBisnonzero = false;
         for (i = 1; i < Asize; i++)
           {
            if (Ax[i] != Ax[0]) break;
            if (Ay[i] != 0.0) { firstAisnonzero = true; break; }
           }
         for (i = Bsize-2; i >= 0; i--)
           {
            if (Bx[i] != Bx[Bsize-1]) break;
            if (By[i] != 0.0) { lastBisnonzero = true; break; }
           }
         if (!firstAisnonzero || !lastBisnonzero)
           return true;
        }

      {
       int firstBiszero2 = (By[0] == 0.0);
       int lastAiszero = (Ay[Asize-1] == 0.0);

       if ((Ax[Asize-1] < Bx[0]) && firstBiszero2 && lastAiszero)
         return true;

       if ((Ax[Asize-1] == Bx[0]) && firstBiszero2 && lastAiszero)
         {
          firstBisnonzero = false;
          lastAisnonzero = false;
          for (i = 1; i < Bsize; i++)
            {
             if (Bx[i] != Bx[0]) break;
             if (By[i] != 0.0) { firstBisnonzero = true; break; }
            }
          for (i = Asize-2; i >= 0; i--)
            {
             if (Ax[i] != Ax[Asize-1]) break;
             if (Ay[i] != 0.0) { lastAisnonzero = true; break; }
            }
          if (!firstBisnonzero || !lastAisnonzero)
            return true;
         }
      }
     }

   /* Check if either set is all zeros */
   count = 0;
   while (count < Asize)
     {
      if (Ay[count] > 0.0) break;
      count++;
     }
   if (count == Asize) return true;

   count = 0;
   while (count < Bsize)
     {
      if (By[count] > 0.0) return false;
      count++;
     }
   return true;
}

/******************************************************************
    concatenate - concatenate two fuzzy set point arrays
 ******************************************************************/

static void concatenate(
  double *set1x, double *set1y, int length1,
  double *set2x, double *set2y, int length2,
  struct fuzzy_value *newValue)
{
   double *x = newValue->x;
   double *y = newValue->y;
   int i, j;

   for (i = 0; i < length1; i++)
     { x[i] = set1x[i]; y[i] = set1y[i]; }

   j = length1;
   if ((set1x[length1-1] != set2x[0] || set1y[length1-1] != set2y[0]))
     { x[j] = set2x[0]; y[j] = set2y[0]; j++; }

   for (i = 1; i < length2; i++)
     { x[j] = set2x[i]; y[j] = set2y[i]; j++; }

   newValue->n = j;
}

/******************************************************************
    horizontal_union - union of a horizontal line and a fuzzy set
 ******************************************************************/

static struct fuzzy_value *horizontal_union(
  Environment *theEnv,
  struct fuzzy_value *fv,
  double yvalue)
{
   return horizontal_union_or_intersection(theEnv, fv, yvalue, true);
}

/******************************************************************
    horizontal_intersection - intersection of a horizontal line
    and a fuzzy set
 ******************************************************************/

static struct fuzzy_value *horizontal_intersection(
  Environment *theEnv,
  struct fuzzy_value *fv,
  double yvalue)
{
   return horizontal_union_or_intersection(theEnv, fv, yvalue, false);
}

/******************************************************************
    max_prod_scale - scale fuzzy set by yvalue/maxY if maxY > yvalue
 ******************************************************************/

static struct fuzzy_value *max_prod_scale(
  Environment *theEnv,
  struct fuzzy_value *fv,
  double yvalue)
{
   double maxYValueOfSet;
   struct fuzzy_value *fvResult;
   double *fvy;
   double scale;
   int i, n;

   fvResult = CopyFuzzyValue(theEnv, fv);
   fvy = fvResult->y;
   n = fvResult->n;

   maxYValueOfSet = fvy[0];
   for (i = 1; i < n; i++)
     if (fvy[i] > maxYValueOfSet) maxYValueOfSet = fvy[i];

   if (maxYValueOfSet <= yvalue)
     return fvResult;

   scale = yvalue / maxYValueOfSet;
   for (i = 0; i < n; i++)
     fvy[i] = fvy[i] * scale;

   return fvResult;
}

/******************************************************************
    horizontal_union_or_intersection - union or intersection of
    a horizontal line with a fuzzy set
 ******************************************************************/

static struct fuzzy_value *horizontal_union_or_intersection(
  Environment *theEnv,
  struct fuzzy_value *fv,
  double yvalue,
  int unionFlg)
{
   int i, num, count, newmax;
   struct fuzzy_value *newfv;
   double previousy, currenty, X;
   double *newX, *newY;
   double *fvX, *fvY;

   if ((unionFlg && yvalue == 0.0) ||
       (!unionFlg && yvalue == 1.0))
     return CopyFuzzyValue(theEnv, fv);

   newfv = get_struct(theEnv, fuzzy_value);
   newfv->name = NULL;
   newfv->whichDeftemplate = fv->whichDeftemplate;
   num = fv->n;

   newmax = (3*num - 1)/2;
   newfv->maxn = newmax;
   newfv->x = FgetArray(theEnv, newmax);
   newfv->y = FgetArray(theEnv, newmax);

   newX = newfv->x;
   newY = newfv->y;
   fvX = fv->x;
   fvY = fv->y;

   currenty = fvY[0];

   if ((unionFlg && (currenty >= yvalue)) ||
       (!unionFlg && (currenty <= yvalue)))
     { newX[0] = fvX[0]; newY[0] = fvY[0]; count = 1; }
   else
     count = 0;

   for (i = 1; i < num; i++)
     {
      previousy = currenty;
      currenty = fvY[i];
      if ((previousy < yvalue && currenty > yvalue) ||
          (previousy > yvalue && currenty < yvalue))
        {
         X = fvX[i-1] + (fvX[i] - fvX[i-1]) * (yvalue - fvY[i-1]) / (fvY[i] - fvY[i-1]);
         if (count == 0 || !FZ_EQUAL(X, newX[count-1]) || !FZ_EQUAL(yvalue, newY[count-1]))
           {
            if ((count == 1 && newY[0] == yvalue) ||
                (count > 2 && yvalue == newY[count-1] && yvalue == newY[count-2]) ||
                (count > 2 && X == newX[count-1] && X == newX[count-2]))
              count--;

            newX[count] = X;
            newY[count] = yvalue;
            count++;

            if (count > 2 && X == newX[count-2] && X == newX[count-3])
              {
               if ((yvalue > newY[count-2] && newY[count-2] > newY[count-3]) ||
                   (yvalue < newY[count-2] && newY[count-2] < newY[count-3]) ||
                   (count > 3 && X == newX[count-4]))
                 { count--; newY[count-1] = newY[count]; }
              }
           }
        }
      if ((unionFlg && (currenty >= yvalue)) ||
          (!unionFlg && (currenty <= yvalue)))
        {
         if (count == 0 || !FZ_EQUAL(fvX[i], newX[count-1]) || !FZ_EQUAL(currenty, newY[count-1]))
           {
            if ((count == 1 && newY[0] == currenty) ||
                (count > 2 && currenty == newY[count-1] && currenty == newY[count-2]) ||
                (count > 2 && fvX[i] == newX[count-1] && fvX[i] == newX[count-2]))
              count--;

            newX[count] = fvX[i];
            newY[count] = currenty;
            count++;

            if (count > 2 && fvX[i] == newX[count-2] && fvX[i] == newX[count-3])
              {
               if ((currenty > newY[count-2] && newY[count-2] > newY[count-3]) ||
                   (currenty < newY[count-2] && newY[count-2] < newY[count-3]) ||
                   (count > 3 && fvX[i] == newX[count-4]))
                 { count--; newY[count-1] = newY[count]; }
              }
           }
        }
     }

   if (count > 2 && newY[count-1] == newY[count-2])
     count--;
   else if (count == 0)
     { newX[0] = fvX[0]; newY[0] = yvalue; count = 1; }

   newfv->n = count;
   if (newmax - count > 5)
     CompactFuzzyValue(theEnv, newfv);

   return newfv;
}

/******************************************************************
    funion - compute union of two fuzzy values
 ******************************************************************/

struct fuzzy_value *funion(
  Environment *theEnv,
  struct fuzzy_value *f1,
  struct fuzzy_value *f2)
{
   struct fuzzy_value *result;
   double *Ax, *Ay, *Bx, *By;
   int Alength, Blength;
   double lastAx, lastAy, lastBx, lastBy;
   double currentAx, currentBx, currentAy, currentBy;
   double Xmax, X, Y;
   int Aindex, Bindex;
   int intersectFlag;
   int i, size;

   Ax = f1->x; Ay = f1->y; Alength = f1->n;
   Bx = f2->x; By = f2->y; Blength = f2->n;

   if (Alength == 1 && Blength == 1)
     return (Ay[0] < By[0]) ? CopyFuzzyValue(theEnv, f2) : CopyFuzzyValue(theEnv, f1);

   if (Alength == 1)
     return horizontal_union(theEnv, f2, Ay[0]);

   if (Blength == 1)
     return horizontal_union(theEnv, f1, By[0]);

   if (nonintersectiontest(Ax, Ay, Bx, By, Alength, Blength))
     {
      for (i = 0; i < Alength; i++)
        if (Ay[i] != 0.0) break;
      if (i == Alength) return CopyFuzzyValue(theEnv, f2);

      for (i = 0; i < Blength; i++)
        if (By[i] != 0.0) break;
      if (i == Blength) return CopyFuzzyValue(theEnv, f1);

      result = get_struct(theEnv, fuzzy_value);
      size = Alength + Blength;
      result->name = NULL;
      result->whichDeftemplate = f1->whichDeftemplate;
      result->x = FgetArray(theEnv, size);
      result->y = FgetArray(theEnv, size);
      result->n = 0;
      result->maxn = size;

      if (Ax[Alength-1] <= Bx[0])
        concatenate(Ax, Ay, Alength, Bx, By, Blength, result);
      else
        concatenate(Bx, By, Blength, Ax, Ay, Alength, result);
      return result;
     }

   result = get_struct(theEnv, fuzzy_value);
   size = (Alength*3-1)/2 + (Blength*3-1)/2 + 1;
   result->x = FgetArray(theEnv, size);
   result->y = FgetArray(theEnv, size);
   result->n = 0;
   result->maxn = size;
   result->name = NULL;
   result->whichDeftemplate = f1->whichDeftemplate;

   lastAy = Ay[0];
   lastBy = By[0];

   if (Ax[0] == Bx[0])
     { lastAx = Ax[0]; lastBx = Bx[0]; Aindex = 1; Bindex = 1; }
   else if (Ax[0] <= Bx[0])
     { lastAx = Ax[0]; lastBx = Ax[0]; Aindex = 1; Bindex = 0; }
   else
     { lastBx = Bx[0]; lastAx = Bx[0]; Aindex = 0; Bindex = 1; }

   if (lastAy >= lastBy)
     STORE_THE_POINT(theEnv, result, lastAx, lastAy);
   else
     STORE_THE_POINT(theEnv, result, lastBx, lastBy);

   Xmax = (Ax[Alength-1] >= Bx[Blength-1]) ? Ax[Alength-1] : Bx[Blength-1];

   currentAx = Ax[Aindex]; currentBx = Bx[Bindex];
   currentAy = Ay[Aindex]; currentBy = By[Bindex];

   while (Bindex < Blength || Aindex < Alength)
     {
      if (lastAx == lastBx && lastAy == lastBy)
        {
         if (currentAx == currentBx)
           {
            if (currentAy <= currentBy)
              STORE_THE_POINT(theEnv, result, currentBx, currentBy);
            else
              STORE_THE_POINT(theEnv, result, currentAx, currentAy);
            Aindex++; Bindex++;
            if (Aindex <= Alength)
              {
               lastAx = currentAx; lastAy = currentAy;
               if (Aindex == Alength) currentAx = Xmax;
               else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
              }
            if (Bindex <= Blength)
              {
               lastBx = currentBx; lastBy = currentBy;
               if (Bindex == Blength) currentBx = Xmax;
               else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
              }
           }
         else
           {
            double dAydBx, dBydAx;
            if (currentAx == lastAx)
              { dAydBx = (currentAy > lastAy) ? 1.0 : -1.0; dBydAx = 0.0; }
            else if (currentBx == lastBx)
              { dAydBx = 0.0; dBydAx = (currentBy > lastBy) ? 1.0 : -1.0; }
            else
              { dAydBx = (currentAy-lastAy)*(currentBx-lastBx);
                dBydAx = (currentBy-lastBy)*(currentAx-lastAx); }

            if (currentAx < currentBx)
              {
               if (dAydBx >= dBydAx)
                 STORE_THE_POINT(theEnv, result, currentAx, currentAy);
               lastAy = currentAy; lastAx = currentAx;
               Aindex++;
               if (Aindex >= Alength) currentAx = Xmax;
               else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
               if (dAydBx == dBydAx)
                 { lastBy = lastAy; lastBx = lastAx; }
              }
            else
              {
               if (dBydAx >= dAydBx)
                 STORE_THE_POINT(theEnv, result, currentBx, currentBy);
               lastBy = currentBy; lastBx = currentBx;
               Bindex++;
               if (Bindex >= Blength) currentBx = Xmax;
               else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
               if (dBydAx == dAydBx)
                 { lastAy = lastBy; lastAx = lastBx; }
              }
           }
        }
      else /* not same last points */
        {
         if (currentAx == currentBx && currentAy == currentBy)
           {
            STORE_THE_POINT(theEnv, result, currentBx, currentBy);
            Aindex++; Bindex++;
            if (Aindex <= Alength)
              {
               lastAx = currentAx; lastAy = currentAy;
               if (Aindex == Alength) currentAx = Xmax;
               else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
              }
            if (Bindex <= Blength)
              {
               lastBx = currentBx; lastBy = currentBy;
               if (Bindex == Blength) currentBx = Xmax;
               else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
              }
           }
         else
           {
            intersectFlag = line_segment_intersection(
                              lastAx, lastAy, currentAx, currentAy,
                              lastBx, lastBy, currentBx, currentBy,
                              &X, &Y);

            if (intersectFlag == 1)
              {
               lastAx = X; lastAy = Y;
               lastBx = X; lastBy = Y;
               if (currentAx == X && currentAy == Y)
                 {
                  Aindex++;
                  if (Aindex <= Alength)
                    {
                     if (Aindex == Alength) currentAx = Xmax;
                     else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
                    }
                 }
               if (currentBx == X && currentBy == Y)
                 {
                  Bindex++;
                  if (Bindex <= Blength)
                    {
                     if (Bindex == Blength) currentBx = Xmax;
                     else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
                    }
                 }
               STORE_THE_POINT(theEnv, result, X, Y);
              }
            else /* NO intersection */
              {
               if (currentAx == currentBx)
                 {
                  if (currentAy <= currentBy)
                    STORE_THE_POINT(theEnv, result, currentBx, currentBy);
                  else
                    STORE_THE_POINT(theEnv, result, currentAx, currentAy);
                  Aindex++; Bindex++;
                  if (Aindex <= Alength)
                    {
                     lastAx = currentAx; lastAy = currentAy;
                     if (Aindex == Alength) currentAx = Xmax;
                     else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
                    }
                  if (Bindex <= Blength)
                    {
                     lastBx = currentBx; lastBy = currentBy;
                     if (Bindex == Blength) currentBx = Xmax;
                     else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
                    }
                 }
               else if (currentAx < currentBx)
                 {
                  double dABydBx, dBydABx;
                  if (intersectFlag == 2)
                    { dABydBx = 0.0; dBydABx = 0.0; }
                  else
                    {
                     dABydBx = (currentAy-lastBy)*(currentBx-lastBx);
                     dBydABx = (currentBy-lastBy)*(currentAx-lastBx);
                    }
                  if (dABydBx >= dBydABx)
                    {
                     STORE_THE_POINT(theEnv, result, currentAx, currentAy);
                     if (dABydBx == dBydABx)
                       { lastBy = currentAy; lastBx = currentAx; }
                    }
                  lastAy = currentAy; lastAx = currentAx;
                  Aindex++;
                  if (Aindex >= Alength) currentAx = Xmax;
                  else { currentAx = Ax[Aindex]; currentAy = Ay[Aindex]; }
                 }
               else /* currentAx > currentBx */
                 {
                  double dBAydAx, dAydBAx;
                  if (intersectFlag == 2)
                    { dBAydAx = 0.0; dAydBAx = 0.0; }
                  else
                    {
                     dBAydAx = (currentBy-lastAy)*(currentAx-lastAx);
                     dAydBAx = (currentAy-lastAy)*(currentBx-lastAx);
                    }
                  if (dBAydAx >= dAydBAx)
                    {
                     STORE_THE_POINT(theEnv, result, currentBx, currentBy);
                     if (dBAydAx == dAydBAx)
                       { lastAy = currentBy; lastAx = currentBx; }
                    }
                  lastBy = currentBy; lastBx = currentBx;
                  Bindex++;
                  if (Bindex >= Blength) currentBx = Xmax;
                  else { currentBx = Bx[Bindex]; currentBy = By[Bindex]; }
                 }
              }
           }
        }
     } /* end while */

   i = result->n;
   if (currentAy > currentBy)
     { Y = currentAy; X = currentAx; }
   else
     { Y = currentBy; X = currentBx; }

   if (Y != result->y[i-1])
     {
      if (i > 1 && (X == result->x[i-1] && X == result->x[i-2]))
        i--;
      result->x[i] = X;
      result->y[i] = Y;
      result->n = ++i;
     }

   while ((i > 1) && (result->y[i-1] == result->y[i-2]))
     { result->n--; i--; }

   if ((result->maxn - result->n) > 5)
     CompactFuzzyValue(theEnv, result);

   return result;
}

/******************************************************************
    computeFuzzyConsequence - compute consequent fuzzy set based on
    antecedent match strength (compositional rule of inference)
 ******************************************************************/

void computeFuzzyConsequence(
  Environment *theEnv,
  Fact *new_fact)
{
   /* The compositional rule of inference requires access to the
      executing rule's LHS fuzzy pattern matches and min_of_maxmins.
      This requires deep integration with the CLIPS 6.42 rule engine
      internals (ExecutingRule, GlobalLHSBinds) which are not yet
      exposed for fuzzy pattern matching. This will be activated once
      full fuzzy LHS pattern matching is integrated into the rule
      engine. For now, fuzzy facts pass through unmodified. */
}

/******************************************************************
    changeValueOfFuzzySlots - global contribution: when asserting
    a fact with fuzzy slots that matches an existing fact, compute
    the union of the fuzzy values
 ******************************************************************/

void changeValueOfFuzzySlots(
  Environment *theEnv,
  Fact *fact1,
  Fact *fact2)
{
   /* Global contribution requires access to the internal field
      representation of facts in CLIPS 6.42 (which uses CLIPSValue
      arrays differently from the old struct field approach).
      This will be activated once fuzzy deftemplate parsing produces
      facts with FUZZY_VALUE_TYPE slots. */
}

/******************************************************************
    PrintFuzzyTemplateFact
 ******************************************************************/

void PrintFuzzyTemplateFact(
  Environment *theEnv,
  const char *logName,
  struct fuzzy_value *fv
#if CERTAINTY_FACTORS
  ,double CF
#endif
  )
{
   if (fv == NULL) return;
   if (fv->name != NULL)
     { WriteString(theEnv, logName, fv->name); }
   else
     { PrintFuzzySet(theEnv, logName, fv); }
#if CERTAINTY_FACTORS
   {
     char buf[64];
     snprintf(buf, sizeof(buf), " CF %.2f", CF);
     WriteString(theEnv, logName, buf);
   }
#endif
}

/******************************************************************
    PrintFuzzySet
 ******************************************************************/

void PrintFuzzySet(
  Environment *theEnv,
  const char *logName,
  struct fuzzy_value *fv)
{
   int i;
   char buf[128];

   if (fv == NULL) return;

   WriteString(theEnv, logName, "( ");
   for (i = 0; i < fv->n; i++)
     {
      snprintf(buf, sizeof(buf), "%g/%g ", fv->y[i], fv->x[i]);
      WriteString(theEnv, logName, buf);
     }
   WriteString(theEnv, logName, ")");
}

#endif /* FUZZY_DEFTEMPLATES */
