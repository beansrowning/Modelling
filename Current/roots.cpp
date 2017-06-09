#include <R.h>
#include <Rinternals.h>

void roots(int *nin, double *x)
{
   int n = nin[0];
   Rboolean y;
   int i;
   
   for(i=0; i<n; i++)
      if(i == 0)
          if(x[i]>0)
              y = TRUE;
          else
              y = FALSE;
      else
          if(x[i]>0)
              if(x[i-1] == 0)
                  y = TRUE;
              else 
                  y = FALSE;
          else
              if(x[i-1]>0)
                  y = TRUE;
              else
                  y = FALSE;
}