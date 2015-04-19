/*
 *  Copyright (C)2009-2015 Klaus Schliep
 *               
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

 
#include <R.h>
#include <Rmath.h>



#define MIN(x,y)  (((x)<(y)) ?    (x)  : (y))
#define MAX(x,y)  (((x)>(y)) ?    (x)  : (y))
#define EPS 1.0e-8
#define BIG 1.0e300



void dm(double *learn, double *valid, int *n, int *m, int *p, double *dm, int *cl, int *k, double *mink, double *weights){
    int i, j, l, t, nn, ii, kk; 
    double tmp, *dvec, maxD;
    int *cvec;

    kk = MAX(10L, *k);
    kk = MIN(kk, *n);
    nn = MIN(2L*kk, *n);

    cvec = (int *) R_alloc(nn, sizeof(int));
    dvec = (double *) R_alloc(nn, sizeof(double));
    for(t=0;t<nn;t++)dvec[t]= BIG;


    for(j=0;j<(*m);j++){
        i=0;
        ii=0L; 
        maxD = BIG;
      while(i<*n){
	      tmp=0.0;
        l=0; 
	      while(l<*p && tmp < (maxD+EPS)){
	        tmp+=pow(fabs(learn[i+l*n[0]]-valid[j+l*m[0]]),*mink)* weights[l];
          l++; 
	      }

        if(tmp < maxD){
            dvec[ii]=tmp;
	          cvec[ii]=i;
            ii++;
            }
        if( ii==(nn-1L) ){  
            rsort_with_index(dvec, cvec, nn);
            ii= *k-1L;
            maxD = dvec[*k-1L]; 
        }
        i++;         
	   }
     rsort_with_index(dvec, cvec, nn);
     for(t=0;t<*k;t++){
         cl[j+t * *m]=cvec[t];
         dm[j+t * *m]=pow(dvec[t],(1.0/(*mink)));
     }
  }
}



void dmEuclid(double *learn, double *valid, int *n, int *m, int *p, double *dm, int *cl, int *k, double *mink, double *weights){
    int i, j, l, t, nn, ii, kk; 
    double tmp, *dvec, maxD, tmp2;
    int *cvec;

    kk = MAX(10L, *k);
    kk = MIN(kk, *n);
    nn = MIN(2L*kk, *n);

    cvec = (int *) R_alloc(nn, sizeof(int));
    dvec = (double *) R_alloc(nn, sizeof(double));
    for(t=0;t<nn;t++)dvec[t]= BIG;
    for(j=0;j<(*m);j++){
        i=0L;
        ii=0L; 
        maxD = BIG;

        while(i<*n){
	        tmp=0.0;
            l=0; 
	        while(l<*p && tmp < (maxD+EPS)){
                tmp2 = learn[i+l*n[0]]-valid[j+l*m[0]];
	            tmp += (tmp2*tmp2)* weights[l];
                l++;               
	        }                      
            if(tmp < maxD){
                dvec[ii]=tmp;
                cvec[ii]=i;
                ii++;
            }
            if( ii==(nn-1L) ){  
                rsort_with_index(dvec, cvec, nn);
                ii= *k-1L;
                maxD = dvec[*k-1L]; 
                }
             i++;         
	    }
        rsort_with_index(dvec, cvec, nn);
        for(t=0;t<*k;t++){
            cl[j+t * *m]=cvec[t];
            dm[j+t * *m]=sqrt(dvec[t]);
        }
    }
}



void dmEuclid2(double *learn, double *valid, int *learnF, int *validF, int *n, int *m, int *p, int *p2, double *dm, int *cl, int *k, double *mink, double *weights, double *weights2){
    int i, j, l, l2, t, nn, ii, kk; 
    double tmp, *dvec, maxD, tmp2;
    int *cvec;

    kk = MAX(10L, *k);
    kk = MIN(kk, *n);
    nn = MIN(2L*kk, *n);

    cvec = (int *) R_alloc(nn, sizeof(int));
    dvec = (double *) R_alloc(nn, sizeof(double));
    for(t=0;t<nn;t++)dvec[t]= BIG;
    for(j=0;j<(*m);j++){
        i=0L;
        ii=0L; 
        maxD = BIG;
        while(i<*n){
            tmp=0.0;
            l=0; 
            l2=0;
	        while(l<*p && tmp < (maxD+EPS)){
                tmp2 = learn[i+l*n[0]]-valid[j+l*m[0]];
	            tmp += (tmp2*tmp2)* weights[l];
                l++;               
	        }
            while(l2<*p2 && tmp < (maxD+EPS)){
                if(learnF[i+l2*n[0]] != validF[j+l2*m[0]]) tmp += weights2[l2];
                l2++;               
	        }      
            if(tmp < maxD){
                dvec[ii]=tmp;
                cvec[ii]=i;
                ii++;
            }
            if( ii==(nn-1L) ){  
                rsort_with_index(dvec, cvec, nn);
                ii= *k-1L; // raus??
                maxD = dvec[*k-1L]; 
            }
            i++;         
	    }
        rsort_with_index(dvec, cvec, nn);
        for(t=0;t<*k;t++){
            cl[j+t * *m]=cvec[t];
            dm[j+t * *m]=sqrt(dvec[t]);
        }
    }
}


   




