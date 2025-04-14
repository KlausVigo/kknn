#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void dm(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void dmEuclid(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"dm",        (DL_FUNC) &dm,        10},
    {"dmEuclid",  (DL_FUNC) &dmEuclid,  9},
    {NULL, NULL, 0}
};

void R_init_kknn(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

