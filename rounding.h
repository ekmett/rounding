#ifndef INCLUDED_HASKELL_ROUNDING_H
#define INCLUDED_HASKELL_ROUNDING_H

#include <math.h>
#include <fenv.h>

#define FE_DEFAULT FE_TONEAREST

#define UNARY_T(t,f,g) \
    extern t f (int mode, t x);

#define BINARY_T(t,f,g) \
    extern t f (int mode, t x, t y);

#define INFIX_T(t,f,op) \
    extern t f (int mode, t x, t y);

// invoke the rounding "X-Macro"
#include "rounding.inc"

#undef UNARY_T
#undef BINARY_T
#undef INFIX_T

#endif
