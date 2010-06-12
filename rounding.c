#include "rounding.h"

#define UNARY_T(t,f,g) \
    t f (int mode, t x) { \
        fesetround(mode); \
        t result = g(x); \
        fesetround(FE_DEFAULT); \
        return result; \
    }
        
#define BINARY_T(t,f,g) \
    t f (int mode, t x, t y) { \
        fesetround(mode); \
        t result = g(x, y); \
        fesetround(FE_DEFAULT); \
        return result; \
    }
        
#define INFIX_T(t,f,op) \
    t f (int mode, t x, t y) { \
        fesetround(mode); \
        t result = (op); \
        fesetround(FE_DEFAULT); \
        return result; \
    }
        
#include "rounding-inc.h"

