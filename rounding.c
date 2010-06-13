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

const float pi_f_l = 13176794.0f/(1<<22);
const float pi_f_u = 13176795.0f/(1<<22);
const double pi_d_l = (3373259426.0 + 273688.0 / (1<<21)) / (1<<30);
const double pi_d_u = (3373259426.0 + 273689.0 / (1<<21)) / (1<<30);
