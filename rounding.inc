#define UNARY(fun) \
    UNARY_T(double, m ## fun, fun) \
    UNARY_T(float , m ## fun ## f, fun) \
    UNARY_T(long double , m ## fun ## l, fun)

#define BINARY(fun) \
    BINARY_T(double, m ## fun, fun) \
    BINARY_T(float , m ## fun ## f, fun) \
    BINARY_T(long double , m ## fun ## l, fun)

#define INFIX(fun, op) \
    INFIX_T(double, m ## fun, op) \
    INFIX_T(float , m ## fun ## f, op) \
    INFIX_T(long double , m ## fun ## l, op)

UNARY(fabs)
UNARY(exp)
// UNARY(expm1)
// UNARY(exp2)
UNARY(log)
// UNARY(logp1)
UNARY(sqrt)
UNARY(sin)
UNARY(cos)
UNARY(tan)
UNARY(asin)
UNARY(acos)
UNARY(atan)
UNARY(sinh)
UNARY(cosh)
UNARY(tanh)
UNARY(asinh)
UNARY(acosh)
UNARY(atanh)
UNARY(ceil)
UNARY(floor)
UNARY(round)
UNARY(trunc)
BINARY(atan2)
// BINARY(hypot)
// BINARY(cbrt)
BINARY(pow)
INFIX(add, (x + y))
INFIX(times, (x * y))
INFIX(div, (x / y))
INFIX(minus, (x - y))

#undef UNARY
#undef BINARY
#undef INFIX
