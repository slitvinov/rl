#include <math.h>
#include <stdlib.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_errno.h>
#include "pole.h"

static const double eps = 1e-14;
static const double EPSABS = 1e-8;
static const double EPSREL = 1e-8;

int
pole_rhs(double time, const double y[], double dy[], void *param)
{
    enum { T = POLE_T, DT = POLE_DT, X = POLE_X, DX = POLE_DX };

    (void) (time);
    double ddt, ddx;
    double mc, F, t, dt;
    struct PoleParam *p;

    p = param;
    mc = p->mc;
    F = p->F;
    t = y[T];
    dt = y[DT];
    if (pole_rhs0(mc, F, t, dt, &ddt, &ddx) != 0)
        return 1;
    dy[T] = y[DT];
    dy[X] = y[DX];
    dy[DT] = ddt;
    dy[DX] = ddx;
    return 0;
}

int
pole_rhs0(double mc, double F, double t, double dt, double *ddt,
          double *ddx)
{
    double A11, A12, A21, A22;
    double b1, b2;
    double det;

    A11 = cos(t);
    A12 = mc + 1;
    A21 = 4.0 / 3;
    A22 = cos(t);
    b1 = (-dt * dt * sin(t)) - F;
    b2 = -sin(t);
    det = A11 * A22 - A12 * A21;
    if (fabs(det) < eps)
        return 1;
    *ddt = (A12 * b2 - A22 * b1) / det;
    *ddx = -(A11 * b2 - A21 * b1) / det;
    return 0;
}

int
pole_rhs_linear(double mc, double F, double t, double dt, double *ddt,
                double *ddx)
{
    double A11, A12, A21, A22;
    double b1, b2;
    double det;

    A11 = 1;
    A12 = mc + 1;
    A21 = 4.0 / 3;
    A22 = 1;
    b1 = -F;
    b2 = -t;
    det = A11 * A22 - A12 * A21;
    if (fabs(det) < eps)
        return 1;
    *ddt = (A12 * b2 - A22 * b1) / det;
    *ddx = -(A11 * b2 - A21 * b1) / det;
    return 0;
}

static const int Type[] = {
    POLE_RK2,
    POLE_RK4,
    POLE_RK8PD,
    POLE_RKCK,
    POLE_RKF45,
};

static const gsl_odeiv2_step_type **Stype[] = {
    &gsl_odeiv2_step_rk2,
    &gsl_odeiv2_step_rk4,
    &gsl_odeiv2_step_rk8pd,
    &gsl_odeiv2_step_rkf45,
};

struct ODE {
    gsl_odeiv2_driver *d;
    gsl_odeiv2_system sys;
};
int
ode_ini(int type, double dt, void *param, struct ODE **pq)
{
    const int n = sizeof(Type) / sizeof(Type[0]);
    struct ODE *q;
    gsl_odeiv2_driver *d;
    gsl_odeiv2_system *sys;
    int i;

    for (i = 0;; i++) {
        if (i == n) {
            fprintf(stderr, "ode_ini: unknown type %d\n", type);
            return 1;
        }
        if (Type[i] == type)
            break;
    }
    q = malloc(sizeof(*q));
    if (q == NULL)
        return 1;
    sys = &q->sys;
    sys->function = pole_rhs;
    sys->jacobian = NULL;
    sys->dimension = POLE_N;
    sys->params = param;
    d = gsl_odeiv2_driver_alloc_y_new(sys, *Stype[i], dt, EPSABS, EPSREL);
    if (d == NULL)
        return 1;
    q->d = d;
    *pq = q;
    return 0;
}

int
ode_step(struct ODE *q, double tstep, double *time, double y[])
{
    int status;

    status = gsl_odeiv2_driver_apply(q->d, time, tstep, y);
    if (status != GSL_SUCCESS)
        return 1;
    return 0;
}

int
ode_fin(struct ODE *q)
{
    gsl_odeiv2_driver_free(q->d);
    free(q);
    return 0;
}
