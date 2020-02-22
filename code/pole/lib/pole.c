#include <math.h>
#include "pole.h"

static const double eps = 1e-14;

int
pole_rhs(double time, const double y[4], double dy[4], void *param)
{
  enum {T = POLE_T, DT = POLE_DT, X = POLE_X, DX = POLE_DX};
  (void)(time);
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
