#include <math.h>

static const double eps = 1e-14;

int F(double mc, double F, double t, double dt, double *ddt, double *ddx)
{
  double A11, A12, A21, A22;
  double b1, b2;
  double det;

  A11 = cos(t);
  A12 = mc+1;
  A21 = 4.0/3;
  A22 = cos(t);
  b1 = (-dt*dt*sin(t))-F;
  b2 = -sin(t);
  det = A11*A22-A12*A21;
  if (fabs(det) < eps)
    return 1;
  *ddt = -(A12*b2 - A22*b1)/det;
  *ddx = (A11*b2 - A21*b1)/det;
  return 0;
}
