enum {POLE_T, POLE_DT, POLE_X, POLE_DX};
enum {POLE_N = 4};
struct PoleParam {
  double mc, F;
};
int pole_rhs0(double mc, double F, double t, double dt, double *ddt,
              double *ddx);
int pole_rhs(double time, const double y[4], double dy[4], void *param);
