enum { POLE_T, POLE_DT, POLE_X, POLE_DX };
enum { POLE_N = 4 };
enum {
    POLE_RK2,
    POLE_RK4,
    POLE_RK8PD,
    POLE_RKCK,
    POLE_RKF45,
};

struct PoleParam {
    double mc, F;
};
int pole_rhs0(double mc, double F, double t, double dt, double *ddt,
              double *ddx);
int pole_rhs_linear(double mc, double F, double t, double dt, double *ddt,
                    double *ddx);
int pole_rhs(double time, const double y[4], double dy[4], void *param);

struct ODE;
int ode_ini(int type, double dt, void *param, struct ODE **);
int ode_step(struct ODE *, double tstep, double *time, double y[]);
int ode_fin(struct ODE *);
