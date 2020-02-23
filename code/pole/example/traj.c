#include <stdlib.h>
#include <stdio.h>
#include <pole.h>

static const double STEP = 1e-6;

static const char *me = "traj";
static void
usg(void)
{
    fprintf(stderr, "%s [-e end] [-m mc] [-f F] [-t t] [-d dt] [-x x]\n",
            me);
    exit(2);
}

int
main(int argc, char **argv)
{
    enum { T = POLE_T, DT = POLE_DT, X = POLE_X, DX = POLE_DX };
    double mc, F, t, dt, x, tend;
    struct PoleParam param;
    double y[POLE_N], dy[POLE_N];
    struct ODE *ode;
    double time;
    int type;
    int nstep;

    (void) argc;
    mc = F = t = dt = x = 0;
    nstep = 10;
    while (*++argv != NULL && argv[0][0] == '-')
        switch (argv[0][1]) {
        case 'h':
            usg();
            break;
        case 'n':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -n needs an argument\n", me);
                exit(2);
            }
            nstep = atoi(*argv);
            break;
        case 'e':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -e needs an argument\n", me);
                exit(2);
            }
            tend = atof(*argv);
            break;
        case 'm':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -m needs an argument\n", me);
                exit(2);
            }
            mc = atof(*argv);
            break;
        case 'x':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -x needs an argument\n", me);
                exit(2);
            }
            x = atof(*argv);
            break;
        case 'f':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -f needs an argument\n", me);
                exit(2);
            }
            F = atof(*argv);
            break;
        case 't':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -t needs an argument\n", me);
                exit(2);
            }
            t = atof(*argv);
            break;
        case 'd':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -d needs an argument\n", me);
                exit(2);
            }
            dt = atof(*argv);
            break;
        default:
            fprintf(stderr, "%s: unknown option '%s'\n", me, argv[0]);
            exit(2);
        }

    type = POLE_RK8PD;
    param.F = F;
    param.mc = mc;
    time = 0.0;
    y[T] = t;
    y[DT] = dt;
    y[X] = x;
    y[DX] = 0;
    if (ode_ini(type, STEP, &param, &ode) != 0) {
        fprintf(stderr, "%s: ode_ini failed\n", me);
        exit(2);
    };
    printf("time theta dtheta x dx\n");
    double dstep;
    int i;

    for (i = 1;; i++) {
        dstep = i * tend / nstep;
        printf("%.16e %.16e %.16e %.16e %.16e\n",
               time, y[T], y[DT], y[X], y[DX]);
        if (i > nstep)
            break;
        if (ode_step(ode, dstep, &time, y) != 0) {
            fprintf(stderr, "%s: ode_step failed\n", me);
            exit(2);
        }
    }
    ode_fin(ode);
    return 0;
}
