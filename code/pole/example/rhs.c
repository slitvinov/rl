#include <stdlib.h>
#include <stdio.h>
#include <pole.h>

static const char *me = "rhs";
static void
usg(void)
{
    fprintf(stderr, "%s -m mc -f F -t t -d dt\n", me);
    exit(2);
}

int
main(int argc, char **argv)
{
    enum { T = POLE_T, DT = POLE_DT, X = POLE_X, DX = POLE_DX };
    double mc, F, t, dt;
    struct PoleParam p;
    double time;
    double y[POLE_N], dy[POLE_N];

    (void) argc;
    mc = F = t = dt = 0;
    while (*++argv != NULL && argv[0][0] == '-')
        switch (argv[0][1]) {
        case 'h':
            usg();
            break;
        case 'm':
            argv++;
            if (*argv == NULL) {
                fprintf(stderr, "%s: -m needs an argument\n", me);
                exit(2);
            }
            mc = atof(*argv);
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
    p.F = F;
    p.mc = mc;
    y[T] = t;
    y[DT] = dt;
    y[X] = 0;
    y[DX] = 0;
    time = 0;
    if (pole_rhs(time, y, dy, &p) != 0) {
        fprintf(stderr, "%s: pole_rhs failed\n", me);
        exit(2);
    }
    printf("%+.16e\n", dy[T]);
    printf("%+.16e\n", dy[DT]);
    printf("%+.16e\n", dy[X]);
    printf("%+.16e\n", dy[DX]);
    return 0;
}
