#include <stdlib.h>
#include <stdio.h>
#include <pole.h>

static const char *me = "rhs0";
static void
usg(void)
{
    fprintf(stderr, "%s -m mc -f F -t t -d dt\n", me);
    exit(2);
}

int
main(int argc, char **argv)
{
    double mc, F, t, dt;
    double ddt, ddx;

    (void) argc;
    int (*rhs)(double, double, double, double, double *, double *);

    mc = F = t = dt = 0;
    rhs = pole_rhs0;
    while (*++argv != NULL && argv[0][0] == '-')
        switch (argv[0][1]) {
        case 'h':
            usg();
            break;
        case 'l':
            rhs = pole_rhs_linear;
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
    if (rhs(mc, F, t, dt, &ddt, &ddx) != 0) {
        fprintf(stderr, "%s: rhs failed\n", me);
        exit(2);
    }
    printf("%+.16e\n", ddt);
    printf("%+.16e\n", ddx);
}
