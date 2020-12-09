#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <math.h>
#include <random>


int *mass = NULL, *massr = NULL;
float bad = 0, badc = 0;
float count = 0;
int cheack = 0;
int can = 1, wc = 0;
double canal = 0.1, warning = 1.0;
const char *u;
char *y_pod = NULL, *a_pod = NULL;
int mem = 0, *degr = NULL, stolb = 0, **deg = NULL, **m = NULL, *g = NULL, **mb = NULL, dg = 0, *buf = NULL, pi_x;
int *a = NULL, *sind = NULL, *y = NULL, *con = NULL, *sig = NULL, w = 0, w1 = 0, step = 1, sig_s = 1, *e = NULL, e_s = 0;
int b = 0, del = 0;
unsigned int randomer = 0;

int power(int i, int j) {
    int n = 1;
    for (int k = 1; k <= j; k++)
        n *= i;
    return n;
}

long int fact(int x) {
    long int all = 1;
    for (int i = 1; i <= x; i++) {
        all *= i;
    }
    return all;
}

long int proizv(int n, int k) {
    long int all = 1;
    for (int i = n - k + 1; i <= n; i++)
        all *= i;
    return all;
}

long double cnk(int n, int k) {
    long double all;
    all = (long double) proizv(n, k) / (long double) fact(k);
    return all;
}

void scan_y(int j) {
    cheack = 0;
    y = (int *) calloc(j, sizeof(int));
    for (int i = 0; i < j; i++) {
        if (y_pod[i] == '0') {
            y[j - i - 1] = 0;
        } else {
            y[j - i - 1] = 1;
            cheack++;
        }
    }
}

void scan_a(int j) {
    cheack = 0;
    a = (int *) calloc(j, sizeof(int));
    for (int i = 0; i < j; i++) {
        if (a_pod[i] == '0') {
            a[j - i - 1] = 0;
        } else {
            a[j - i - 1] = 1;
            cheack++;
        }
    }
}

void scan(int j) {
    pi_x = 0;
    int p;
    sscanf(u, "%d", &p);
    double i;
    for (w = 0; w < j; w++) {
        pi_x += (p % 10) * power(2, w);
        p /= 10;
    }
}

void delt() {
    int f, i, p = dg + w1 - 1, *h = NULL;
    h = (int *) calloc(step - 1, sizeof(int));
    for (i = 0; i <= p; i++)
        h[i] = con[i];
    while (p >= dg) {
        f = dg;
        for (i = p; i >= 0; i--) {
            if (f >= 0)
                h[i] = (h[i] + g[f]) % 2;
            f--;
        }
        for (i = p; i >= 0; i--)
            if (h[i] == 1) {
                break;
            }
        p = i;
    }
    for (int i = 0; i < dg; i++)
        con[i] = h[i];
    free(h);
}

void pr_g(int k, int d) {
    int *h = NULL;
    h = (int *) calloc(d + dg + 1, sizeof(int));
    g = (int *) realloc(g, (d + dg + 1) * sizeof(int));
    for (int i = 0; i <= d; i++)
        if (mb[k][i] == 1) {
            for (int j = 0; j <= dg; j++)
                h[j + i] = (h[j + i] + g[j]) % 2;
        }
    for (int i = 0; i < d + dg + 1; i++)
        g[i] = h[i];
    dg = dg + d;
    free(h);
}

int plus(unsigned int f, unsigned int k) {
    int i = 0;
    if (mass[f] == mass[k%(step-1)])
        return -1;
    i = mass[f] ^ mass[k%(step-1)];
    i = massr[i] % (step - 1);
    return i;
}

void pr() {
    mem = 0;
    m = NULL;
    m = (int **) malloc(sizeof(int));
    for (int k = 0; k < stolb; k++) {
        if (deg[k][0] != 0) {
            mem += degr[k];
            m = (int **) realloc(m, mem * sizeof(int));
            buf = (int *) calloc(degr[k], sizeof(int));
            buf[0] = (deg[k][0] + deg[k][1]) % (step - 1);
            buf[1] = plus(deg[k][0], deg[k][1]);
            for (int i = 2; i < degr[k]; i++) {
                buf[i] = plus(deg[k][i], buf[i - 1]);
                for (int j = i - 1; j > 0; j--)
                    buf[j] = plus((deg[k][i] + buf[j]) % (step - 1), buf[j - 1]);
                buf[0] = (deg[k][i] + buf[0]) % (step - 1);
            }
            m[k] = buf;
            buf = NULL;
        } else {
            mem += 2;
            buf = (int *) realloc(buf, 1);
            buf[0] = 0;
            m = (int **) realloc(m, mem * sizeof(int));
            m[k] = buf;
        }
    }
}

void sr() {
    int c = 0, ch = 0, f = b;
    mem = 0;
    buf = NULL;
    deg = (int **) calloc(1, sizeof(int));
    buf = (int *) malloc(sizeof(int));
    for (int i = f; i <= (b + del - 2); i++) {
        int j = i % (step - 1);
        ch = 0;
        buf = (int *) realloc(buf, sizeof(int));
        if (stolb > 0) {
            for (int r = 0; r < stolb; r++)
                for (int l = 0; l < degr[r]; l++)
                    if (deg[r][l] == j) {
                        ch++;
                    }
        }
        if (ch != 0)
            continue;
        do {
            c++;
            buf = (int *) realloc(buf, c * sizeof(int));
            buf[c - 1] = j;
            j = (j * 2) % (step - 1);
        } while (j != (i % (step - 1)));
        mem += c;
        deg = (int **) realloc(deg, mem * sizeof(int *));
        deg[stolb] = buf;
        degr = (int *) realloc(degr, (stolb + 1) * sizeof(int));
        degr[stolb] = c;
        buf = NULL;
        c = 0;
        stolb++;
    }
}

void pole() {
    int i = 0, j = 0;
    mass[(int) strlen(u) - 1] = pi_x;
    massr[pi_x] = (int) strlen(u) - 1;
    for (i = 0; i < (int) strlen(u) - 1; i++) {
        mass[i] = power(2, i);
        massr[mass[i]] = i;
    }
    for (i = (int) strlen(u); i < step - 1; i++) {
        mass[i] = (mass[i - 1] << 1);
        if (mass[i] > (step)) {
            mass[i] = mass[i] ^ pi_x;
            mass[i] = mass[i] % (step);
        }
        massr[mass[i]] = i;
    }
}


void sindr() {
    int bf = -1, i = 0, j;
    sind = (int *) calloc(del, sizeof(int));
    while (y[i] != 1) {
        i++;
    }
    for (j = 0; j < del - 1; j++) {
        bf = (i * (b + j)) % (step - 1);
        for (int k = i + 1; k < (int) strlen(y_pod); k++) {
            if (y[k] == 1) {
                if (bf == -1) {
                    bf = (k * (b + j)) % (step - 1);
                } else {
                    bf = plus((k * (b + j)) % (step - 1), bf);
                }
            }
        }
        sind[j] = bf;
    }
}

void messi() {
    int i, j, ro_s = 2, *ro = NULL, delta, *signew = NULL, sig_snew = 1, l = 0;
    sig_s = 1;
    sig=NULL;
    sig = (int *) calloc(sig_s, sizeof(int));
    signew = (int *) calloc(sig_snew, sizeof(int));
    ro = (int *) calloc(ro_s, sizeof(int));
    ro[1] = 0;
    ro[0] = -1;
    sig[0] = 0;
    for (i = 1; i < del; i++) {
        if (sind[i - 1] != -1) {
            delta = (sig[0] + sind[i - 1]) % (step - 1);
        } else {
            delta = -1;
        }
        for (j = 1; j <= l; j++) {
            if (delta != -1) {
                if (sind[i - 1 - j] != -1) {
                    delta = plus(delta, ((sind[i - 1 - j]) + sig[j]) % (step - 1));
                }
            } else {
                if (sind[i - 1 - j] != -1) {
                    delta = (sig[j] + sind[i - 1 - j]) % (step - 1);
                } else {
                    delta = -1;
                }
            }
        }
        if (delta != -1) {
            if ((sig_snew + 1) <= ro_s) {
                signew = (int *) realloc(signew, (++sig_snew) * sizeof(int));
                signew[sig_snew - 1] = -1;
            }
            for (int k = 0; k <= l; k++) {
                signew[k] = sig[k];
            }
            for (int k = 0; k < ro_s; k++) {
                if (ro[k] != -1) {
                    if (signew[k] == -1) {
                        signew[k] = (ro[k] + delta) % (step - 1);
                    } else {
                        signew[k] = plus(signew[k], (ro[k] + delta) % (step - 1));
                    }
                }
            }
            if (2 * l <= i - 1) {
                l = i - l;
                ro_s = sig_s;
                ro = new int(ro_s);
                for (int k = 0; k < sig_s; k++) {
                    ro[k] = -1;
                    if (sig[k] != -1) {
                        ro[k] = (sig[k] - delta);
                        if (ro[k] < 0) {
                            ro[k] += (step - 1);
                        }
                    }
                }
            }
            sig_s = sig_snew;
            sig = (int *) realloc(sig, sig_s * sizeof(int));
            for (int k = 1; k < sig_s; k++) {
                sig[k] = signew[k] % (step - 1);
            }

        }
        ro = (int *) realloc(ro, (++ro_s) * sizeof(int));
        for (int k = ro_s - 1; k > 0; k--) {
            ro[k] = ro[k - 1];
            ro[k - 1] = -1;
        }
    }
}

void chenya() {
    int prov;
    e_s = 0;
    for (int i = 0; i < step - 1; i++) {
        prov = sig[0] % (step - 1);
        for (int j = 1; j < sig_s; j++) {
            if (prov == -1) {
                if (sig[j] != -1) {
                    prov = (sig[j] + (i * j)) % (step - 1);
                }
            } else {
                if (sig[j] != -1) {
                    prov = plus(prov, (sig[j] + (i * j)) % (step - 1));
                }
            }
        }
        if (prov == -1) {
            e_s++;
            e = (int *) realloc(e, e_s * sizeof(int));
            e[e_s - 1] = (step - i - 1) % (step - 1);
        }
    }
}

void zam() {
    for (int i = 0; i < e_s; i++) {
        if (y[e[i]] == 0) {
            y[e[i]] = 1;
        } else {
            y[e[i]] = 0;
        }
    }
}

void cod() {
    int j = 0, i = 0;
    char c;
    char *cr = NULL;
    con = NULL;
    scan_a((int) strlen(a_pod));
    w1 = (int) strlen(a_pod);
    con = (int *) calloc(w1 + dg, sizeof(int));
    for (i = 0; i < w1; i++) {
        con[i + dg] = a[i];
    }
    if (cheack > 0)
        delt();
}

void decod() {
    int j = 0, i = 0;
    char c;
    char *cr = NULL;
    y = NULL;
    sind = NULL;
    sig = NULL;
    e = NULL;
    scan_y((int) strlen(y_pod));
    if (cheack > 0) {
        sindr();
        messi();
        chenya();
        zam();
    }
}

using namespace std;

int main(int argc, char *argv[]) {
    random_device rd;
    mt19937_64 gen(rd());
    uniform_real_distribution<> dis(0, 1);
    struct timeval t0, t1;
    int j = 0, i = 0;
    char c;
    char *cr = NULL;
    unsigned long ltime, zamen;
    int skoc;
    float skok;
    int u_pod = atoi(argv[1]);
    switch (u_pod) {
        case (2):
            u = "111";
            break;
        case (3):
            u = "1011";
            break;
        case (4):
            u = "10011";
            break;
        case (5):
            u = "100101";
            break;
        case (6):
            u = "1000011";
            break;
        case (7):
            u = "10000011";
            break;
        case (8):
            u = "100011101";
            break;
        case (9):
            u = "1000010001";
            break;
        case (10):
            u = "10000001001";
            break;
        case (11):
            u = "100000000101";
            break;
        case (12):
            u = "1011101000101";
            break;
        case (13):
            u = "10000000011011";
            break;
        case (14):
            u = "111111001100001";
            break;
        case (15):
            u = "1000011010101001";
            break;
        case (16):
            u = "10001000000001011";
            break;
        case (17):
            u = "100000000000001111";
            break;
    };
    ltime = time(NULL);
    srand(ltime);
    canal = atof(argv[4]);
    b = atoi(argv[2]);
    del = atoi(argv[3]);
    randomer = atoi(argv[5]);
    for (i = 0; i < (int) strlen(u) - 1; i++)
        step *= 2;
    mass = (int *) malloc(step * ((int) strlen(u) - 1) * sizeof(int));
    massr = (int *) malloc(step * ((int) strlen(u) - 1) * sizeof(int));
    scan((int) strlen(u) - 1);
    pole();
    if (randomer > 0) {
        gettimeofday(&t0, 0);
        sr();
        pr();
        mb = (int **) malloc(sizeof(int));
        mem = 0;
        g = (int *) malloc((degr[0] + 1) * sizeof(int));
        buf = (int *) malloc(sizeof(int));
        for (i = 0; i < stolb; i++) {
            buf = (int *) malloc((degr[i] + 1) * sizeof(int));
            buf[degr[i]] = 1;
            for (j = 0; j < degr[i]; j++) {
                if (m[i][j] == 0) {
                    buf[j] = 1;
                } else {
                    buf[j] = 0;
                }
            }
            mem += degr[i];
            mb = (int **) realloc(mb, mem * sizeof(int));
            mb[i] = buf;
            if (i == 0) {
                g = buf;
            }
            buf = NULL;
        }
        dg = degr[0];
        if (stolb > 1) {
            for (i = 1; i < stolb; i++) {
                pr_g(i, degr[i]);
            }
        }
        printf("k: %d\n", step - 1 - dg);
        for (i = 0; i < randomer; i++) {
            a_pod = NULL;
            a_pod = (char *) calloc(step - dg, sizeof(char));
            a_pod[0] = '1';
            for (j = 0; j < step - dg; j++) {
                skoc = 0;//dis(gen);
                if (skoc >= 0.5) {
                    a_pod[j] = '1';
                } else {
                    a_pod[j] = '0';
                }
            }
            cod();
            y_pod = NULL;
            y_pod = (char *) calloc(dg + w1, sizeof(char));
            for (j = dg + w1 - 1; j >= 0; j--) {
                if (con[j] == 0) {
                    y_pod[dg + w1 - 1 - j] = '0';
                } else {
                    y_pod[dg + w1 - 1 - j] = '1';
                }
            }
            wc = 0;
            if (canal != 0) {
                for (j = 0; j < dg + w1; j++) {
                    skok = dis(gen);
                    if (canal >= skok)
                        if (y_pod[j] == '0') {
                            y_pod[j] = '1';
                            wc++;
                        } else {
                            y_pod[j] = '0';
                            wc++;
                        }
                }
            };
            decod();
            count++;
            badc = 0;
            for (j = 0; j < dg + w1; j++)
                if (con[j] != y[j])
                    badc++;
            if (badc > 0)
                bad++;
        }
        for (int k = 0; k <= (del - 1) / 2; k++) {ф
            warning -= pow(canal, k) * pow(1 - canal, step - 1 - k) * cnk(step - 1, k);
        }
        printf("%d %d %f %f\n", (int) count, (int) bad, bad / count, warning);
        gettimeofday(&t1, 0);
        long long elapsed = (t1.tv_sec - t0.tv_sec) * 1000000LL + t1.tv_usec - t0.tv_usec;
        printf("time elapsed = %lldμs\n", elapsed);
    }

    free(deg);
    free(degr);
    free(g);
    free(m);
    free(mb);
    free(buf);
    free(mass);
    free(massr);
    free(con);
    free(a);
    free(y);
    free(sind);
    free(e);
    return 0;
}
