#include "../bits/bits.h"
#include "pol.h"
#include <stdio.h>


void gf256_showbin(gf256 pol) {
    int i, k, andmask;
    for (i = 7; i >= 0;i--)
    {
        andmask = 1 << i;
        k = pol & andmask;
        k == 0 ? printf ("0") : printf ("1");
    }
}

void gf256_showhex(gf256 pol) {
    printf("%c%c", hex[pol / 16], hex[pol % 16]);
}

gf256 gf256_parse(char input[2]) {
    return search_hexval(input[0])*16 + search_hexval(input[1]);
}

void pol_init(pol a) {
    for(int i = 0; i < DEG_MAX_POL; i++)
        a[i] = 0;
}

int pol_deg(pol a) {
    int deg = DEG_MAX_POL;
    for(deg; deg >= 0 && a[deg] == 0; deg--);
    return deg;
}

void pol_copy(pol a, pol dest) {
    for(int i = 0; i < DEG_MAX_POL; i++) {
        dest[i] = a[i];
    }
}

void pol_add(pol a, pol b, pol res) {
    for(int i = 0; i < DEG_MAX_POL; i++) {
        res[i] = a[i] + b[i];
    }
}

void pol_sub(pol a, pol b, pol res) {
    for(int i = 0; i < DEG_MAX_POL; i++) {
        res[i] = a[i] - b[i];
    }
}

void pol_mul(pol a, pol b, pol res) {
    int deg_a = pol_deg(a); int deg_b = pol_deg(b);
    pol_init(res);
    for(int i = 0; i < DEG_MAX_POL; i++) {
        for(int j = 0; j < DEG_MAX_POL; j++) {
            res[i+j] = a[i+j] + a[i] * b[j]; // Attention de ne pas dépasser le degré max
        }
    }
}

gf256 gf256_add(gf256 pol1, gf256 pol2) {
    return pol1 ^ pol2;
}

char pol_mul(char pol1, char pol2) {
    
}