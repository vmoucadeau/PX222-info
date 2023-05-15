#include "../bits/bits.h"
#include "pol.h"
#include <stdio.h>

// Tools
char hex[] = "0123456789abcdef";
int search_hexval(char val) {
    for(int i = 0; i < 16; i++) {
        if (val == hex[i]) {
            return i;
        }
    }
    return 0;
}



// Polynômes génériques

void pol_init(pol a) {
    for(int i = 0; i <= DEG_MAX_POL; i++) {
        a[i] = 0;   
    }
}

void pol_fill(pol a, int deg, pol res) {
    pol_init(res);
    for(int i = deg; i >= 0; i--) {
        res[i] = a[i];
    }
}



void pol_show(pol a) {
    int deg_a = pol_deg(a);
    printf("[");
    for(int i = deg_a; i >= 0; i--) {
        i == 0 ? printf("%i", a[i]) : printf("%i ", a[i]); 
    }
    printf("]\n");
}

int pol_deg(pol a) {
    int deg = DEG_MAX_POL;
    for(deg; deg >= 0 && a[deg] == 0; deg--);
    return deg;
}

int pol_maxdeg(pol a, pol b) {
    return pol_deg(a) > pol_deg(b) ? pol_deg(a) : pol_deg(b);
}

void pol_copy(pol a, pol dest) {
    for(int i = 0; i <= DEG_MAX_POL; i++) {
        dest[i] = a[i];
    }
}

void pol_add(pol a, pol b, pol res) {
    pol_init(res);
    for(int i = 0; i <= DEG_MAX_POL; i++) {
        res[i] = a[i] + b[i];
    }
}

void pol_sub(pol a, pol b, pol res) {
    pol_init(res);
    for(int i = 0; i <= DEG_MAX_POL; i++) {
        res[i] = a[i] - b[i];
    }
}

void pol_mul(pol a, pol b, pol res) {
    int deg_a = pol_deg(a); int deg_b = pol_deg(b);
    pol_init(res);
    for(int i = deg_a; i >= 0; i--) {
        for(int j = deg_b; j >= 0; j--) {
            res[i+j] += a[i] * b[j]; // Attention de ne pas dépasser le degré max
        }
    }
}

void pol_mulbyx(pol a, pol res) {
    pol_init(res);
    for(int i = 0; i < DEG_MAX_POL; i++) { // Attention de ne pas dépasser le degré max
        res[i+1] = a[i];
    }
}

void pol_div(pol a, pol b, pol res) {
    int deg_a = pol_deg(a); int deg_b = pol_deg(b);
    pol_init(res);
    pol_copy(a, res);
    for(int i = deg_a; i >= deg_b; i--) {
        res[i] = res[i] / b[deg_b];
        for(int j = deg_b; j >= 0; j--) {
            res[i-j] -= res[i] * b[j];
        }
    }
}


// GF256

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
    printf("%c%c\n", hex[pol / 16], hex[pol % 16]);
}

gf256 gf256_parse(char input[2]) {
    return search_hexval(input[0])*16 + search_hexval(input[1]);
}

gf256 gf256_add(gf256 pol1, gf256 pol2) {
    return pol1 ^ pol2;
}

gf256 gf256_mul(gf256 pol1, gf256 pol2) {
    // Copier pol1 et pol2 dans un pol générique
    pol a; pol_init(a);
    pol b; pol_init(b);
    for(int i = 0; i < 8; i++) {
        a[i] = (pol1 >> i) & 1;
        b[i] = (pol2 >> i) & 1;
    }
    // Initialiser le résultat
    pol res; pol_init(res);
    // Multiplier
    pol_mul(a, b, res);
    // Réduire par x^8 + x^4 + x^3 + x + 1
    pol div; pol_init(div);
    div[8] = 1; div[4] = 1; div[3] = 1; div[1] = 1; div[0] = 1;
    pol_div(res, div, res);
    // Convertir le résultat en gf256
    // gf256 res_gf256 = 0;
    // for(int i = 0; i < 8; i++) {
    //     res_gf256 += res[i] << i;
    // }
    return res;
}
