#include "../bits/bits.h"
#include "pol.h"
#include <stdio.h>

// Tools

int search_hexval(char val) {
    char hex[] = "0123456789abcdef";
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
    for(int i = 0; i <= deg_a; i++) {
        i == deg_a ? printf("%i", a[i]) : printf("%i ", a[i]); 
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
    for(int i = 0; i < DEG_MAX_POL; i++) { // Attention de ne pas dépasser le degré max
        res[i+1] = a[i];
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

int gf256_deg(gf256 pol) {
    int i, andmask;
    for (i = 7; i >= 0;i--)
    {
        andmask = 1 << i;
        if(pol & andmask) {
            return i;
        }
    }
    return -1;
}

void gf256_showhex(gf256 pol) {
    char hex[] = "0123456789abcdef";
    printf("%c%c", hex[pol/16], hex[pol%16]);
}

void gf256_getstr(gf256 pol, char res[2]) {
    char hex[] = "0123456789abcdef";
    res[0] = hex[pol/16];
    res[1] = hex[pol%16];
}

gf256 gf256_parse(char input[2]) {
    return search_hexval(input[0])*16 + search_hexval(input[1]);
}

gf256 gf256_add(gf256 pol1, gf256 pol2) {
    return pol1 ^ pol2;
}

gf256 gf256_mulbyx(gf256 pol1) {
    if(pol1 & 128) {
        return 27 ^ (pol1 << 1);
    }
    else {
        return pol1 << 1;
    }
}

gf256 gf256_mulbyxpower(gf256 pol, int power) {
    gf256 res = power == 0 ? pol : gf256_mulbyxpower(gf256_mulbyx(pol), power - 1);
    return res;
}

gf256 gf256_mul(gf256 pol1, gf256 pol2) {
    int deg_b = gf256_deg(pol2);
    gf256 res = 0;
    for(int j = deg_b; j >= 0; j--) {
        if(pol2 & 1 << j) {
            res ^= gf256_mulbyxpower(pol1, j);
        }
    }
    return res;
}
