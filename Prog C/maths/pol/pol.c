
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
    // switch (pol1)
    // {
    // case 0x01:
    //     return gf256_table01[pol2];
    //     break;
    
    // default:
    //     break;
    // }


    int deg_b = gf256_deg(pol2);
    gf256 res = 0;
    for(int j = deg_b; j >= 0; j--) {
        if(pol2 & 1 << j) {
            res ^= gf256_mulbyxpower(pol1, j);
        }
    }
    return res;
}
