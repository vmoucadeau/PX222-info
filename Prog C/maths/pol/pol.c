#include "../bits/bits.h"
#include <stdio.h>
char hexa[] = "0123456789abcdef";

void show_bin_pol(char pol) {
    int i, k, andmask;
    for (i = 7; i >= 0;i--)
    {
        andmask = 1 << i;
        k = pol & andmask;
        k == 0 ? printf ("0") : printf ("1");
    }
}

void show_hex_pol(char pol) {
    printf("%c%c", hexa[pol / 16], hexa[pol % 16]);
}

char parse_pol(char input[2]) {
    return search_hexval(input[0])*16 + search_hexval(input[1]);
}

int *pol_clean(int *pol) {
    size_t length = sizeof(pol)/sizeof(pol[0]);
    for(int i = 0; i < length; i++) {
        if(pol[i] == 0) {
            continue;
        }

    }
}

int pol_deg(int *pol1) {

}

int *pol_add(int *pol1, int *pol2, int *res) {

}

char gf256_add(char pol1, char pol2) {
    return pol1 ^ pol2;
}

char pol_mul(char pol1, char pol2) {
    
}