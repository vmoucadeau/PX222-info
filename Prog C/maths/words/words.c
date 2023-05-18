#include <stdio.h>
#include "words.h"
#include "../pol/pol.h"

void w4_showbin(w4 list) {
    printf("[");
    for(int i = 0; i < 4; i++) {
        gf256_showbin(list[i]);
        if(i < 3) {
            printf(", ");
        }
    }
    printf("]");
}

void w4_showhex(w4 list) {
    printf("[");
    for(int i = 0; i < 4; i++) {
        gf256_showhex(list[i]);
        if(i < 3) {
            printf(" ");
        }
    }
    printf("]");
}

void w4_showstr(w4 list) {
    for(int i = 0; i < 4; i++) {
        gf256_showhex(list[i]);
        
    }
}

void w4_showlst(w4 *list, int length) {
    for(int i = 0; i < length; i++) {
        printf("%d : [", i);
        for(int j = 0; j < 4; j++) {
            gf256_showhex(list[i][j]);
            j < 3 ? printf(" ") : printf("]\n");
        }
    }
}

void w4_getstr(w4 list, char res[8]) {
    for(int i = 0; i < 4; i++) {
        gf256_getstr(list[i], &res[i*2]);
    }
}

void w4_parse(char word[8], w4 res) {
    for(int i = 0; i < 8; i += 2) {
        char input[2] = {word[i], word[i+1]};
        res[i/2] = gf256_parse(input);
    }
}

void w4_copy(w4 a, w4 res) {
    for(int i = 0; i < 4; i++) {
        res[i] = a[i];
    }
}

void w4_add(w4 a, w4 b, w4 res) {
    for(int i = 0; i < 4; i++) {
        res[i] = a[i] ^ b[i];
    }
}

void w4_mul(w4 a, w4 b, w4 res) {
    gf256 d0 = gf256_mul(a[0], b[0]) ^ gf256_mul(a[3], b[1]) ^ gf256_mul(a[2], b[2]) ^ gf256_mul(a[1], b[3]);
    gf256 d1 = gf256_mul(a[1], b[0]) ^ gf256_mul(a[0], b[1]) ^ gf256_mul(a[3], b[2]) ^ gf256_mul(a[2], b[3]);
    gf256 d2 = gf256_mul(a[2], b[0]) ^ gf256_mul(a[1], b[1]) ^ gf256_mul(a[0], b[2]) ^ gf256_mul(a[3], b[3]);
    gf256 d3 = gf256_mul(a[3], b[0]) ^ gf256_mul(a[2], b[1]) ^ gf256_mul(a[1], b[2]) ^ gf256_mul(a[0], b[3]);
    res[0] = d0;
    res[1] = d1;
    res[2] = d2;
    res[3] = d3;
}