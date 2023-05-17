#include <stdio.h>
#include "state.h"



void state_parse(char input[33], state res) {
    for(int i = 0; i < 4; i++) {
        w4 line = W4_INIT;
        w4_parse(&input[i*8], line);
        for(int j = 0; j < 4; j++) {
            res[i][j] = line[j];
        }
    }
}

void state_showhex(state list) {
    for(int i = 0; i < 4; i++) {
        printf("[");
        for(int j = 0; j < 4; j++) {
            gf256_showhex(list[j][i]);
            j < 3 ? printf(" ") : printf("]\n");
        }
    }
}

void state_showbin(state list) {
    for(int i = 0; i < 4; i++) {
        printf("[");
        for(int j = 0; j < 4; j++) {
            gf256_showbin(list[j][i]);
            j < 3 ? printf(" ") : printf("]\n");
        }
    }
}

void state_showstr(state list) {
    for(int i = 0; i < 4; i++) {
        w4_showstr(list[i]);
        
    }
    printf("\n");
}

void state_copy(state a, state res) {
    for(int i = 0; i < 4; i++) {
        w4_copy(a[i], res[i]);
    }
}


void state_add(state a, state b, state res) {
    for(int i = 0; i < 4; i++) {
        w4_add(a[i], b[i], res[i]);
    }
}

void state_mul(state a, state b, state res) {
    for(int i = 0; i < 4; i++) {
        w4_mul(a[i], b[i], res[i]);
    }
}