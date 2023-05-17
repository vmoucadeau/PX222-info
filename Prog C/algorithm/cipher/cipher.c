#include "cipher.h"

const unsigned char sbox[16][16] = SBOX_TABLE;
const unsigned char sbox_inv[16][16] = SBOX_INV_TABLE;

void subbytes(state input, state output) {
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++) {
            gf256 to_sub = input[i][j];
            int to_sub_x = to_sub >> 4;
            int to_sub_y = to_sub & 0x0F;
            output[i][j] = sbox[to_sub_x][to_sub_y];       
        }
    }
}

void shiftrows(state input, state output) {
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++){
            output[j][i] = input[(j+i)%4][i];
        }
    }
}

void mixcolumns(state input, state output) {
    for(int i = 0; i < 4; i++) {
        w4 tmp = W4_INIT;
        w4_copy(input[i], tmp);
        w4 ax = W4_INIT;
        w4_parse(AX_HEX, ax);
        w4_mul(ax, tmp, output[i]);
    }
}

void addroundkey(state input, state key, state output) {
    state_add(input, key, output);
}

void inv_subbytes(state input, state output) {
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++) { 
            gf256 to_sub = input[i][j];
            int to_sub_x = to_sub >> 4;
            int to_sub_y = to_sub & 0x0F;
            output[i][j] = sbox_inv[to_sub_x][to_sub_y];       
        }
    }
}

void inv_shiftrows(state input, state output) {
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++){
            output[j][i] = input[(j-i+4)%4][i];
        }
    }
}

void inv_mixcolumns(state input, state output) {
    for(int i = 0; i < 4; i++) {
        w4 tmp = W4_INIT;
        w4_copy(input[i], tmp);
        w4 ax_inv = W4_INIT;
        w4_parse(AX_INV_HEX, ax_inv);
        w4_mul(ax_inv, tmp, output[i]);
    }
}