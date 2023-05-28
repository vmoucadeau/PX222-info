#include "cipher.h"
#include <stdio.h>

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
    state temp = STATE_INIT;
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++){
            temp[j][i] = input[(j+i)%4][i];
        }
    }
    state_copy(temp, output);
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

void addroundkey(state input, w4 words[nB], state output) {
    state words_in_state = STATE_INIT;
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++) {
            words_in_state[i][j] = words[i][j];
        }
    }
    state_add(input, words_in_state, output);
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
    state temp = STATE_INIT;
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++){
            temp[j][i] = input[(j-i+4)%4][i];
        }
    }
    state_copy(temp, output);
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

void cipher(state input, state output, w4 key_expended[nB*(nR+1)]) {
    state ciphering = STATE_INIT;
    state_copy(input, ciphering);
    
    int Nb = nB;
    int Nr = nR;
    addroundkey(ciphering, key_expended, ciphering);
    
    for(int r = 1; r < Nr; r++) {
        subbytes(ciphering, ciphering);
        shiftrows(ciphering, ciphering);
        mixcolumns(ciphering, ciphering);
        addroundkey(ciphering, &key_expended[r*Nb], ciphering);
    }
    subbytes(ciphering, ciphering);
    shiftrows(ciphering, ciphering);
    addroundkey(ciphering, &key_expended[Nb*Nr], ciphering);
    state_copy(ciphering, output);
}

void inv_cipher(state input, state output, w4 key_expended[KEY_LENGTH]) {
    state unciphering = STATE_INIT;
    state_copy(input, unciphering);
    int Nb = nB;
    int Nr = nR;
    addroundkey(unciphering, &key_expended[Nb*Nr], unciphering);
    for(int r = Nr-1; r >= 1; r--) {
        inv_shiftrows(unciphering, unciphering);
        inv_subbytes(unciphering, unciphering);
        addroundkey(unciphering, &key_expended[r*Nb], unciphering);
        inv_mixcolumns(unciphering, unciphering);
    }
    inv_shiftrows(unciphering, unciphering);
    inv_subbytes(unciphering, unciphering);
    addroundkey(unciphering, key_expended, unciphering);
    state_copy(unciphering, output);
}