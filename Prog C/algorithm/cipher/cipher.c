
#include "cipher.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const unsigned char sbox[16][16] = SBOX_TABLE;
const unsigned char sbox_inv[16][16] = SBOX_INV_TABLE;

int nK;
int nR;
int keyexp_length;

w4 *key_expended;

void select_key(char *key, int keysize) {
    free(key_expended);
    nK = keysize/8;
    nR = 6 + nK;
    keyexp_length = nB * (nR+1);
    key_expended = malloc(sizeof(w4)*keyexp_length);
    keyexpansion(key, key_expended);
}

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
        w4 ax = AX;
        // w4_parse(AX_HEX, ax); // useless
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
        w4 ax_inv = AX_INV;
        // w4_parse(AX_INV_HEX, ax_inv);
        w4_mul(ax_inv, tmp, output[i]);
    }
}

void cipher(state input, state output, w4 *key_expended) {
    state ciphering = STATE_INIT;
    state_copy(input, ciphering);
    
    int Nb = nB;
    nR = 6+nK;
    addroundkey(ciphering, key_expended, ciphering);
    for(int r = 1; r < nR; r++) {
        subbytes(ciphering, ciphering);
        shiftrows(ciphering, ciphering);
        mixcolumns(ciphering, ciphering);
        addroundkey(ciphering, &key_expended[r*Nb], ciphering);
    }
    subbytes(ciphering, ciphering);
    shiftrows(ciphering, ciphering);
    addroundkey(ciphering, &key_expended[Nb*nR], ciphering);
    state_copy(ciphering, output);
}

void inv_cipher(state input, state output, w4 *key_expended) {
    state unciphering = STATE_INIT;
    state_copy(input, unciphering);
    int Nb = nB;
    int Nr = 6+nK;
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

void encode_blockhex(char bloc[8*nB], w4 *key_expended, state output) {
    state to_cipher = STATE_INIT;
    state_parse(bloc, to_cipher);
    cipher(to_cipher, output, key_expended);
}

void decode_blockhex(char bloc[8*nB], w4 *key_expended, state output) {
    state to_uncipher = STATE_INIT;
    state_parse(bloc, to_uncipher);
    inv_cipher(to_uncipher, output, key_expended);
}

void encode_block(char bloc[4*nB], w4 *key_expended, state output, state previous) {
    state to_cipher = STATE_INIT;
    for(int i = 0; i < 4; i++) {
        for(int j = 0; j < nB; j++) {
            to_cipher[i][j] = bloc[i*nB+j];
        }
    }
    state_add(to_cipher, previous, to_cipher);
    cipher(to_cipher, output, key_expended);
}

void decode_block(char bloc[4*nB], w4 *key_expended, state output, state previous) {
    state to_uncipher = STATE_INIT;
    state_init(bloc, to_uncipher);
    inv_cipher(to_uncipher, output, key_expended);
    state_add(output, previous, output);
}
