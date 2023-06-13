
#include "keyexp.h"
#include <stdio.h>
#include <string.h>

const unsigned char s_box[16][16] = SBOX_TABLE;
const unsigned char s_box_inv[16][16] = SBOX_INV_TABLE;

void subword(w4 input, w4 output) {
    for (int i = 0; i < 4; i++) {
        gf256 to_sub = input[i];
        int to_sub_x = to_sub >> 4;
        int to_sub_y = to_sub & 0x0F;
        output[i] = s_box[to_sub_x][to_sub_y];
    }
}

void rotword(w4 input, w4 output) {
    w4 rotating = W4_INIT;
    for (int i = 0; i < 4; i++) {
        rotating[i] = input[(i + 1) % 4];
    }
    w4_copy(rotating, output);
}

void rcon(int i, w4 output) {
    output[0] = gf256_mulbyxpower(1, i - 1);
    output[1] = 0;
    output[2] = 0;
    output[3] = 0;
}

void keyexpension(char *key, w4 *words) {
    int nK = (strlen(key)) / 8;
    int nR = 6 + nK;
    int key_length = (nR + 1) * nB;
    w4 tmp = W4_INIT;
    // Copie (et parsing) de la clé dans la future clé étendue
    for (int i = 0; i < 2 * nK; i += 2) {
        w4_parse(&key[4 * i], words[i / 2]);
    }

    // Expension de la clé
    for (int i = nK; i < key_length; i++) {
        w4_copy(words[i - 1], tmp);
        if (i % nK == 0) {
            rotword(tmp, tmp);
            subword(tmp, tmp);
            w4 my_rcon = W4_INIT;
            rcon(i / nK, my_rcon);
            w4_add(tmp, my_rcon, tmp);
        } else if (nK > 6 && i % nK == 4) {
            subword(tmp, tmp);
        }
        w4_add(words[i - nK], tmp, words[i]);
    }
}
