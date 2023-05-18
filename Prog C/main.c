#include "maths/bits/bits.h"
#include "maths/pol/pol.h"
#include "maths/words/words.h"
#include "algorithm/state/state.h"
#include "algorithm/cipher/cipher.h"
#include "algorithm/keyexp/keyexp.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void encodebloc(char bloc[8*nB], w4 key_expended[nB*(nR+1)], state output) {
    state to_cipher = STATE_INIT;
    state_parse(bloc, to_cipher);
    cipher(to_cipher, output, key_expended);
}

void decodebloc(char bloc[8*nB], w4 key_expended[nB*(nR+1)], state output) {
    state to_uncipher = STATE_INIT;
    state_parse(bloc, to_uncipher);
    inv_cipher(to_uncipher, output, key_expended);
}

void encodetext(char *text, int length, char *key, char *output) {
    char hex[] = "0123456789abcdef";
    w4 key_expended[nB*(nR+1)] = {W4_INIT};
    keyexpension(key, key_expended);
    
    char *texthex = malloc(length*2);
    for(int i = 0; i < length*2; i+=2) {
        texthex[i] = hex[text[i/2] >> 4];
        texthex[i+1] = hex[text[i/2] & 0x0F];
    }

    char *textcipherhex = malloc(32+(length/16)*32);
    for(int i = 0; i < 1+length/16; i++) {
        state ciphered = STATE_INIT;
        if((length*2 - i*32) <= 16) {
            char notfullbloc[32] = {0};
            for(int j = 0; j < (length*2 - i*32); j++) {
                notfullbloc[j] = texthex[i*32+j];
            }
            encodebloc(notfullbloc, key_expended, ciphered);
        }
        else {
            encodebloc(&texthex[i*32], key_expended, ciphered);   
        }    
        state_getstr(ciphered, &textcipherhex[i*32]);
    }

    // for(int i = 0; i < (32+(length/16)*32); i+=2) {
    //     char charval = search_hexval(textcipherhex[i])*16 + search_hexval(textcipherhex[i]);
    //     printf("%c", charval);
    //     output[i/2] = charval;
    // }
    strcpy(output, textcipherhex);
    free(textcipherhex);
    free(texthex);
}


int main() {
    // char to_cipher[] = "3243f6a8885a308d313198a2e0370734";
    char testkey1[] = "2b7e151628aed2a6abf7158809cf4f3c";
    char toencode[18] = "azerthgbvfgthj5658";
    char encoded[32*3] = {0};
    encodetext(toencode, 18, testkey1, encoded);
    printf("%s\n", toencode);
    printf("%s", encoded);
}

/* TESTS POUR W4 */
/*
    w4 ax = W4_INIT;
    w4 ax_inv = W4_INIT;
    w4_parse(AX_HEX, ax);
    w4_parse(AX_INV_HEX, ax_inv);
    w4 mul = W4_INIT;
    w4_mul(ax, ax_inv, mul);
    w4_showhex(mul);
*/

/* TESTS POUR SUBBYTE */
/*
    state test = STATE_INIT;
    state_parse("0cae5d10203040151610feda15235874", test);
    state subed = STATE_INIT;
    subbytes(test, subed);
    state unsubed = STATE_INIT;
    inv_subbytes(subed, unsubed);
    state_showhex(test);
    printf("\n");
    state_showhex(subed);
    printf("\n");
    state_showhex(unsubed);
*/

/* TESTS POUR SHIFTROWS */
/*
    state test = STATE_INIT;
    state_parse("0cae5d10203040151610feda15235874", test);
    state shifted = STATE_INIT;
    shiftrows(test, shifted);
    state unshifted = STATE_INIT;
    inv_shiftrows(shifted, unshifted);
    state_showhex(test);
    printf("\n");
    state_showhex(shifted);
    printf("\n");
    state_showhex(unshifted);
*/

/* TESTS POUR MIXCOLUMNS */
/*
    state test = STATE_INIT;
    state_parse("0cae5d10203040151610feda15235874", test);
    state mixed = STATE_INIT;
    mixcolumns(test, mixed);
    state unmixed = STATE_INIT;
    inv_mixcolumns(mixed, unmixed);
    state_showhex(test);
    printf("\n");
    state_showhex(mixed);
    printf("\n");
    state_showhex(unmixed);
*/

/* TESTS POUR RCON */
/*
    w4 test = W4_INIT;
    rcon(1, test);
    w4_showhex(test);
    rcon(2, test);
    w4_showhex(test);
    rcon(3, test);
    w4_showhex(test);
    rcon(4, test);
    w4_showhex(test);
    rcon(5, test);
    w4_showhex(test);
    rcon(6, test);
    w4_showhex(test);
    rcon(7, test);
    w4_showhex(test);
    rcon(8, test);
    w4_showhex(test);
    rcon(9, test);
    w4_showhex(test);
    rcon(10, test);
    w4_showhex(test);
*/

/* TESTS POUR KEYEXP */
/*
    char testkey1[] = "2b7e151628aed2a6abf7158809cf4f3c"; // nK = 4
    char testkey2[] = "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"; // nK = 6
    char testkey3[] = "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"; // nK = 8
    w4 words[KEY_LENGTH] = {W4_INIT};
    keyexpension(testkey1, words);
    w4_showlst(words, KEY_LENGTH);
*/

/* TEST POUR CIPHER ET INV_CIPHER */
/*
    char to_cipher[] = "3243f6a8885a308d313198a2e0370734";
    char testkey1[] = "2b7e151628aed2a6abf7158809cf4f3c";
    w4 expended[nB*(nR+1)] = {W4_INIT};
    keyexpension(testkey1, expended);
    state ciphered = STATE_INIT;
    encodebloc(to_cipher, expended, ciphered);
    state_showhex(ciphered);
    printf("\n");
    char ciphered_str[8*nB] = {0};
    state_getstr(ciphered, ciphered_str);
    state unciphered = STATE_INIT;
    decodebloc(ciphered_str, expended, unciphered);
    state_showhex(unciphered);
*/