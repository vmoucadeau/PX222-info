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

char *encodetext(char *key, char *text) {
    char hex[] = "0123456789abcdef";
    w4 key_expended[nB*(nR+1)] = {W4_INIT};
    keyexpension(key, key_expended);

    int length = strlen(text);
    
    char *texthex = malloc(length*2);
    for(int i = 0; i < length*2; i+=2) {
        texthex[i] = hex[text[i/2] >> 4];
        texthex[i+1] = hex[text[i/2] & 0x0F];
    }

    char *output = malloc(32+(length/16)*32);
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
        state_getstr(ciphered, &output[i*32]);
    }
    free(texthex);
    return output;
}

char *decodetext(char *key, char *text) {
    char hex[] = "0123456789abcdef";
    w4 key_expended[nB*(nR+1)] = {W4_INIT};
    keyexpension(key, key_expended);
    int length = strlen(text);

    char *textuncipherhex = malloc(length);
    for(int i = 0; i < length/32; i++) {
        state unciphered = STATE_INIT;
        if((length - i*32) <= 32) {
            char notfullbloc[32] = {0};
            for(int j = 0; j < (length - i*32); j++) {
                notfullbloc[j] = text[i*32+j];
            }
            decodebloc(notfullbloc, key_expended, unciphered);
        }
        else {
            decodebloc(&text[i*32], key_expended, unciphered);   
        }    
        state_getstr(unciphered, &textuncipherhex[i*32]);
    }

    int truelength = 1;
    for(int i = 0; i < length-1; i++) {
        if(textuncipherhex[i] == '0' && textuncipherhex[i+1] == '0') {
            truelength = i/2+1;
            break;
        }
    }
    char *textuncipher = malloc(truelength);
    for(int i = 0; i < (32+(length/16)*32); i+=2) {
        char charval = search_hexval(textuncipherhex[i])*16 + search_hexval(textuncipherhex[i+1]);
        textuncipher[i/2] = charval;
        if(textuncipherhex[i] == '0' && textuncipherhex[i+1] == '0') break;
    }
    // printf("Hex unciphered : %s\n", textuncipherhex);
    free(textuncipherhex);
    return textuncipher;
}


int main() {
    char testkey1[] = "2b7e151628aed2a6abf7158809cf4f3c";
    char testkey2[] = "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"; // nK = 6
    char testkey3[] = "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"; // nK = 8
    char toencode[] = "J'aime les pates aux basilic !";
    // char toencode[] = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc eu venenatis nunc, sed maximus erat. Sed neque elit, facilisis non quam id, lacinia vehicula mauris. Vivamus eu bibendum enim. Sed risus nunc, venenatis at finibus nec, auctor eu libero. Nulla auctor blandit ex sed malesuada. Suspendisse vehicula, ipsum et lacinia varius, tellus nisl malesuada nisi, at vulputate ipsum odio eu mi. Proin vitae nibh eros. Nulla purus velit, commodo non ornare et, commodo vitae felis. Praesent egestas justo mauris, vel consequat tellus mollis ac. Vivamus mattis lacinia magna, mollis elementum justo dictum id. In varius nulla egestas quam pulvinar, eget gravida eros porta. Maecenas at orci diam. Curabitur pulvinar, leo vel ornare tincidunt, mauris nisi vehicula lorem, eu ullamcorper arcu arcu sed arcu. Pellentesque bibendum tortor sed sagittis feugiat. Etiam consequat mi vitae facilisis vestibulum. Sed cursus, nibh at tempus varius, dui tellus lobortis nulla, ut cursus nulla orci in arcu. Curabitur nisi magna, volutpat at dui ac, egestas fringilla magna. Praesent malesuada velit eros, sed egestas sem sagittis sit amet. Duis accumsan gravida euismod. Sed ipsum sem, interdum eget congue in, luctus eget justo. Curabitur ut lacinia erat, at pharetra nisl. Ut ullamcorper eu felis in scelerisque. Nullam in vulputate metus. Morbi pharetra, magna ultrices dignissim tempus, quam ligula dignissim enim, et viverra metus sem a nisl. Sed nec orci id tortor convallis varius vitae id odio. In quis condimentum nibh. Aliquam malesuada sed tortor non lacinia. Nulla imperdiet porta quam, eu pellentesque ante finibus eget. Nullam volutpat gravida lorem vitae fermentum. Proin sit amet ultrices dui. Pellentesque feugiat tempor lorem in ultricies. Sed commodo mattis quam ac vestibulum. Curabitur et rhoncus libero. Nam tortor ligula, sagittis quis massa dapibus, consectetur iaculis eros. Aliquam auctor faucibus arcu vel faucibus. Morbi eleifend accumsan massa, vitae convallis tellus fermentum eget. Vivamus lorem nunc, tincidunt non posuere non, sodales vitae neque. Nam nec augue condimentum erat fermentum facilisis eu id felis. Vestibulum facilisis leo leo, non facilisis lectus congue et. Maecenas et elit ut elit malesuada sodales. In malesuada rutrum ullamcorper. Integer cursus, urna eget imperdiet vestibulum, nisl tortor bibendum diam, nec maximus nulla lorem sit amet felis. Nam nec est id quam molestie volutpat. Phasellus ornare magna volutpat sem convallis commodo. Maecenas a mi eget tortor sagittis tempor sit amet eget nulla. Cras tincidunt posuere ipsum vitae laoreet. Quisque auctor blandit ipsum. Donec nisl massa, vulputate sit amet ornare sed, tincidunt sed turpis. Curabitur et blandit augue. Donec dictum ante in tortor feugiat, id egestas turpis eleifend. Aenean rutrum elementum sodales. Aliquam dui augue, porttitor nec turpis id, hendrerit maximus odio. Ut semper gravida ligula at pretium. Integer sed magna ut sem auctor tincidunt.";
    char *encoded = encodetext(testkey1, toencode);

    char *decoded = decodetext(testkey1, encoded);

    printf("To encode: %s\n", toencode);
    printf("Ciphered: %s\n", encoded);
    printf("Unciphered: %s\n", decoded);

    free(encoded);
    free(decoded);
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

/* TEST POUR ENCODE ET DECODE */
/*
    char testkey1[] = "2b7e151628aed2a6abf7158809cf4f3c";
    char toencode[] = "azerthgbvfgthj5658";
    char encoded[65] = {0};
    encodetext(toencode, strlen(toencode), testkey1, encoded);
    char todecode[] = "068743c462d1927dd97749337cd504c27026d00c69e394819b7bf36d948732d8";
    char decoded[65];
    decodetext(todecode, 65, testkey1, decoded);
    printf("%s\n", toencode);
    printf("%s\n", encoded);
    printf("%s\n", decoded);
*/