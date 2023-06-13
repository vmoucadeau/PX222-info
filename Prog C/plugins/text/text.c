
#include <stdlib.h>
#include <string.h>

#include "../../algorithm/cipher/cipher.h"

extern w4 *key_expended;

char *encode_text(char *key, char *text) {
    char hex[] = "0123456789abcdef";
    select_key(key, strlen(key));
    int length = strlen(text);
    char *texthex = malloc(length * 2);
    for (int i = 0; i < length * 2; i += 2) {
        texthex[i] = hex[text[i / 2] >> 4];
        texthex[i + 1] = hex[text[i / 2] & 0x0F];
    }
    char *output = malloc(32 + (length / 16) * 32);
    for (int i = 0; i < 1 + length / 16; i++) {
        state ciphered = STATE_INIT;
        if ((length * 2 - i * 32) <= 16) {
            char notfullbloc[32] = {0};
            for (int j = 0; j < (length * 2 - i * 32); j++) {
                notfullbloc[j] = texthex[i * 32 + j];
            }
            encode_blockhex(notfullbloc, key_expended, ciphered);
        } else {
            encode_blockhex(&texthex[i * 32], key_expended, ciphered);
        }
        state_getstr(ciphered, &output[i * 32]);
    }
    free(texthex);
    return output;
}

char *decode_text(char *key, char *text) {
    select_key(key, strlen(key));
    int length = strlen(text);
    char *textuncipherhex = malloc(length);
    for (int i = 0; i < length / 32; i++) {
        state unciphered = STATE_INIT;
        if ((length - i * 32) <= 32) {
            char notfullbloc[32] = {0};
            for (int j = 0; j < (length - i * 32); j++) {
                notfullbloc[j] = text[i * 32 + j];
            }
            decode_blockhex(notfullbloc, key_expended, unciphered);
        } else {
            decode_blockhex(&text[i * 32], key_expended, unciphered);
        }
        state_getstr(unciphered, &textuncipherhex[i * 32]);
    }
    int truelength = 1;
    for (int i = 0; i < length - 1; i++) {
        if (textuncipherhex[i] == '0' && textuncipherhex[i + 1] == '0') {
            truelength = i / 2 + 1;
            break;
        }
    }
    char *textuncipher = malloc(truelength);
    for (int i = 0; i < (32 + (length / 16) * 32); i += 2) {
        char charval = search_hexval(textuncipherhex[i]) * 16 + search_hexval(textuncipherhex[i + 1]);
        textuncipher[i / 2] = charval;
        if (textuncipherhex[i] == '0' && textuncipherhex[i + 1] == '0') break;
    }
    // printf("Hex unciphered : %s\n", textuncipherhex);
    free(textuncipherhex);
    return textuncipher;
}
