
#include "../../algorithm/cipher/cipher.h"

extern w4 *key_expended;

int aes_encrypt(char *data, int size, char *key, int keysize) {
    if (size % 16 != 0) return 1;
    if (keysize != 32 && keysize != 48 && keysize != 64) return 1;
    select_key(key, keysize);
    for (int i = 0; i < 1 + size / 16; i++) {
        if (i * 16 >= size) {
            break;
        }
        state ciphered = STATE_INIT;
        encode_block(&data[i * 16], key_expended, ciphered);
        state_concat(ciphered, &data[i * 16]);
    }
    return 0;
}

int aes_decrypt(char *data, int size, char *key, int keysize) {
    if (size % 16 != 0) return 1;
    if (keysize != 32 && keysize != 48 && keysize != 64) return 1;
    select_key(key, keysize);

    for (int i = 0; i < 1 + size / 16; i++) {
        if (i * 16 >= size) {
            break;
        }
        state ciphered = STATE_INIT;
        decode_block(&data[i * 16], key_expended, ciphered);
        state_concat(ciphered, &data[i * 16]);
    }
    return 0;
}
