#include "algorithm/cipher/cipher.h"
#include "plugins/text/text.h"
#include "plugins/files/files.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>



char testkeyexp1[] = "2b7e151628aed2a6abf7158809cf4f3c";
char testkeyexp2[] = "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"; // nK = 6
char testkeyexp3[] = "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"; // nK = 8

char key_exvect1[] = "000102030405060708090a0b0c0d0e0f";
char key_exvect2[] = "000102030405060708090a0b0c0d0e0f1011121314151617";
char key_exvect3[] = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";

extern w4 *key_expended;
extern int keyexp_length;

int prog_io(int argc, char **argv) {
    if(argc < 4) {
        printf("Usage : ./aes.out encode/decode <key> <text>\n");
        return 0;
    }
    else {
        if(strcmp(argv[1], "encode") == 0) {
            if(strcmp(argv[2], "-b") == 0) {
                select_key(argv[3]);
                state result = STATE_INIT;
                encode_block(argv[4], key_expended, result);
                char *encoded = malloc(32);
                state_getstr(result, encoded);
                printf("Encoded : %s\n", encoded);
                free(encoded);
                return 0;
            }
            char *encoded = encode_text(argv[2], argv[3]);
            printf("Encoded : %s\n", encoded);
            free(encoded);
        }
        else if(strcmp(argv[1], "decode") == 0) {
            if(strcmp(argv[2], "-b") == 0) {
                select_key(argv[3]);
                state result = STATE_INIT;
                decode_block(argv[4], key_expended, result);
                char *decoded = malloc(32);
                state_getstr(result, decoded);
                printf("Decoded : %s\n", decoded);
                free(decoded);
                return 0;
            }
            char *decoded = decode_text(argv[2], argv[3]);
            printf("Decoded : %s\n", decoded);
            free(decoded);
        }
        else {
            printf("Usage : ./aes.out encode/decode <key> <text> \n");
            return 0;
        }
    }
    return 0;
}



/* TESTS POUR W4 */
void w4_test() {
    w4 ax = W4_INIT;
    w4 ax_inv = W4_INIT;
    w4_parse(AX_HEX, ax);
    w4_parse(AX_INV_HEX, ax_inv);
    w4 mul = W4_INIT;
    w4_mul(ax, ax_inv, mul);
    w4_showhex(mul);
}

/* TESTS POUR SUBBYTE */
void subbyte_test() {
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
}

/* TESTS POUR SHIFTROWS */
void shiftrows_test() {
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
}

/* TESTS POUR MIXCOLUMNS */
void mixcolumns_test() {
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
}

/* TESTS POUR RCON */
void rcon_test() {
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
}

/* TESTS POUR KEYEXP */
void keyexp_test() {
    select_key(testkeyexp1);
    w4_showlst(key_expended, keyexp_length);
}

/* TEST POUR CIPHER ET INV_CIPHER */
void cipher_test() {
    char to_cipher[] = "3243f6a8885a308d313198a2e0370734";
    select_key(testkeyexp1);
    state ciphered = STATE_INIT;
    encode_block(to_cipher, key_expended, ciphered);
    state_showhex(ciphered);
    printf("\n");
    char ciphered_str[8*nB] = {0};
    state_getstr(ciphered, ciphered_str);
    state unciphered = STATE_INIT;
    decode_block(ciphered_str, key_expended, unciphered);
    state_showhex(unciphered);
}


/* TEST ENCODE/DECODE TEXT */
void encodetext_test() {
    // char toencode[] = "J'aime les pates aux basilic !";
    char toencode[] = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc eu venenatis nunc, sed maximus erat. Sed neque elit, facilisis non quam id, lacinia vehicula mauris. Vivamus eu bibendum enim. Sed risus nunc, venenatis at finibus nec, auctor eu libero. Nulla auctor blandit ex sed malesuada. Suspendisse vehicula, ipsum et lacinia varius, tellus nisl malesuada nisi, at vulputate ipsum odio eu mi. Proin vitae nibh eros. Nulla purus velit, commodo non ornare et, commodo vitae felis. Praesent egestas justo mauris, vel consequat tellus mollis ac. Vivamus mattis lacinia magna, mollis elementum justo dictum id. In varius nulla egestas quam pulvinar, eget gravida eros porta. Maecenas at orci diam. Curabitur pulvinar, leo vel ornare tincidunt, mauris nisi vehicula lorem, eu ullamcorper arcu arcu sed arcu. Pellentesque bibendum tortor sed sagittis feugiat. Etiam consequat mi vitae facilisis vestibulum. Sed cursus, nibh at tempus varius, dui tellus lobortis nulla, ut cursus nulla orci in arcu. Curabitur nisi magna, volutpat at dui ac, egestas fringilla magna. Praesent malesuada velit eros, sed egestas sem sagittis sit amet. Duis accumsan gravida euismod. Sed ipsum sem, interdum eget congue in, luctus eget justo. Curabitur ut lacinia erat, at pharetra nisl. Ut ullamcorper eu felis in scelerisque. Nullam in vulputate metus. Morbi pharetra, magna ultrices dignissim tempus, quam ligula dignissim enim, et viverra metus sem a nisl. Sed nec orci id tortor convallis varius vitae id odio. In quis condimentum nibh. Aliquam malesuada sed tortor non lacinia. Nulla imperdiet porta quam, eu pellentesque ante finibus eget. Nullam volutpat gravida lorem vitae fermentum. Proin sit amet ultrices dui. Pellentesque feugiat tempor lorem in ultricies. Sed commodo mattis quam ac vestibulum. Curabitur et rhoncus libero. Nam tortor ligula, sagittis quis massa dapibus, consectetur iaculis eros. Aliquam auctor faucibus arcu vel faucibus. Morbi eleifend accumsan massa, vitae convallis tellus fermentum eget. Vivamus lorem nunc, tincidunt non posuere non, sodales vitae neque. Nam nec augue condimentum erat fermentum facilisis eu id felis. Vestibulum facilisis leo leo, non facilisis lectus congue et. Maecenas et elit ut elit malesuada sodales. In malesuada rutrum ullamcorper. Integer cursus, urna eget imperdiet vestibulum, nisl tortor bibendum diam, nec maximus nulla lorem sit amet felis. Nam nec est id quam molestie volutpat. Phasellus ornare magna volutpat sem convallis commodo. Maecenas a mi eget tortor sagittis tempor sit amet eget nulla. Cras tincidunt posuere ipsum vitae laoreet. Quisque auctor blandit ipsum. Donec nisl massa, vulputate sit amet ornare sed, tincidunt sed turpis. Curabitur et blandit augue. Donec dictum ante in tortor feugiat, id egestas turpis eleifend. Aenean rutrum elementum sodales. Aliquam dui augue, porttitor nec turpis id, hendrerit maximus odio. Ut semper gravida ligula at pretium. Integer sed magna ut sem auctor tincidunt.";
    char *encoded = encode_text(key_exvect1, toencode);

    char *decoded = decode_text(key_exvect1, encoded);

    printf("To encode: %s\n", toencode);
    printf("Ciphered: %s\n", encoded);
    printf("Unciphered: %s\n", decoded);

    free(encoded);
    free(decoded);
}

/* APPENDIX C - TESTS */
void test_appendix_c() {
    select_key(key_exvect3);
    char to_cipher[] = "00112233445566778899aabbccddeeff";
    
    state ciphered = STATE_INIT;
    encode_blockstr(to_cipher, key_expended, ciphered);
    state_showstr(ciphered);
    printf("\n");
    char ciphered_str[8*nB] = {0};
    state_getstr(ciphered, ciphered_str);
    state unciphered = STATE_INIT;
    decode_blockstr(ciphered_str, key_expended, unciphered);
    state_showstr(unciphered);
}

void testall() {
    keyexp_test();
    cipher_test();
    encodetext_test();
    test_appendix_c();
}

int main() {
    encode_file(key_exvect1, "monfichier.txt", "ciphered.txt");
    decode_file(key_exvect1, "ciphered.txt", "unciphered.txt");
    return 0;
}