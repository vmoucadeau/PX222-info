
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "algorithm/cipher/cipher.h"
#include "plugins/api/api.h"
#include "plugins/bmp/bmp.h"
#include "plugins/files/files.h"
#include "plugins/text/text.h"

char testkeyexp1[] = "2b7e151628aed2a6abf7158809cf4f3c";
char testkeyexp2[] = "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b";                  // nK = 6
char testkeyexp3[] = "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4";  // nK = 8

char key_exvect1[] = "000102030405060708090a0b0c0d0e0f";
char key_exvect2[] = "000102030405060708090a0b0c0d0e0f1011121314151617";
char key_exvect3[] = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";

extern w4 *key_expended;
extern int keyexp_length;

int prog_io(int argc, char **argv) {
    if (argc < 4) {
        printf("Usage : ./aes.out encode/decode <key> <text>\n");
        return 0;
    } else {
        if (strcmp(argv[1], "encode") == 0) {
            if (strcmp(argv[2], "-b") == 0) {
                select_key(argv[3], strlen(argv[3]));
                state result = STATE_INIT;
                encode_block(argv[4], key_expended, result, result);
                char encoded[8 * nB + 1];
                state_getstr(result, encoded);
                printf("Encoded : %s\n", encoded);
                return 0;
            }
            char *encoded = malloc(32 + (strlen(argv[3]) / 16) * 32);
            encode_text(argv[2], argv[3], encoded);
            printf("Encoded : %s\n", encoded);
            free(encoded);
        } else if (strcmp(argv[1], "decode") == 0) {
            if (strcmp(argv[2], "-b") == 0) {
                select_key(argv[3], strlen(argv[3]));
                state result = STATE_INIT;
                decode_block(argv[4], key_expended, result, result);
                char decoded[8 * nB + 1];
                state_getstr(result, decoded);
                printf("Decoded : %s\n", decoded);
                return 0;
            }
            char *decoded = malloc(strlen(argv[3])+1);
            decode_text(argv[2], argv[3], decoded);
            printf("Decoded : %s\n", decoded);
            free(decoded);
        } else {
            printf("Usage : ./aes.out encode/decode <key> <text> \n");
            return 0;
        }
    }
    return 0;
}

/* TESTS POUR W4 */
void w4_test() {
    printf("---- W4 TEST ----\n");
    w4 ax = W4_INIT;
    w4 ax_inv = W4_INIT;
    w4_parse(AX_HEX, ax);
    w4_parse(AX_INV_HEX, ax_inv);
    w4 mul = W4_INIT;
    w4_mul(ax, ax_inv, mul);
    w4_showhex(mul);
    w4 one = {0x01, 0x00, 0x00, 0x00};
    assert(w4_isequal(mul, one));
    printf("\n---- W4 TEST OK ----\n\n");
}

/* TESTS POUR SUBBYTE */
void subbyte_test() {
    printf("---- SUBBYTE TEST ----\n");
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
    char subed_str[8 * nB + 1];
    state_getstr(subed, subed_str);
    assert(strcmp(subed_str, "fee44ccab704095947cabb5759266a92") == 0);
    assert(state_isequal(test, unsubed));
    printf("---- SUBBYTE TEST OK ----\n\n");
}

/* TESTS POUR SHIFTROWS */
void shiftrows_test() {
    printf("---- SHIFTROW TEST ----\n");
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
    char shifted_str[8 * nB + 1];
    state_getstr(shifted, shifted_str);
    assert(strcmp(shifted_str, "0c30fe742010581016235d1515ae40da") == 0);
    assert(state_isequal(test, unshifted));
    printf("---- SHIFTROW TEST OK ----\n\n");
}

/* TESTS POUR MIXCOLUMNS */
void mixcolumns_test() {
    printf("---- MIXCOLUMNS TEST ----\n");
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
    char mixed_str[8 * nB + 1];
    state_getstr(mixed, mixed_str);
    assert(strcmp(mixed_str, "bcbc28c74595af3a38f5947b63cf1aac") == 0);
    assert(state_isequal(test, unmixed));
    printf("---- MIXCOLUMNS TEST OK ----\n\n");
}

/* TESTS POUR RCON */
void rcon_test() {
    printf("---- RCON TEST ----\n");
    w4 test = W4_INIT;
    w4 rconlst[] = {{0x01, 0x00, 0x00, 0x00}, {0x02, 0x00, 0x00, 0x00}, {0x04, 0x00, 0x00, 0x00}, {0x08, 0x00, 0x00, 0x00}, {0x10, 0x00, 0x00, 0x00}, {0x20, 0x00, 0x00, 0x00}, {0x40, 0x00, 0x00, 0x00}, {0x80, 0x00, 0x00, 0x00}, {0x1b, 0x00, 0x00, 0x00}, {0x36, 0x00, 0x00, 0x00}};
    for (int i = 0; i < 10; i++)
    {
        rcon(i+1,test);
        w4_showhex(test);
        assert(w4_isequal(test, rconlst[i]));
    }
    printf("\n---- RCON TEST OK ----\n\n");
}

/* TESTS POUR KEYEXP */
void keyexp_test() {
    printf("---- KEYEXP TEST ----\n");
    select_key(testkeyexp1, strlen(testkeyexp1));
    w4_showlst(key_expended, keyexp_length);
    printf("---- KEYEXP TEST OK ----\n\n");
}

/* TEST POUR CIPHER ET INV_CIPHER */
void cipher_test() {
    printf("---- CIPHER TEST ----\n");
    char to_cipher[] = "3243f6a8885a308d313198a2e0370734";
    select_key(testkeyexp1, strlen(testkeyexp1));
    state ciphered = STATE_INIT;
    encode_blockhex(to_cipher, key_expended, ciphered);
    state_showhex(ciphered);
    printf("\n");
    char ciphered_str[8 * nB + 1] = {0};
    state_getstr(ciphered, ciphered_str);
    state unciphered = STATE_INIT;
    decode_blockhex(ciphered_str, key_expended, unciphered);
    state_showhex(unciphered);
    char uncipheredstr[8 * nB + 1] = {0};
    state_getstr(unciphered, uncipheredstr);
    assert(strcmp(to_cipher, uncipheredstr) == 0);
    assert(strcmp("3925841d02dc09fbdc118597196a0b32", ciphered_str) == 0);
    printf("---- CIPHER TEST OK ----\n\n");
}

/* TEST ENCODE/DECODE TEXT */
void encodetext_test() {
    printf("---- ENCODE TEXT TEST ----\n");
    // char toencode[] = "J'aime les pates aux basilic !";
    char toencode[] = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc eu venenatis nunc, sed maximus erat. Sed neque elit, facilisis non quam id, lacinia vehicula mauris. Vivamus eu bibendum enim. Sed risus nunc, venenatis at finibus nec, auctor eu libero. Nulla auctor blandit ex sed malesuada. Suspendisse vehicula, ipsum et lacinia varius, tellus nisl malesuada nisi, at vulputate ipsum odio eu mi. Proin vitae nibh eros. Nulla purus velit, commodo non ornare et, commodo vitae felis. Praesent egestas justo mauris, vel consequat tellus mollis ac. Vivamus mattis lacinia magna, mollis elementum justo dictum id. In varius nulla egestas quam pulvinar, eget gravida eros porta. Maecenas at orci diam. Curabitur pulvinar, leo vel ornare tincidunt, mauris nisi vehicula lorem, eu ullamcorper arcu arcu sed arcu. Pellentesque bibendum tortor sed sagittis feugiat. Etiam consequat mi vitae facilisis vestibulum. Sed cursus, nibh at tempus varius, dui tellus lobortis nulla, ut cursus nulla orci in arcu. Curabitur nisi magna, volutpat at dui ac, egestas fringilla magna. Praesent malesuada velit eros, sed egestas sem sagittis sit amet. Duis accumsan gravida euismod. Sed ipsum sem, interdum eget congue in, luctus eget justo. Curabitur ut lacinia erat, at pharetra nisl. Ut ullamcorper eu felis in scelerisque. Nullam in vulputate metus. Morbi pharetra, magna ultrices dignissim tempus, quam ligula dignissim enim, et viverra metus sem a nisl. Sed nec orci id tortor convallis varius vitae id odio. In quis condimentum nibh. Aliquam malesuada sed tortor non lacinia. Nulla imperdiet porta quam, eu pellentesque ante finibus eget. Nullam volutpat gravida lorem vitae fermentum. Proin sit amet ultrices dui. Pellentesque feugiat tempor lorem in ultricies. Sed commodo mattis quam ac vestibulum. Curabitur et rhoncus libero. Nam tortor ligula, sagittis quis massa dapibus, consectetur iaculis eros. Aliquam auctor faucibus arcu vel faucibus. Morbi eleifend accumsan massa, vitae convallis tellus fermentum eget. Vivamus lorem nunc, tincidunt non posuere non, sodales vitae neque. Nam nec augue condimentum erat fermentum facilisis eu id felis. Vestibulum facilisis leo leo, non facilisis lectus congue et. Maecenas et elit ut elit malesuada sodales. In malesuada rutrum ullamcorper. Integer cursus, urna eget imperdiet vestibulum, nisl tortor bibendum diam, nec maximus nulla lorem sit amet felis. Nam nec est id quam molestie volutpat. Phasellus ornare magna volutpat sem convallis commodo. Maecenas a mi eget tortor sagittis tempor sit amet eget nulla. Cras tincidunt posuere ipsum vitae laoreet. Quisque auctor blandit ipsum. Donec nisl massa, vulputate sit amet ornare sed, tincidunt sed turpis. Curabitur et blandit augue. Donec dictum ante in tortor feugiat, id egestas turpis eleifend. Aenean rutrum elementum sodales. Aliquam dui augue, porttitor nec turpis id, hendrerit maximus odio. Ut semper gravida ligula at pretium. Integer sed magna ut sem auctor tincidunt.";
    int length = strlen(toencode);
    char *encoded = malloc(33 + (length / 16) * 32);
    encode_text(key_exvect1, toencode, encoded);
    char *decoded = malloc(33 + (length / 16) * 32);
    decode_text(key_exvect1, encoded, decoded);
    printf("To encode: %s\n", toencode);
    printf("Ciphered: %s\n", encoded);
    printf("Unciphered: %s\n", decoded);
    assert(strcmp(toencode, decoded) == 0);
    free(encoded);
    free(decoded);
    printf("---- ENCODE TEXT TEST OK ----\n\n");
}

/* APPENDIX C - TESTS */
void appendix_c_test() {
    printf("---- APPENDIX C TEST ----\n");
    select_key(key_exvect3, strlen(key_exvect3));
    char to_cipher[] = "00112233445566778899aabbccddeeff";
    printf("\n");
    state ciphered = STATE_INIT;
    encode_blockhex(to_cipher, key_expended, ciphered);
    state_showstr(ciphered);
    char ciphered_str[8 * nB + 1] = {0};
    state_getstr(ciphered, ciphered_str);
    state unciphered = STATE_INIT;
    decode_blockhex(ciphered_str, key_expended, unciphered);
    state_showstr(unciphered);
    printf("\n");
    char uncipheredstr[8 * nB + 1] = {0};
    state_getstr(unciphered, uncipheredstr);
    assert(strcmp(to_cipher, uncipheredstr) == 0);
    printf("---- APPENDIX C TEST OK ----\n\n");
}

void api_test() {
    printf("---- API TEST ----\n");
    char toencrypt[] = "00112233445566778899aabbccddeeff";
    int testecb_enc = aes_encrypt(toencrypt, 32, key_exvect1, strlen(key_exvect1), 0);
    printf("ecb encrypt res: %d\n", testecb_enc);
    printf("testchar: %s\n", toencrypt);
    int testecb_dec = aes_decrypt(toencrypt, 32, key_exvect1, strlen(key_exvect1), 0);
    printf("ecb decrypt res: %d\n", testecb_dec);
    printf("testchar: %s\n", toencrypt);
    assert(testecb_enc == 0);
    assert(testecb_dec == 0);
    assert(strcmp(toencrypt, "00112233445566778899aabbccddeeff") == 0);

    int testcbc_enc = aes_encrypt(toencrypt, 32, key_exvect1, strlen(key_exvect1), 1);
    printf("cbc encrypt res: %d\n", testcbc_enc);
    printf("testchar: %s\n", toencrypt);
    int testcbc_dec = aes_decrypt(toencrypt, 32, key_exvect1, strlen(key_exvect1), 1);
    printf("cbc decrypt res: %d\n", testcbc_dec);
    printf("testchar: %s\n", toencrypt);
    assert(testcbc_enc == 0);
    assert(testcbc_dec == 0);
    assert(strcmp(toencrypt, "00112233445566778899aabbccddeeff") == 0);
    printf("---- API TEST OK ----\n\n");
}

void bmp_test(char *key) {
    printf("---- BMP TEST ----\n");
    encode_bmp(key, "monfichier.bmp", "monfichierECBciph.bmp", 0);
    decode_bmp(key, "monfichierECBciph.bmp", "monfichierECBunciph.bmp", 0);
    encode_bmp(key, "monfichier.bmp", "monfichierCBCciph.bmp", 1);
    decode_bmp(key, "monfichierCBCciph.bmp", "monfichierCBCunciph.bmp", 1);
    printf("---- BMP TEST OK ----\n\n");
}

void file_test() {
    printf("---- FILE TEST ----\n");
    encode_file(key_exvect1, "monfichier.txt", "monfichierciph.txt", 0);
    decode_file(key_exvect1, "monfichierciph.txt", "monfichierunciph.txt", 0);
    printf("---- FILE TEST OK ----\n\n");
}

void testall() {
    printf("------ TEST ALL ------\n\n");
    w4_test();
    subbyte_test();
    shiftrows_test();
    mixcolumns_test();
    rcon_test();
    keyexp_test();
    cipher_test();
    encodetext_test();
    appendix_c_test();
    api_test();
}

int main() {
    // file_test();
    file_entropy("bitmap_original.bmp");
    file_entropy("bitmap_ciphered_ecb192.bmp");
    file_entropy("bitmap_ciphered_cbc192.bmp");

    return 0;
}
