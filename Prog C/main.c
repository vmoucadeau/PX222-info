char test_pol = 65;

#include "maths/bits/bits.h"
#include "maths/pol/pol.h"
#include "maths/words/words.h"
#include "algorithm/state/state.h"
#include "algorithm/cipher/cipher.h"
#include "algorithm/keyexp/keyexp.h"

#include <stdio.h>
#include <stdlib.h>

void test_gf256_add() {
    gf256 test_pol = gf256_parse("1d");
    gf256_showhex(gf256_add(test_pol, test_pol)); // 1d xor 1d = 00
    gf256 test_pol2 = gf256_parse("1e");
    gf256_showhex(gf256_add(test_pol, test_pol2)); // 1d xor 1e = 03
}

int main() {
    
    
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