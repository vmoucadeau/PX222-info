char test_pol = 65;

#include "maths/bits/bits.h"
#include "maths/pol/pol.h"
#include "maths/words/words.h"

#include <stdio.h>
#include <stdlib.h>

void test_gf256_add() {
    gf256 test_pol = gf256_parse("1d");
    gf256_showhex(gf256_add(test_pol, test_pol)); // 1d xor 1d = 00
    gf256 test_pol2 = gf256_parse("1e");
    gf256_showhex(gf256_add(test_pol, test_pol2)); // 1d xor 1e = 03
}

int main() {
    w4 mot1 = {gf256_parse("02"), gf256_parse("01"), gf256_parse("01"), gf256_parse("03")};
    w4 mot2 = {gf256_parse("0e"), gf256_parse("09"), gf256_parse("0d"), gf256_parse("0b")};
    w4 add = W4_INIT;
    w4_add(mot1, mot2, add);
    w4_showhex(add);

    w4 mul = W4_INIT;
    w4_mul(mot1, mot2, mul);
    w4_showhex(mul);
}