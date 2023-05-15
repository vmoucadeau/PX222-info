char test_pol = 65;

#include "maths/bits/bits.h"
#include "maths/pol/pol.h"

#include <stdio.h>

int main() {
    gf256 test_pol = gf256_parse("1d");
    // printf("%i\n", test_pol);
    gf256_showhex(test_pol);
}