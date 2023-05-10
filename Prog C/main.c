char test_pol = 65;

#include "maths/bits/bits.h"
#include "maths/pol/pol.h"

#include <stdio.h>

int main() {
    char test_pol = parse_pol("1d");
    // printf("%i\n", test_pol);
    show_hex_pol(test_pol);
}