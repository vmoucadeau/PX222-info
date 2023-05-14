char test_pol = 65;

#include "maths/bits/bits.h"
#include "maths/pol/pol.h"

#include <stdio.h>
#include <stdlib.h>

int main() {
    char test_pol = parse_pol("1d");
    // printf("%i\n", test_pol);
    show_hex_pol(test_pol);
    int test_pol2[] = {0,0,0,0,25,10};
    int *cleaned = malloc(sizeof(test_pol2)/sizeof(int));
    int deg = pol_clean(test_pol2, sizeof(test_pol2)/sizeof(int), cleaned);
    // show clean pol
    printf("\n");
    for(int i = 0; i < deg; i++) {
        printf("%i ", cleaned[i]);
    }
}

