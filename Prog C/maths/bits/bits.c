
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "bits.h"

z2z z2z_parse(int input) {
    return input % 2;
}

z2z z2z_add(z2z a, z2z b) {
    return a ^ b;
}

z2z z2z_mul(z2z a, z2z b) {
    return a & b;
}

z2z z2z_inv(z2z a) { // non sens pour 0
    return a;
}
