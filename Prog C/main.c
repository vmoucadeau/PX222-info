char test_pol = 65;

#include "maths/bits/bits.h"
#include "maths/pol/pol.h"

#include <stdio.h>
#include <stdlib.h>

void test_gf256_add() {
    gf256 test_pol = gf256_parse("1d");
    gf256_showhex(gf256_add(test_pol, test_pol)); // 1d xor 1d = 00
    gf256 test_pol2 = gf256_parse("1e");
    gf256_showhex(gf256_add(test_pol, test_pol2)); // 1d xor 1e = 03
}

void test_pol_add() {
    pol a;
    pol_init(a);
    a[0] = 1;
    a[1] = 5;
    a[2] = 3;
    pol b; pol_init(b);
    b[0] = 5;
    b[1] = 2;
    b[2] = 1;
    b[3] = 18;
    pol res; 
    pol_add(a, b, res);
    pol_show(res);
}

void test_pol_mul() {
    pol a;
    pol_init(a);
    a[0] = 1;
    a[1] = 5;
    a[2] = 3;
    pol b; pol_init(b);
    b[0] = 5;
    b[1] = 2;
    b[2] = 1;
    b[3] = 18;
    printf("a = "); pol_show(a);
    printf("b = "); pol_show(b);
    printf("deg(a) = %i\n", pol_deg(a));
    printf("deg(b) = %i\n", pol_deg(b));
    pol res; 
    pol_mul(a, b, res);
    printf("a*b = "); pol_show(res);
}

int main() {
    test_gf256_add();
    test_pol_add();
    test_pol_mul();
}