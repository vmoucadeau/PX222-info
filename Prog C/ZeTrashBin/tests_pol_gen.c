void test_pol_add() {
    pol a;
    pol_init(a);
    a[0] = 1;
    a[1] = 1;
    a[2] = 0;
    pol b; pol_init(b);
    b[0] = 1;
    b[1] = 1;
    b[2] = 0;
    b[3] = 0;
    pol res; 
    pol_add(a, b, res);
    pol_show(res);
}

void test_pol_mul() {
    pol a;
    pol_init(a);
    a[0] = 1;
    a[1] = 1;
    a[2] = 0;
    pol b; pol_init(b);
    b[0] = 1;
    b[1] = 1;
    b[2] = 0;
    b[3] = 0;
    printf("a = "); pol_show(a);
    printf("b = "); pol_show(b);
    printf("deg(a) = %i\n", pol_deg(a));
    printf("deg(b) = %i\n", pol_deg(b));
    pol res; 
    pol_mul(a, b, res);
    printf("a*b = "); pol_show(res);
}