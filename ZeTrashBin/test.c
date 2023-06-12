
#include <stdio.h>
#include <stdlib.h>

char *stateinit()
{
    char *p = malloc(16);
    *p = 0x0123456789abcdef;
    printf("%x",*p);
    return p;
}

int main()
{
    char *p = stateinit();
    printf("%x,%x,%x,%x\n",(int)*p,(int)*(p+1),(int)*(p+2),(int)*(p+3));
    free(p);
    return 0;
}

// EOF
