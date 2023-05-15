#include <stdio.h>
#include <string.h>
#include "../pol/pol.h"

int search_hexval(char val) {
    for(int i = 0; i < 16; i++) {
        if (val == hex[i]) {
            return i;
        }
    }
    return 0;
}

