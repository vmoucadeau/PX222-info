
#include <stdbool.h>

#define WORD_SIZE 4
#define W4_INIT {0,0,0,0}
#define AX_HEX "02010103"
#define AX {0x02, 0x01, 0x01, 0x03}
#define AX_INV_HEX "0e090d0b"
#define AX_INV {0x0e, 0x09, 0x0d, 0x0b}

typedef unsigned char w4 [WORD_SIZE];

void w4_parse(char word[8], w4 res);
void w4_copy(w4 a, w4 res);
void w4_showbin(w4 list);
void w4_showhex(w4 list);
void w4_showstr(w4 list);
void w4_showlst(w4 *list, int length);
void w4_getstr(w4 list, char res[8]);
void w4_add(w4 a, w4 b, w4 res);
void w4_mul(w4 a, w4 b, w4 res);
bool w4_isequal(w4 a, w4 b);
bool w4_lstisequal(w4 *a, w4 *b, int length);
