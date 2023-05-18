#define WORD_SIZE 4
#define W4_INIT {0,0,0,0}
#define AX_HEX "02010103"
#define AX_INV_HEX "0e090d0b"


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
