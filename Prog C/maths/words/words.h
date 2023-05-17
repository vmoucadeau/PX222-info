#define WORD_SIZE 4
#define nB 4
#define nK 4
#define W4_INIT {0,0,0,0}


typedef unsigned char w4 [WORD_SIZE];

void w4_showbin(w4 list);
void w4_showhex(w4 list);
void w4_add(w4 a, w4 b, w4 res);
void w4_mul(w4 a, w4 b, w4 res);