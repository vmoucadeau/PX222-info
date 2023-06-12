#include "../../maths/words/words.h"
#include "../../maths/pol/pol.h"
#define nB 4

#define STATE_INIT {W4_INIT, W4_INIT, W4_INIT, W4_INIT}


typedef unsigned char state[nB][nB];

void state_parse(char input[8*nB], state res);
void state_showhex(state list);
void state_showstr(state list); 
void state_showbin(state list);
void state_getstr(state list, char res[8*nB]);
void state_concat(state list, char res[4*nB]);
void state_copy(state a, state res);
void state_add(state a, state b, state res);
void state_mul(state a, state b, state res);