#include "../state/state.h"
#include "../keyexp/keyexp.h"

void select_key(char *key);

void subbytes(state input, state output);
void inv_subbytes(state input, state output);

void shiftrows(state input, state output);
void inv_shiftrows(state input, state output);

void mixcolumns(state input, state output);
void inv_mixcolumns(state input, state output);

void addroundkey(state input, w4 words[nB], state output);

void cipher(state input, state output, w4 *key_expended);
void inv_cipher(state input, state output, w4 *key_expended);

void encode_block(char bloc[4*nB], w4 *key_expended, state output);
void decode_block(char bloc[4*nB], w4 *key_expended, state output);

void encode_blockstr(char bloc[8*nB], w4 *key_expended, state output);
void decode_blockstr(char bloc[8*nB], w4 *key_expended, state output);