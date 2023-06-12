#include "../../algorithm/cipher/cipher.h"
#include "stdio.h"
#include "stdlib.h"

extern w4 *key_expended;

void encode_file(char *key, char *fileinput, char *fileoutput) {
    select_key(key);
    FILE *file_toencode = fopen(fileinput, "r");
    FILE *file_encoded = fopen(fileoutput, "w");
    char buffer[4*nB];
    if ( file_toencode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    size_t byte_read = fread(buffer, 1, 4*nB, file_toencode);
    while(byte_read == 16) {
        state result = STATE_INIT;
        encode_block(buffer, key_expended, result);
        char encoded[4*nB];
        state_concat(result, encoded);
        fwrite(encoded, 4*nB, 1, file_encoded);
        byte_read = fread(buffer, 1, 4*nB, file_toencode);
    }
    for(int i = byte_read; i < 16; i++) {
        buffer[i] = 16-byte_read;
    }
    state result = STATE_INIT;
    encode_block(buffer, key_expended, result);
    char encoded[4*nB];
    state_concat(result, encoded);
    fwrite(encoded, 4*nB, 1, file_encoded);
    fclose(file_toencode);
    fclose(file_encoded);
}

void decode_file(char *key, char *fileinput, char *fileoutput) {
    select_key(key);
    FILE *file_todecode = fopen(fileinput, "r");
    FILE *file_decoded = fopen(fileoutput, "w");
    char buffer[4*nB];
    if ( file_todecode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    size_t byte_read = fread(buffer, 1, 4*nB, file_todecode);

    state result = STATE_INIT;
    decode_block(buffer, key_expended, result);
    char decoded[4*nB];
    state_concat(result, decoded);
    byte_read = fread(buffer, 1, 4*nB, file_todecode);
    state_showhex(result);
    while(byte_read == 16) {
        fwrite(decoded, 4*nB, 1, file_decoded);
        state result = STATE_INIT;
        decode_block(buffer, key_expended, result);
        state_concat(result, decoded);
        byte_read = fread(buffer, 1, 4*nB, file_todecode);
        state_showhex(result);
    }
    //traitement decoded
    char padding = decoded[15];
    fwrite(decoded, 16-padding, 1, file_decoded);
    fclose(file_todecode);
    fclose(file_decoded);
    return;
}