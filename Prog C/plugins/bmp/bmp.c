
#include "../../algorithm/cipher/cipher.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#define head_size 72

extern w4 *key_expended;

void encode_bmp(char *key, char *fileinput, char *fileoutput) {
    select_key(key, strlen(key));
    FILE *file_toencode = fopen(fileinput, "r");
    FILE *file_encoded = fopen(fileoutput, "w");
    char buffer[4*nB];
    char encoded[4*nB];
    char head[head_size];
    if ( file_toencode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    fread(head, 1, head_size, file_toencode);
    fwrite(head, head_size, 1, file_encoded);
    size_t byte_read = fread(buffer, 1, 4*nB, file_toencode);
    while(byte_read == 16) {
        state result = STATE_INIT;
        encode_block(buffer, key_expended, result);
        state_concat(result, encoded);
        fwrite(encoded, 4*nB, 1, file_encoded);
        byte_read = fread(buffer, 1, 4*nB, file_toencode);
    }
    fwrite(buffer, byte_read, 1, file_encoded);
    fclose(file_toencode);
    fclose(file_encoded);
}

void decode_bmp(char *key, char *fileinput, char *fileoutput) {
    select_key(key, strlen(key));
    FILE *file_todecode = fopen(fileinput, "r");
    FILE *file_decoded = fopen(fileoutput, "w");
    char buffer[4*nB];
    char head[head_size];
    if ( file_todecode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    fread(head, 1, head_size, file_todecode);
    fwrite(head, head_size, 1, file_decoded);
    size_t byte_read = fread(buffer, 1, 4*nB, file_todecode);
    while(byte_read == 16) {
        state result = STATE_INIT;
        decode_block(buffer, key_expended, result);
        char decoded[4*nB];
        state_concat(result, decoded);
        fwrite(decoded, 4*nB, 1, file_decoded);
        byte_read = fread(buffer, 1, 4*nB, file_todecode);
    }
    fwrite(buffer, byte_read, 1, file_decoded);
    fclose(file_todecode);
    fclose(file_decoded);
    return;
}
