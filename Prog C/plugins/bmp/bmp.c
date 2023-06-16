
#include "../../algorithm/cipher/cipher.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#define head_size 10

extern w4 *key_expended;

void encode_bmp(char *key, char *fileinput, char *fileoutput, int mode) {
    select_key(key, strlen(key));
    FILE *file_toencode = fopen(fileinput, "r");
    FILE *file_encoded = fopen(fileoutput, "w");

    if ( file_toencode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }

    char buffer[4*nB];
    char encoded[4*nB];
    char head[head_size];
    unsigned char offset[4];
    
    fread(head, 1, head_size, file_toencode);
    fwrite(head, head_size, 1, file_encoded);
    fread(offset, 1, 4, file_toencode);
    fwrite(offset, 4, 1, file_encoded);
    int offset_int = offset[0] + offset[1] * 256 + offset[2] * 256 * 256 + offset[3] * 256 * 256 * 256;
    char *offset_buffer = malloc(offset_int);
    fread(offset_buffer, 1, offset_int, file_toencode);
    fwrite(offset_buffer, offset_int, 1, file_encoded);
    free(offset_buffer);
    
    state previous = STATE_INIT;
    state result = STATE_INIT;
    size_t byte_read = fread(buffer, 1, 4*nB, file_toencode);
    while(byte_read == 16) {
        encode_block(buffer, key_expended, result, previous);
        if(mode != 0) state_copy(result, previous); // cbc mode
        state_concat(result, encoded);
        fwrite(encoded, 4*nB, 1, file_encoded);
        byte_read = fread(buffer, 1, 4*nB, file_toencode);
    }
    fwrite(buffer, byte_read, 1, file_encoded);
    fclose(file_toencode);
    fclose(file_encoded);
}

void decode_bmp(char *key, char *fileinput, char *fileoutput, int mode) {
    select_key(key, strlen(key));
    FILE *file_todecode = fopen(fileinput, "r");
    FILE *file_decoded = fopen(fileoutput, "w");
    if ( file_todecode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    
    char buffer[4*nB];
    char head[head_size];
    unsigned char offset[4];
    char decoded[4*nB];
    
    fread(head, 1, head_size, file_todecode);
    fwrite(head, head_size, 1, file_decoded);
    fread(offset, 1, 4, file_todecode);
    fwrite(offset, 4, 1, file_decoded);
    int offset_int = offset[0] + offset[1] * 256 + offset[2] * 256 * 256 + offset[3] * 256 * 256 * 256;
    char *offset_buffer = malloc(offset_int);
    fread(offset_buffer, 1, offset_int, file_todecode);
    fwrite(offset_buffer, offset_int, 1, file_decoded);
    free(offset_buffer);

    state previous = STATE_INIT;
    state result = STATE_INIT;
    state input = STATE_INIT;
    size_t byte_read = fread(buffer, 1, 4*nB, file_todecode);
    while(byte_read == 16) {
        state_init(buffer, input);
        decode_block(buffer, key_expended, result, previous);
        if(mode != 0) state_copy(input, previous); // cbc mode
        state_concat(result, decoded);
        fwrite(decoded, 4*nB, 1, file_decoded);
        byte_read = fread(buffer, 1, 4*nB, file_todecode);
    }
    fwrite(buffer, byte_read, 1, file_decoded);
    fclose(file_todecode);
    fclose(file_decoded);
    return;
}
