
#include "../../algorithm/cipher/cipher.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <math.h>

extern w4 *key_expended;

void encode_file(char *key, char *fileinput, char *fileoutput, int mode) {
    select_key(key, strlen(key));
    FILE *file_toencode = fopen(fileinput, "r");
    FILE *file_encoded = fopen(fileoutput, "w");
    char buffer[4*nB];
    char encoded[4*nB];
    if ( file_toencode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    state previous = STATE_INIT;
    size_t byte_read = fread(buffer, 1, 4*nB, file_toencode);
    while(byte_read == 16) {
        state result = STATE_INIT;
        encode_block(buffer, key_expended, result, previous);
        if(mode != 0) state_copy(result, previous); // cbc mode
        state_concat(result, encoded);
        fwrite(encoded, 4*nB, 1, file_encoded);
        byte_read = fread(buffer, 1, 4*nB, file_toencode);
    }
    for(int i = byte_read; i < 16; i++) {
        buffer[i] = 16-byte_read;
    }
    state result = STATE_INIT;
    encode_block(buffer, key_expended, result, previous);
    state_concat(result, encoded);
    fwrite(encoded, 4*nB, 1, file_encoded);
    fclose(file_toencode);
    fclose(file_encoded);
}

void decode_file(char *key, char *fileinput, char *fileoutput, int mode) {
    select_key(key, strlen(key));
    FILE *file_todecode = fopen(fileinput, "r");
    FILE *file_decoded = fopen(fileoutput, "w");
    char buffer[4*nB];
    char decoded[4*nB];
    state previous = STATE_INIT;
    if ( file_todecode == NULL ) {
        fprintf( stderr, "Impossible d'ouvrir le fichier\n" );
        exit( -1 );
    }
    size_t byte_read = fread(buffer, 1, 4*nB, file_todecode);
    state result = STATE_INIT;
    state input = STATE_INIT;
    state_init(buffer, input);
    decode_block(buffer, key_expended, result, previous);
    if(mode != 0) state_copy(input, previous);
    state_concat(result, decoded);
    byte_read = fread(buffer, 1, 4*nB, file_todecode);
    while(byte_read == 16) {
        fwrite(decoded, 4*nB, 1, file_decoded);
        state_init(buffer, input);
        decode_block(buffer, key_expended, result, previous);
        if(mode != 0) state_copy(input, previous);
        state_concat(result, decoded);
        byte_read = fread(buffer, 1, 4*nB, file_todecode);
    }
    // padding
    char padding = decoded[15];
    fwrite(decoded, 16-padding, 1, file_decoded);
    fclose(file_todecode);
    fclose(file_decoded);
    return;
}

float file_entropy(char *fileinput) {
    FILE *myfile = fopen(fileinput, "r");
    float occurences[256] = {0};
    float proba[256] = {0};
    int octet = fgetc(myfile);
    float sum = 0;
    float count = 0;
    float entropy = 0;
    while(octet != EOF) {
        occurences[octet] += 1;
        octet = fgetc(myfile);
        count++;
    }
    for(int i = 0; i < 256; i++) {
        proba[i] = occurences[i]/count;
        sum += proba[i];
        entropy += proba[i] != 0 ? proba[i]*log2(count/occurences[i]) : 0;
    }
    printf("\ncount = %f\n sum = %f \n", count, sum);
    printf("entropy = %f\n", entropy);
    fclose(myfile);
    return entropy;
}