
// Include Zone

#include <stdio.h>

// Define Zone

// #define W4 unsigned int
// #define F8 unsigned char
typedef unsigned int W4;
typedef unsigned char F8;

// Const Zone

const F8 sbox[] = {
    0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
    0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
    0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
    0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
    0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
    0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
    0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
    0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
    0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
    0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
    0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
    0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
    0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
    0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
    0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
    0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16};

const F8 sboxi[] = {
    0x52,0x09,0x6a,0xd5,0x30,0x36,0xa5,0x38,0xbf,0x40,0xa3,0x9e,0x81,0xf3,0xd7,0xfb,
    0x7c,0xe3,0x39,0x82,0x9b,0x2f,0xff,0x87,0x34,0x8e,0x43,0x44,0xc4,0xde,0xe9,0xcb,
    0x54,0x7b,0x94,0x32,0xa6,0xc2,0x23,0x3d,0xee,0x4c,0x95,0x0b,0x42,0xfa,0xc3,0x4e,
    0x08,0x2e,0xa1,0x66,0x28,0xd9,0x24,0xb2,0x76,0x5b,0xa2,0x49,0x6d,0x8b,0xd1,0x25,
    0x72,0xf8,0xf6,0x64,0x86,0x68,0x98,0x16,0xd4,0xa4,0x5c,0xcc,0x5d,0x65,0xb6,0x92,
    0x6c,0x70,0x48,0x50,0xfd,0xed,0xb9,0xda,0x5e,0x15,0x46,0x57,0xa7,0x8d,0x9d,0x84,
    0x90,0xd8,0xab,0x00,0x8c,0xbc,0xd3,0x0a,0xf7,0xe4,0x58,0x05,0xb8,0xb3,0x45,0x06,
    0xd0,0x2c,0x1e,0x8f,0xca,0x3f,0x0f,0x02,0xc1,0xaf,0xbd,0x03,0x01,0x13,0x8a,0x6b,
    0x3a,0x91,0x11,0x41,0x4f,0x67,0xdc,0xea,0x97,0xf2,0xcf,0xce,0xf0,0xb4,0xe6,0x73,
    0x96,0xac,0x74,0x22,0xe7,0xad,0x35,0x85,0xe2,0xf9,0x37,0xe8,0x1c,0x75,0xdf,0x6e,
    0x47,0xf1,0x1a,0x71,0x1d,0x29,0xc5,0x89,0x6f,0xb7,0x62,0x0e,0xaa,0x18,0xbe,0x1b,
    0xfc,0x56,0x3e,0x4b,0xc6,0xd2,0x79,0x20,0x9a,0xdb,0xc0,0xfe,0x78,0xcd,0x5a,0xf4,
    0x1f,0xdd,0xa8,0x33,0x88,0x07,0xc7,0x31,0xb1,0x12,0x10,0x59,0x27,0x80,0xec,0x5f,
    0x60,0x51,0x7f,0xa9,0x19,0xb5,0x4a,0x0d,0x2d,0xe5,0x7a,0x9f,0x93,0xc9,0x9c,0xef,
    0xa0,0xe0,0x3b,0x4d,0xae,0x2a,0xf5,0xb0,0xc8,0xeb,0xbb,0x3c,0x83,0x53,0x99,0x61,
    0x17,0x2b,0x04,0x7e,0xba,0x77,0xd6,0x26,0xe1,0x69,0x14,0x63,0x55,0x21,0x0c,0x7d};

const F8 F8x02[] = {
    0x00,0x02,0x04,0x06,0x08,0x0a,0x0c,0x0e,0x10,0x12,0x14,0x16,0x18,0x1a,0x1c,0x1e,
    0x20,0x22,0x24,0x26,0x28,0x2a,0x2c,0x2e,0x30,0x32,0x34,0x36,0x38,0x3a,0x3c,0x3e,
    0x40,0x42,0x44,0x46,0x48,0x4a,0x4c,0x4e,0x50,0x52,0x54,0x56,0x58,0x5a,0x5c,0x5e,
    0x60,0x62,0x64,0x66,0x68,0x6a,0x6c,0x6e,0x70,0x72,0x74,0x76,0x78,0x7a,0x7c,0x7e,
    0x80,0x82,0x84,0x86,0x88,0x8a,0x8c,0x8e,0x90,0x92,0x94,0x96,0x98,0x9a,0x9c,0x9e,
    0xa0,0xa2,0xa4,0xa6,0xa8,0xaa,0xac,0xae,0xb0,0xb2,0xb4,0xb6,0xb8,0xba,0xbc,0xbe,
    0xc0,0xc2,0xc4,0xc6,0xc8,0xca,0xcc,0xce,0xd0,0xd2,0xd4,0xd6,0xd8,0xda,0xdc,0xde,
    0xe0,0xe2,0xe4,0xe6,0xe8,0xea,0xec,0xee,0xf0,0xf2,0xf4,0xf6,0xf8,0xfa,0xfc,0xfe,
    0x1b,0x19,0x1f,0x1d,0x13,0x11,0x17,0x15,0x0b,0x09,0x0f,0x0d,0x03,0x01,0x07,0x05,
    0x3b,0x39,0x3f,0x3d,0x33,0x31,0x37,0x35,0x2b,0x29,0x2f,0x2d,0x23,0x21,0x27,0x25,
    0x5b,0x59,0x5f,0x5d,0x53,0x51,0x57,0x55,0x4b,0x49,0x4f,0x4d,0x43,0x41,0x47,0x45,
    0x7b,0x79,0x7f,0x7d,0x73,0x71,0x77,0x75,0x6b,0x69,0x6f,0x6d,0x63,0x61,0x67,0x65,
    0x9b,0x99,0x9f,0x9d,0x93,0x91,0x97,0x95,0x8b,0x89,0x8f,0x8d,0x83,0x81,0x87,0x85,
    0xbb,0xb9,0xbf,0xbd,0xb3,0xb1,0xb7,0xb5,0xab,0xa9,0xaf,0xad,0xa3,0xa1,0xa7,0xa5,
    0xdb,0xd9,0xdf,0xdd,0xd3,0xd1,0xd7,0xd5,0xcb,0xc9,0xcf,0xcd,0xc3,0xc1,0xc7,0xc5,
    0xfb,0xf9,0xff,0xfd,0xf3,0xf1,0xf7,0xf5,0xeb,0xe9,0xef,0xed,0xe3,0xe1,0xe7,0xe5};

const F8 F8x03[] = {
    0x00,0x03,0x06,0x05,0x0c,0x0f,0x0a,0x09,0x18,0x1b,0x1e,0x1d,0x14,0x17,0x12,0x11,
    0x30,0x33,0x36,0x35,0x3c,0x3f,0x3a,0x39,0x28,0x2b,0x2e,0x2d,0x24,0x27,0x22,0x21,
    0x60,0x63,0x66,0x65,0x6c,0x6f,0x6a,0x69,0x78,0x7b,0x7e,0x7d,0x74,0x77,0x72,0x71,
    0x50,0x53,0x56,0x55,0x5c,0x5f,0x5a,0x59,0x48,0x4b,0x4e,0x4d,0x44,0x47,0x42,0x41,
    0xc0,0xc3,0xc6,0xc5,0xcc,0xcf,0xca,0xc9,0xd8,0xdb,0xde,0xdd,0xd4,0xd7,0xd2,0xd1,
    0xf0,0xf3,0xf6,0xf5,0xfc,0xff,0xfa,0xf9,0xe8,0xeb,0xee,0xed,0xe4,0xe7,0xe2,0xe1,
    0xa0,0xa3,0xa6,0xa5,0xac,0xaf,0xaa,0xa9,0xb8,0xbb,0xbe,0xbd,0xb4,0xb7,0xb2,0xb1,
    0x90,0x93,0x96,0x95,0x9c,0x9f,0x9a,0x99,0x88,0x8b,0x8e,0x8d,0x84,0x87,0x82,0x81,
    0x9b,0x98,0x9d,0x9e,0x97,0x94,0x91,0x92,0x83,0x80,0x85,0x86,0x8f,0x8c,0x89,0x8a,
    0xab,0xa8,0xad,0xae,0xa7,0xa4,0xa1,0xa2,0xb3,0xb0,0xb5,0xb6,0xbf,0xbc,0xb9,0xba,
    0xfb,0xf8,0xfd,0xfe,0xf7,0xf4,0xf1,0xf2,0xe3,0xe0,0xe5,0xe6,0xef,0xec,0xe9,0xea,
    0xcb,0xc8,0xcd,0xce,0xc7,0xc4,0xc1,0xc2,0xd3,0xd0,0xd5,0xd6,0xdf,0xdc,0xd9,0xda,
    0x5b,0x58,0x5d,0x5e,0x57,0x54,0x51,0x52,0x43,0x40,0x45,0x46,0x4f,0x4c,0x49,0x4a,
    0x6b,0x68,0x6d,0x6e,0x67,0x64,0x61,0x62,0x73,0x70,0x75,0x76,0x7f,0x7c,0x79,0x7a,
    0x3b,0x38,0x3d,0x3e,0x37,0x34,0x31,0x32,0x23,0x20,0x25,0x26,0x2f,0x2c,0x29,0x2a,
    0x0b,0x08,0x0d,0x0e,0x07,0x04,0x01,0x02,0x13,0x10,0x15,0x16,0x1f,0x1c,0x19,0x1a};

const F8 F8x09[] = {
    0x00,0x09,0x12,0x1b,0x24,0x2d,0x36,0x3f,0x48,0x41,0x5a,0x53,0x6c,0x65,0x7e,0x77,
    0x90,0x99,0x82,0x8b,0xb4,0xbd,0xa6,0xaf,0xd8,0xd1,0xca,0xc3,0xfc,0xf5,0xee,0xe7,
    0x3b,0x32,0x29,0x20,0x1f,0x16,0x0d,0x04,0x73,0x7a,0x61,0x68,0x57,0x5e,0x45,0x4c,
    0xab,0xa2,0xb9,0xb0,0x8f,0x86,0x9d,0x94,0xe3,0xea,0xf1,0xf8,0xc7,0xce,0xd5,0xdc,
    0x76,0x7f,0x64,0x6d,0x52,0x5b,0x40,0x49,0x3e,0x37,0x2c,0x25,0x1a,0x13,0x08,0x01,
    0xe6,0xef,0xf4,0xfd,0xc2,0xcb,0xd0,0xd9,0xae,0xa7,0xbc,0xb5,0x8a,0x83,0x98,0x91,
    0x4d,0x44,0x5f,0x56,0x69,0x60,0x7b,0x72,0x05,0x0c,0x17,0x1e,0x21,0x28,0x33,0x3a,
    0xdd,0xd4,0xcf,0xc6,0xf9,0xf0,0xeb,0xe2,0x95,0x9c,0x87,0x8e,0xb1,0xb8,0xa3,0xaa,
    0xec,0xe5,0xfe,0xf7,0xc8,0xc1,0xda,0xd3,0xa4,0xad,0xb6,0xbf,0x80,0x89,0x92,0x9b,
    0x7c,0x75,0x6e,0x67,0x58,0x51,0x4a,0x43,0x34,0x3d,0x26,0x2f,0x10,0x19,0x02,0x0b,
    0xd7,0xde,0xc5,0xcc,0xf3,0xfa,0xe1,0xe8,0x9f,0x96,0x8d,0x84,0xbb,0xb2,0xa9,0xa0,
    0x47,0x4e,0x55,0x5c,0x63,0x6a,0x71,0x78,0x0f,0x06,0x1d,0x14,0x2b,0x22,0x39,0x30,
    0x9a,0x93,0x88,0x81,0xbe,0xb7,0xac,0xa5,0xd2,0xdb,0xc0,0xc9,0xf6,0xff,0xe4,0xed,
    0x0a,0x03,0x18,0x11,0x2e,0x27,0x3c,0x35,0x42,0x4b,0x50,0x59,0x66,0x6f,0x74,0x7d,
    0xa1,0xa8,0xb3,0xba,0x85,0x8c,0x97,0x9e,0xe9,0xe0,0xfb,0xf2,0xcd,0xc4,0xdf,0xd6,
    0x31,0x38,0x23,0x2a,0x15,0x1c,0x07,0x0e,0x79,0x70,0x6b,0x62,0x5d,0x54,0x4f,0x46};

const F8 F8x0b[] = {
    0x00,0x0b,0x16,0x1d,0x2c,0x27,0x3a,0x31,0x58,0x53,0x4e,0x45,0x74,0x7f,0x62,0x69,
    0xb0,0xbb,0xa6,0xad,0x9c,0x97,0x8a,0x81,0xe8,0xe3,0xfe,0xf5,0xc4,0xcf,0xd2,0xd9,
    0x7b,0x70,0x6d,0x66,0x57,0x5c,0x41,0x4a,0x23,0x28,0x35,0x3e,0x0f,0x04,0x19,0x12,
    0xcb,0xc0,0xdd,0xd6,0xe7,0xec,0xf1,0xfa,0x93,0x98,0x85,0x8e,0xbf,0xb4,0xa9,0xa2,
    0xf6,0xfd,0xe0,0xeb,0xda,0xd1,0xcc,0xc7,0xae,0xa5,0xb8,0xb3,0x82,0x89,0x94,0x9f,
    0x46,0x4d,0x50,0x5b,0x6a,0x61,0x7c,0x77,0x1e,0x15,0x08,0x03,0x32,0x39,0x24,0x2f,
    0x8d,0x86,0x9b,0x90,0xa1,0xaa,0xb7,0xbc,0xd5,0xde,0xc3,0xc8,0xf9,0xf2,0xef,0xe4,
    0x3d,0x36,0x2b,0x20,0x11,0x1a,0x07,0x0c,0x65,0x6e,0x73,0x78,0x49,0x42,0x5f,0x54,
    0xf7,0xfc,0xe1,0xea,0xdb,0xd0,0xcd,0xc6,0xaf,0xa4,0xb9,0xb2,0x83,0x88,0x95,0x9e,
    0x47,0x4c,0x51,0x5a,0x6b,0x60,0x7d,0x76,0x1f,0x14,0x09,0x02,0x33,0x38,0x25,0x2e,
    0x8c,0x87,0x9a,0x91,0xa0,0xab,0xb6,0xbd,0xd4,0xdf,0xc2,0xc9,0xf8,0xf3,0xee,0xe5,
    0x3c,0x37,0x2a,0x21,0x10,0x1b,0x06,0x0d,0x64,0x6f,0x72,0x79,0x48,0x43,0x5e,0x55,
    0x01,0x0a,0x17,0x1c,0x2d,0x26,0x3b,0x30,0x59,0x52,0x4f,0x44,0x75,0x7e,0x63,0x68,
    0xb1,0xba,0xa7,0xac,0x9d,0x96,0x8b,0x80,0xe9,0xe2,0xff,0xf4,0xc5,0xce,0xd3,0xd8,
    0x7a,0x71,0x6c,0x67,0x56,0x5d,0x40,0x4b,0x22,0x29,0x34,0x3f,0x0e,0x05,0x18,0x13,
    0xca,0xc1,0xdc,0xd7,0xe6,0xed,0xf0,0xfb,0x92,0x99,0x84,0x8f,0xbe,0xb5,0xa8,0xa3};

const F8 F8x0d[] = {
    0x00,0x0d,0x1a,0x17,0x34,0x39,0x2e,0x23,0x68,0x65,0x72,0x7f,0x5c,0x51,0x46,0x4b,
    0xd0,0xdd,0xca,0xc7,0xe4,0xe9,0xfe,0xf3,0xb8,0xb5,0xa2,0xaf,0x8c,0x81,0x96,0x9b,
    0xbb,0xb6,0xa1,0xac,0x8f,0x82,0x95,0x98,0xd3,0xde,0xc9,0xc4,0xe7,0xea,0xfd,0xf0,
    0x6b,0x66,0x71,0x7c,0x5f,0x52,0x45,0x48,0x03,0x0e,0x19,0x14,0x37,0x3a,0x2d,0x20,
    0x6d,0x60,0x77,0x7a,0x59,0x54,0x43,0x4e,0x05,0x08,0x1f,0x12,0x31,0x3c,0x2b,0x26,
    0xbd,0xb0,0xa7,0xaa,0x89,0x84,0x93,0x9e,0xd5,0xd8,0xcf,0xc2,0xe1,0xec,0xfb,0xf6,
    0xd6,0xdb,0xcc,0xc1,0xe2,0xef,0xf8,0xf5,0xbe,0xb3,0xa4,0xa9,0x8a,0x87,0x90,0x9d,
    0x06,0x0b,0x1c,0x11,0x32,0x3f,0x28,0x25,0x6e,0x63,0x74,0x79,0x5a,0x57,0x40,0x4d,
    0xda,0xd7,0xc0,0xcd,0xee,0xe3,0xf4,0xf9,0xb2,0xbf,0xa8,0xa5,0x86,0x8b,0x9c,0x91,
    0x0a,0x07,0x10,0x1d,0x3e,0x33,0x24,0x29,0x62,0x6f,0x78,0x75,0x56,0x5b,0x4c,0x41,
    0x61,0x6c,0x7b,0x76,0x55,0x58,0x4f,0x42,0x09,0x04,0x13,0x1e,0x3d,0x30,0x27,0x2a,
    0xb1,0xbc,0xab,0xa6,0x85,0x88,0x9f,0x92,0xd9,0xd4,0xc3,0xce,0xed,0xe0,0xf7,0xfa,
    0xb7,0xba,0xad,0xa0,0x83,0x8e,0x99,0x94,0xdf,0xd2,0xc5,0xc8,0xeb,0xe6,0xf1,0xfc,
    0x67,0x6a,0x7d,0x70,0x53,0x5e,0x49,0x44,0x0f,0x02,0x15,0x18,0x3b,0x36,0x21,0x2c,
    0x0c,0x01,0x16,0x1b,0x38,0x35,0x22,0x2f,0x64,0x69,0x7e,0x73,0x50,0x5d,0x4a,0x47,
    0xdc,0xd1,0xc6,0xcb,0xe8,0xe5,0xf2,0xff,0xb4,0xb9,0xae,0xa3,0x80,0x8d,0x9a,0x97};

const F8 F8x0e[] = {
    0x00,0x0e,0x1c,0x12,0x38,0x36,0x24,0x2a,0x70,0x7e,0x6c,0x62,0x48,0x46,0x54,0x5a,
    0xe0,0xee,0xfc,0xf2,0xd8,0xd6,0xc4,0xca,0x90,0x9e,0x8c,0x82,0xa8,0xa6,0xb4,0xba,
    0xdb,0xd5,0xc7,0xc9,0xe3,0xed,0xff,0xf1,0xab,0xa5,0xb7,0xb9,0x93,0x9d,0x8f,0x81,
    0x3b,0x35,0x27,0x29,0x03,0x0d,0x1f,0x11,0x4b,0x45,0x57,0x59,0x73,0x7d,0x6f,0x61,
    0xad,0xa3,0xb1,0xbf,0x95,0x9b,0x89,0x87,0xdd,0xd3,0xc1,0xcf,0xe5,0xeb,0xf9,0xf7,
    0x4d,0x43,0x51,0x5f,0x75,0x7b,0x69,0x67,0x3d,0x33,0x21,0x2f,0x05,0x0b,0x19,0x17,
    0x76,0x78,0x6a,0x64,0x4e,0x40,0x52,0x5c,0x06,0x08,0x1a,0x14,0x3e,0x30,0x22,0x2c,
    0x96,0x98,0x8a,0x84,0xae,0xa0,0xb2,0xbc,0xe6,0xe8,0xfa,0xf4,0xde,0xd0,0xc2,0xcc,
    0x41,0x4f,0x5d,0x53,0x79,0x77,0x65,0x6b,0x31,0x3f,0x2d,0x23,0x09,0x07,0x15,0x1b,
    0xa1,0xaf,0xbd,0xb3,0x99,0x97,0x85,0x8b,0xd1,0xdf,0xcd,0xc3,0xe9,0xe7,0xf5,0xfb,
    0x9a,0x94,0x86,0x88,0xa2,0xac,0xbe,0xb0,0xea,0xe4,0xf6,0xf8,0xd2,0xdc,0xce,0xc0,
    0x7a,0x74,0x66,0x68,0x42,0x4c,0x5e,0x50,0x0a,0x04,0x16,0x18,0x32,0x3c,0x2e,0x20,
    0xec,0xe2,0xf0,0xfe,0xd4,0xda,0xc8,0xc6,0x9c,0x92,0x80,0x8e,0xa4,0xaa,0xb8,0xb6,
    0x0c,0x02,0x10,0x1e,0x34,0x3a,0x28,0x26,0x7c,0x72,0x60,0x6e,0x44,0x4a,0x58,0x56,
    0x37,0x39,0x2b,0x25,0x0f,0x01,0x13,0x1d,0x47,0x49,0x5b,0x55,0x7f,0x71,0x63,0x6d,
    0xd7,0xd9,0xcb,0xc5,0xef,0xe1,0xf3,0xfd,0xa7,0xa9,0xbb,0xb5,0x9f,0x91,0x83,0x8d};

const F8 Rcon[] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1b,0x36,0x6c,0xd8,0xab,0x4d};

// Support Zone

void printW4(W4 word)
{
    printf("%02x%02x%02x%02x",word<<24>>24,word<<16>>24,word<<8>>24,word<<0>>24);
    return;
}

void printkeyDX(W4 *keyDX,int nK)
{
    for(int i=0;i<4*(nK+7);i++)
    {
        printW4(keyDX[i]);
        putchar(' ');
    }
    putchar('\n');
    return;
}

void printstate(W4 *state)
{
    for(int i=0;i<4;i++)
    {
        printW4(state[i]);
    }
    putchar('\n');
    return;
}

W4 subword(W4 word)
{
    W4 a=(sbox[word<<0>>24]<<24)|(sbox[word<<8>>24]<<16)|(sbox[word<<16>>24]<<8)|(sbox[word<<24>>24]<<0);
    return a;
}

W4 rotword(W4 word)
{
    W4 a=word>>8|word<<24;
    return a;
}

void keyexpansion(W4 *key, W4 *keyDX, int nK)
{
    for(int i=0;i<nK;i++) keyDX[i]=key[i];
    for(int i=nK;i<4*(nK+7);i++)
    keyDX[i]=keyDX[i-nK]^(i%nK==0?subword(rotword(keyDX[i-1]))^Rcon[i/nK-1]:nK>7&&i%nK==4?subword(keyDX[i-1]):keyDX[i-1]);
    return;
}

// Cipher Zone

void subbytes(W4 *state)
{
    for(int i=0;i<4;i++)
    {
        W4 a=0;
        a^=sbox[state[i]<<0>>24]<<24;
        a^=sbox[state[i]<<8>>24]<<16;
        a^=sbox[state[i]<<16>>24]<<8;
        a^=sbox[state[i]<<24>>24]<<0;
        state[i]=a;
    }
    return;
}

void shiftrows(W4 *state)
{
    W4 w0=(state[0]&(0xFF<<0))|(state[1]&(0xFF<<8))|(state[2]&(0xFF<<16))|(state[3]&(0xFF<<24));
    W4 w1=(state[1]&(0xFF<<0))|(state[2]&(0xFF<<8))|(state[3]&(0xFF<<16))|(state[0]&(0xFF<<24));
    W4 w2=(state[2]&(0xFF<<0))|(state[3]&(0xFF<<8))|(state[0]&(0xFF<<16))|(state[1]&(0xFF<<24));
    W4 w3=(state[3]&(0xFF<<0))|(state[0]&(0xFF<<8))|(state[1]&(0xFF<<16))|(state[2]&(0xFF<<24));
    state[0]=w0;
    state[1]=w1;
    state[2]=w2;
    state[3]=w3;
    return;
}

void mixcolumns(W4 *state)
{
    for(int i=0;i<4;i++)
    {
        W4 a=0;
        a^=(F8x02[state[i]<<24>>24]^F8x03[state[i]<<16>>24]^(state[i]<<8>>24)^state[i]<<0>>24)<<0;
        a^=(F8x02[state[i]<<16>>24]^F8x03[state[i]<<8>>24]^(state[i]<<0>>24)^state[i]<<24>>24)<<8;
        a^=(F8x02[state[i]<<8>>24]^F8x03[state[i]<<0>>24]^(state[i]<<24>>24)^state[i]<<16>>24)<<16;
        a^=(F8x02[state[i]<<0>>24]^F8x03[state[i]<<24>>24]^(state[i]<<16>>24)^state[i]<<8>>24)<<24;
        state[i]=a;
    }
    return;
}

void invsubbytes(W4 *state)
{
    for(int i=0;i<4;i++)
    {
        W4 a=0;
        a^=sboxi[state[i]<<0>>24]<<24;
        a^=sboxi[state[i]<<8>>24]<<16;
        a^=sboxi[state[i]<<16>>24]<<8;
        a^=sboxi[state[i]<<24>>24]<<0;
        state[i]=a;
    }
    return;
}

void invshiftrows(W4 *state)
{
    W4 w0=(state[0]&(0xFF<<0))|(state[3]&(0xFF<<8))|(state[2]&(0xFF<<16))|(state[1]&(0xFF<<24));
    W4 w1=(state[1]&(0xFF<<0))|(state[0]&(0xFF<<8))|(state[3]&(0xFF<<16))|(state[2]&(0xFF<<24));
    W4 w2=(state[2]&(0xFF<<0))|(state[1]&(0xFF<<8))|(state[0]&(0xFF<<16))|(state[3]&(0xFF<<24));
    W4 w3=(state[3]&(0xFF<<0))|(state[2]&(0xFF<<8))|(state[1]&(0xFF<<16))|(state[0]&(0xFF<<24));
    state[0]=w0;
    state[1]=w1;
    state[2]=w2;
    state[3]=w3;
    return;
}

void invmixcolumns(W4 *state)
{
    for(int i=0;i<4;i++)
    {
        W4 a=0;
        a^=(F8x0e[state[i]<<24>>24]^F8x0b[state[i]<<16>>24]^F8x0d[state[i]<<8>>24]^F8x09[state[i]<<0>>24])<<0;
        a^=(F8x0e[state[i]<<16>>24]^F8x0b[state[i]<<8>>24]^F8x0d[state[i]<<0>>24]^F8x09[state[i]<<24>>24])<<8;
        a^=(F8x0e[state[i]<<8>>24]^F8x0b[state[i]<<0>>24]^F8x0d[state[i]<<24>>24]^F8x09[state[i]<<16>>24])<<16;
        a^=(F8x0e[state[i]<<0>>24]^F8x0b[state[i]<<24>>24]^F8x0d[state[i]<<16>>24]^F8x09[state[i]<<8>>24])<<24;
        state[i]=a;
    }
    return;
}

void addroundkey(W4 *state, W4 *key)
{
    state[0]^=key[0];
    state[1]^=key[1];
    state[2]^=key[2];
    state[3]^=key[3];
    return;
}

// Encryption Zone

void cipher(W4 *state, W4 *key, int nK)
{
    addroundkey(state,key);
    for(int i=1;i<nK+6;i++)
    {
        subbytes(state);
        shiftrows(state);
        mixcolumns(state);
        addroundkey(state,key+4*i);
    }
    subbytes(state);
    shiftrows(state);
    addroundkey(state,key+4*(nK+6));
    return;
}

void uncipher(W4 *state, W4 *key, int nK)
{
    addroundkey(state,key+4*(nK+6));
    for(int i=nK+5;i>0;i--)
    {
        invshiftrows(state);
        invsubbytes(state);
        addroundkey(state,key+4*i);
        invmixcolumns(state);
    }
    invshiftrows(state);
    invsubbytes(state);
    addroundkey(state,key);
    return;
}

int aes_encrypt(char *data, int txtsize, char *key, int keysize)
{
    if(txtsize%16!=0) return 1;
    if(keysize!=16 && keysize!=24 && keysize!=32) return 1;
    int nK = keysize/4;
    W4 keyDX[4*(nK+7)];
    keyexpansion((W4*)key,keyDX,nK);
    for(int i=0;i<txtsize/16;i++) cipher((W4*)(data+16*i),keyDX,nK);
    return 0;
}

int aes_decrypt(char *data, int txtsize, char *key, int keysize)
{
    if(txtsize%16!=0) return 1;
    if(keysize!=16 && keysize!=24 && keysize!=32) return 1;
    int nK = keysize/4;
    W4 keyDX[4*(nK+7)];
    keyexpansion((W4*)key,keyDX,nK);
    for(int i=0;i<txtsize/16;i++) uncipher((W4*)(data+16*i),keyDX,nK);
    return 0;
}

// Test Zone

void testsubbytes()
{
    printf(" Tests subbytes:\n");
    W4 state1[] = {0xdbaf2670,0xa2f28066,0x53546cf9,0x958769c0};
    printstate(state1);
    subbytes(state1);
    printstate(state1);
    W4 state2[] = {0x03020100,0x07060504,0x0b0a0908,0x0f0e0d0c};
    printstate(state2);
    subbytes(state2);
    printstate(state2);
    return;
}

void testshiftrows()
{
    printf(" Tests shiftrows:\n");
    W4 state1[] = {0xdbaf2670,0xa2f28066,0x53546cf9,0x958769c0};
    printstate(state1);
    shiftrows(state1);
    printstate(state1);
    W4 state2[] = {0x03020100,0x07060504,0x0b0a0908,0x0f0e0d0c};
    printstate(state2);
    shiftrows(state2);
    printstate(state2);
    return;
}

void testmixcolumns()
{
    printf(" Tests mixcolumns:\n");
    W4 state1[] = {0xdbaf2670,0xa2f28066,0x53546cf9,0x958769c0};
    printstate(state1);
    mixcolumns(state1);
    printstate(state1);
    W4 state2[] = {0x03020100,0x07060504,0x0b0a0908,0x0f0e0d0c};
    printstate(state2);
    mixcolumns(state2);
    printstate(state2);
    return;
}

void testkeyexpansion()
{
    printf(" Tests keyexpansion:\n");
    printf(" --- Key1 (AES128):\n");
    W4 key1[] = {0x16157e2b,0xa6d2ae28,0x8815f7ab,0x3c4fcf09};
    int nK1 = sizeof(key1)/sizeof(*key1);
    W4 keyDX1[4*(nK1+7)];
    keyexpansion(key1,keyDX1,nK1);
    printkeyDX(keyDX1,nK1);
    printf(" --- Key2 (AES192):\n");
    W4 key2[] = {0xf7b0738e,0x52640eda,0x2bf310c8,0xe5799080,0xd2eaf862,0x7b6b2c52};
    int nK2 = sizeof(key2)/sizeof(*key2);
    W4 keyDX2[4*(nK2+7)];
    keyexpansion(key2,keyDX2,nK2);
    printkeyDX(keyDX2,nK2);
    printf(" --- Key3 (AES256):\n");
    W4 key3[] = {0x10eb3d60,0xbe71ca15,0xf0ae732b,0x81777d85,0x072c351f,0xd708613b,0xa310982d,0xf4df1409};
    int nK3 = sizeof(key3)/sizeof(*key3);
    W4 keyDX3[4*(nK3+7)];
    keyexpansion(key3,keyDX3,nK3);
    printkeyDX(keyDX3,nK3);
    return;
}

void testcipher()
{
    printf(" Tests cipher:\n");
    W4 key1[] = {0x16157e2b,0xa6d2ae28,0x8815f7ab,0x3c4fcf09};
    int nK = sizeof(key1)/sizeof(*key1);
    W4 keyDX[4*(nK+7)];
    keyexpansion(key1,keyDX,nK);
    W4 state1[] = {0xdbaf2670,0xa2f28066,0x53546cf9,0x958769c0};
    printstate(state1);
    cipher(state1,keyDX,nK);
    printstate(state1);
    W4 state2[] = {0xa8f64332,0x8d305a88,0xa2983131,0x340737e0};
    printstate(state2);
    cipher(state2,keyDX,nK);
    printstate(state2);
    return;
}

void testuncipher()
{
    printf(" Tests uncipher:\n");
    W4 key1[] = {0x16157e2b,0xa6d2ae28,0x8815f7ab,0x3c4fcf09};
    int nK = sizeof(key1)/sizeof(*key1);
    W4 keyDX[4*(nK+7)];
    keyexpansion(key1,keyDX,nK);
    W4 state1[] = {0xdbaf2670,0xa2f28066,0x53546cf9,0x958769c0};
    printstate(state1);
    uncipher(state1,keyDX,nK);
    printstate(state1);
    W4 state2[] = {0xa8f64332,0x8d305a88,0xa2983131,0x340737e0};
    printstate(state2);
    uncipher(state2,keyDX,nK);
    printstate(state2);
    return;
}

void testencrypt()
{
    printf(" Tests encrypt:\n");
    char data[] = "[!] This is nothing more than a random and boring text. I don't even know why you're reading it...";
    int txtsize = 96;
    char key[] = "\x2b\x7e\x15\x16\x28\xae\xd2\xa6\xab\xf7\x15\x88\x09\xcf\x4f\x3c";
    int keysize = 16;
    aes_encrypt(data,txtsize,key,keysize);
    for(int i=0;i<txtsize;i++) putchar(data[i]);
    putchar('\n');
    return;
}

void testdecrypt()
{
    printf(" Tests decrypt:\n");
    char data[] = "[!] This is nothing more than a random and boring text. I don't even know why you're reading it...";
    int txtsize = 96;
    char key[] = "\x2b\x7e\x15\x16\x28\xae\xd2\xa6\xab\xf7\x15\x88\x09\xcf\x4f\x3c";
    int keysize = 16;
    aes_encrypt(data,txtsize,key,keysize);
    aes_decrypt(data,txtsize,key,keysize);
    for(int i=0;i<txtsize;i++) putchar(data[i]);
    putchar('\n');
    return;
}

void testzone()
{
    testsubbytes();
    testshiftrows();
    testmixcolumns();
    testkeyexpansion();
    testcipher();
    testuncipher();
    testencrypt();
    testdecrypt();
    return;
}

int main()
{
    testzone();
    return 0;
}

// EOF
