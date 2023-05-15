#define DEG_MAX_POL 255
#define DEG_MAX_W4 3
char hex[] = "0123456789abcdef";

typedef int pol [DEG_MAX_POL + 1];
typedef char gf256;
typedef char w4 [DEG_MAX_W4 + 1];



void gf256_showbin(gf256 pol);
void gf256_showhex(gf256 pol);
gf256 gf256_parse(char input[2]);
void pol_init(pol a);
int pol_deg(pol a);
void pol_copy(pol a, pol dest) ;
void pol_add(pol a, pol b, pol res);
void pol_sub(pol a, pol b, pol res);
void pol_mul(pol a, pol b, pol res);
gf256 gf256_add(gf256 pol1, gf256 pol2);
