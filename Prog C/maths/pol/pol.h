#define DEG_MAX_POL 30
#define DEG_MAX_W4 3


typedef int pol [DEG_MAX_POL + 1];
typedef unsigned char gf256;

int search_hexval(char val);

void pol_init(pol a);
void pol_show(pol a);
int pol_deg(pol a);
void pol_copy(pol a, pol dest) ;
void pol_add(pol a, pol b, pol res);
void pol_sub(pol a, pol b, pol res);
void pol_mul(pol a, pol b, pol res);

void gf256_showbin(gf256 pol);
void gf256_showhex(gf256 pol);
void gf256_getstr(gf256 pol, char res[2]);
gf256 gf256_parse(char input[2]);
int gf256_deg(gf256 pol);
gf256 gf256_add(gf256 pol1, gf256 pol2);
gf256 gf256_mulbyx(gf256 pol1);
gf256 gf256_mulbyxpower(gf256 pol, int power);
gf256 gf256_mul(gf256 pol1, gf256 pol2);