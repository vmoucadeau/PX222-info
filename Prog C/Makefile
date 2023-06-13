
CC = gcc
FLAGS = -g -Wall -Wextra # -lsan -lubsan
FLAGSOUT = -g -Wall -Wextra # -fsanitize=address -fsanitize=undefined -fsanitize=shift -fsanitize=shift-exponent -fsanitize=shift-base -fsanitize=integer-divide-by-zero -fsanitize=unreachable -fsanitize=vla-bound -fsanitize=null -fsanitize=return -fsanitize=signed-integer-overflow -fsanitize=bounds -fsanitize=bounds-strict -fsanitize=bool -fsanitize=enum

aes.out : main.o pol.o bits.o words.o state.o cipher.o keyexp.o text.o files.o api.o bmp.o
	$(CC) $(FLAGSOUT) -o $@ $^

main.o : main.c maths/pol/pol.h maths/bits/bits.h
	$(CC) $(FLAGS) -c $<

pol.o : maths/pol/pol.c maths/pol/pol.h
	$(CC) $(FLAGS) -c $< 

bits.o : maths/bits/bits.c maths/bits/bits.h
	$(CC) $(FLAGS) -c $< 

words.o : maths/words/words.c maths/words/words.h
	$(CC) $(FLAGS) -c $< 

state.o : algorithm/state/state.c algorithm/state/state.h
	$(CC) $(FLAGS) -c $< 

cipher.o : algorithm/cipher/cipher.c algorithm/cipher/cipher.h
	$(CC) $(FLAGS) -c $< 

keyexp.o : algorithm/keyexp/keyexp.c algorithm/keyexp/keyexp.h
	$(CC) $(FLAGS) -c $<

text.o : plugins/text/text.c plugins/text/text.h
	$(CC) $(FLAGS) -c $<

files.o : plugins/files/files.c plugins/files/files.h
	$(CC) $(FLAGS) -c $<

bmp.o : plugins/bmp/bmp.c plugins/bmp/bmp.h
	$(CC) $(FLAGS) -c $<

api.o : plugins/api/api.c plugins/api/api.h
	$(CC) $(FLAGS) -c $<

clean :
	rm *.o aes.out *ciph.bmp *ciph.txt
