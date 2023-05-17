CC = gcc
FLAGS = -Wall -g
aes.out : main.o pol.o bits.o words.o state.o cipher.o keyexp.o
	$(CC) $(FLAGS) -o $@ $^

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

clean :
	rm *.o aes.out