CC = gcc
FLAGS = -Wall -g
aes.out : main.o pol.o bits.o
	$(CC) $(FLAGS) -o $@ $^

main.o : main.c maths/pol/pol.h maths/bits/bits.h
	$(CC) $(FLAGS) -c $<

pol.o : maths/pol/pol.c maths/pol/pol.h
	$(CC) $(FLAGS) -c $< 

bits.o : maths/bits/bits.c maths/bits/bits.h
	$(CC) $(FLAGS) -c $< 

clean :
	rm *.o aes.out