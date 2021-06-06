CFLAGS = -Wall -Wextra -g

normal_numbers: normal_numbers.c
	gcc $(CFLAGS) $^ -o $@ -lgsl -lgslcblas

clean:
	rm normal_numbers
