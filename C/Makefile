CFLAGS = -Wall -Wextra -g

normal_numbers-dev: normal_numbers.c
	gcc $(CFLAGS) $^ -o $@ -lgsl -lgslcblas

normal_numbers: normal_numbers.c
	gcc $^ -o $@ -lgsl -lgslcblas

normal_numbers_O3: normal_numbers.c
	gcc -O3 $^ -o $@ -lgsl -lgslcblas

clean:
	rm normal_numbers-dev normal_numbers optimazed_normal_numbers
