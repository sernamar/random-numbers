CFLAGS = -Wall -Wextra -g

normal_numbers_dev: normal_numbers.c
	gcc $(CFLAGS) $^ -o $@ -lgsl -lgslcblas

normal_numbers: normal_numbers.c
	gcc $^ -o $@ -lgsl -lgslcblas

normal_numbers_O3: normal_numbers.c
	gcc -O3 $^ -o $@ -lgsl -lgslcblas

p_normal_numbers_dev: p_normal_numbers.c
	gcc $(CFLAGS) -lpthread $^ -o $@ -lgsl -lgslcblas

p_normal_numbers: p_normal_numbers.c
	gcc -lpthread $^ -o $@ -lgsl -lgslcblas

p_normal_numbers_O3: p_normal_numbers.c
	gcc -O3 -lpthread $^ -o $@ -lgsl -lgslcblas

openmp_normal_numbers: openmp_normal_numbers.c
	gcc -fopenmp $^ -o $@ -lgsl -lgslcblas

clean:
	rm normal_numbers_dev normal_numbers normal_numbers_O3 p_normal_numbers_dev p_normal_numbers p_normal_numbers_O3 openmp_normal_numbers
