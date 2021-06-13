#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/sysinfo.h>
#include <sys/time.h>

#define NUMBER_OF_ELEMENTS 1000000000
#define NUMBER_OF_THREADS 4

#define MEAN 0
#define STANDARD_DEVIATION 1

double array[NUMBER_OF_ELEMENTS];

void * initialize_array (void *thread_number) {
        int *thread_id = (int *) thread_number;

        int i;
        int elements_per_thread = NUMBER_OF_ELEMENTS / NUMBER_OF_THREADS;
	int start = *thread_id * elements_per_thread;
        int end = start + elements_per_thread;

        // declare the necessary random number generator variables
        const gsl_rng_type *T;
        gsl_rng *r;

        // set the default values for the random number generator variables
        gsl_rng_env_setup();

        // create a seed based on time
        struct timeval tv;
        gettimeofday(&tv,0);
        unsigned long seed = tv.tv_sec + tv.tv_usec;

        // setup the generator using the seed just created
        T = gsl_rng_default;
        r = gsl_rng_alloc (T);
        gsl_rng_set(r, seed);
        
        for (i = start; i < end; i++) {
                array[i] = MEAN + gsl_ran_gaussian (r, STANDARD_DEVIATION);
        }

        // Initialize remaining elements if NUMBER_OF_ELEMENTS is not divisible by NUMBER_OF_THREADS
        if (end < NUMBER_OF_ELEMENTS) {
                start = end;
                end = NUMBER_OF_ELEMENTS;
                for (i = start;  i < end; i++) {
                        array[i] = (double) i;
                }
        }

        pthread_exit(NULL);
}

int main (void)
{
        pthread_t threads[NUMBER_OF_THREADS];
	int thread_id[NUMBER_OF_THREADS];
	int i;

        // create threads
	for (i = 0; i < NUMBER_OF_THREADS; i++) {
		thread_id[i] = i;
                if(pthread_create(&threads[i], NULL, initialize_array, (void *) &thread_id[i]) == -1) {
                        fprintf(stderr, "%s %d\n", "Can't create thread number", i);
                }
	}

	// wait until every thread ends
        for (i = 0; i < NUMBER_OF_THREADS; i++) {
		if (pthread_join(threads[i], NULL) == -1) {
			fprintf(stderr, "%s %d\n", "Can't join thread number", i);
		}
	}

        // print numbers
        if(NUMBER_OF_ELEMENTS <= 100) {
                for (i = 0; i < NUMBER_OF_ELEMENTS; i++) {
                        printf (" %f", array[i]);
                }
                printf("\n");
        } else {
                printf("Done.\n");
        }

        return 0;
}
