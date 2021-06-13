#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/sysinfo.h>

#define NUMBER_OF_ELEMENTS 98
#define NUMBER_OF_THREADS 4

double array[NUMBER_OF_ELEMENTS];

void * initialize_array (void *thread_number) {
        int *thread_id = (int *) thread_number;

        int i;
        int elements_per_thread = NUMBER_OF_ELEMENTS / NUMBER_OF_THREADS;
	int start = *thread_id * elements_per_thread;
        int end = start + elements_per_thread;
        
        for (i = start; i < end; i++) {
                array[i] = (double) i;
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

int main (int argc, char *argv[])
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
