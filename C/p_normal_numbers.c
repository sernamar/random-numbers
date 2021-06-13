#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/sysinfo.h>
#include <sys/time.h>
#include <unistd.h>

#define PRINT_MAX 100 // don't print more than 100 elements

struct arguments {
        double *array;
        int number_of_elements;
        int number_of_threads;
        int thread_id;
};

void * initialize_array (void *arguments) {
        struct arguments *args = (struct arguments *) arguments;
        double *array = args->array;
        int number_of_elements = args->number_of_elements;
        int number_of_threads = args->number_of_threads;
        int elements_per_thread = number_of_elements / number_of_threads;
        int thread_id = args->thread_id;

	int start = thread_id * elements_per_thread;
        int end = start + elements_per_thread;

        for (int i = start; i < end; i++) {
                array[i] = i;
        }

        // initialize remaining elements if number_of_elements is not divisible by number_of_threads
        if ((thread_id == number_of_threads - 1) && (end < number_of_elements)) {
                start = end;
                end = number_of_elements;
                for (int i = start;  i < end; i++) {
                        array[i] = i;
                }
        }

        pthread_exit(NULL);
}

int main (int argc, char* argv[])
{
        int i;
        int number_of_elements;
        int number_of_threads;
        if (argc == 3) {
                number_of_elements = atoi(argv[1]);
                number_of_threads = atoi(argv[2]);
        } else {
                fprintf(stderr, "%s:\n", "Wrong number of arguments.");
                fprintf(stderr, "%s:\n", "Use: ./avoid_race_condition number_of_elements number_of_threads");
                exit(EXIT_FAILURE);
        }
        
        double *array = (double *) malloc(number_of_elements * sizeof(double));
        pthread_t *threads = (pthread_t *) malloc(number_of_elements * sizeof(pthread_t));
        struct arguments *args;
        
        // create threads
	for (i = 0; i < number_of_threads; i++) {
                // the calling thread uses a new data structure for each thread,
                // so we can be sure that the args variable cannot be changed by other threads,
                // avoiding race conditions
                args = (struct arguments *) malloc(sizeof(struct arguments));
                args->array = array;
                args->number_of_elements = number_of_elements;
                args->number_of_threads = number_of_threads;
                args->thread_id = i;

                // create the threads
                if(pthread_create(&threads[i], NULL, initialize_array, (void *) args) == -1) {
                        fprintf(stderr, "%s %d\n", "Can't create thread number", i);
                }
	}

	// wait until every thread ends
        for (i = 0; i < number_of_threads; i++) {
		if (pthread_join(threads[i], NULL) == -1) {
			fprintf(stderr, "%s %d\n", "Can't join thread number", i);
		}
	}

        // print numbers
        if(number_of_elements <= PRINT_MAX) {
                for (i = 0; i < number_of_elements; i++) {
                        printf (" %f", array[i]);
                }
                printf("\n");
        } else {
                printf("Done.\n");
        }

        return 0;
}
