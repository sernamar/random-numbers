#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/sysinfo.h>
#include <sys/time.h>

#define PRINT_MAX 100 // don't print more than 100 elements
#define MEAN 0
#define STANDARD_DEVIATION 1

struct arguments {
        double *array;
        int start;
        int end;
        unsigned long seed;
};

void * initialize_array (void *arguments) {

        /* Get arguments */
        struct arguments *args = (struct arguments *) arguments;
        double *array = args->array;
        int start = args->start;
        int end = args->end;
        unsigned long seed = args->seed;

        /* Create a new random number generator */
        const gsl_rng_type *T;
        gsl_rng *r;

        gsl_rng_env_setup(); // set the default values for the random number generator variables

        T = gsl_rng_default;
        r = gsl_rng_alloc (T);
        gsl_rng_set(r, seed); // setup the generator using the seed we got from the main function

        /* Using the GSL library, initialize the array with normal random values */
        for (int i = start; i < end; i++) {
                array[i] = MEAN + gsl_ran_gaussian (r, STANDARD_DEVIATION);;
        }

        /* Free memory */
        gsl_rng_free (r);

        /* Exit thread when finished */
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

        int elements_per_thread = number_of_elements / number_of_threads;
        
        double *array = (double *) malloc(number_of_elements * sizeof(double));
        pthread_t *threads = (pthread_t *) malloc(number_of_threads * sizeof(pthread_t));
        struct arguments *args = (struct arguments *) malloc(number_of_threads * sizeof(struct arguments));;

        srand(time(NULL)); // need to to use a new seed each time we run the program
        
        // create threads
	for (i = 0; i < number_of_threads; i++) {
                args[i].array = array;
                args[i].start = i * elements_per_thread;
                if (i == number_of_threads - 1) { // in the last thread, process the remaining elements in case
                                                  // that NUMBER_OF_ELEMENTS is not divisible by NUMBER_OF_THREADS
                        args[i].end = number_of_elements;
                } else {
                        args[i].end = args[i]. start + elements_per_thread;
                }
                args[i].seed = random();

                // create the threads
                if(pthread_create(&threads[i], NULL, initialize_array, (void *) &args[i]) == -1) {
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
                printf("(Array size = %d. Printing only its first and last elements)\n", number_of_elements);
                printf("%f %f ... %f %f\n", array[0], array[1], array[number_of_elements -2], array[number_of_elements -1]);
        }

        // free memory
        free(array);
        free(threads);
	free(args);
        
        return 0;
}
