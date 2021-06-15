 #include <omp.h>
#include <stdio.h>
#include <sys/time.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#define NUMBER_OF_ELEMENTS 1
#define MEAN 0
#define STANDARD_DEVIATION 1

int main (int argc, char *argv[])
{
        int number_of_elements;
        
        if (argc == 1) {
                number_of_elements = NUMBER_OF_ELEMENTS;
        } else if (argc == 2) {
                number_of_elements = atoi(argv[1]);
        } else {
                fprintf(stderr, "%s\n", "Wrong number of arguments.");
                fprintf(stderr, "%s\n", "Use: ./normal_numbers number_of_elements");
                exit(EXIT_FAILURE);
        }

        double *numbers = malloc (number_of_elements * sizeof(double));

        double mean = MEAN;
        double standard_deviation = STANDARD_DEVIATION;
        
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

        // generate number_of_elements normal random numbers
        #pragma omp parallel for
        for (int i = 0; i < number_of_elements; i++) {
                numbers[i] = mean + gsl_ran_gaussian (r, standard_deviation);
        }

        // print numbers
        if(number_of_elements < 10) {
                for (int i = 0; i < number_of_elements; i++) {
                        printf (" %f", numbers[i]);
                }
                printf("\n");
        } else {
                printf("(Array size = %d. Printing only its first and last elements)\n", number_of_elements);
                printf("%f %f ... %f %f\n", numbers[0], numbers[1], numbers[number_of_elements -2], numbers[number_of_elements -1]);
        }
        
        // free memory        
        gsl_rng_free (r);
        free(numbers);
  
        return 0;
}
