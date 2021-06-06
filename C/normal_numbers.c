#include <stdio.h>
#include <sys/time.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

int main (int argc, char *argv[])
{
        int number_of_elements = atoi(argv[1]);
        double standard_deviation = atof(argv[2]);
        double mean = atof(argv[3]);
        
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

        // print number_of_elements normal random numbers
        for (int i = 0; i < number_of_elements; i++) {
                double x = mean + gsl_ran_gaussian (r, standard_deviation);
                printf (" %f", x);
        }
        printf ("\n");
        
        gsl_rng_free (r);
  
        return 0;
}
