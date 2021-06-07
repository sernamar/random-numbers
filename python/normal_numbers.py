import argparse

from numpy.random import default_rng

NUMBER_OF_SAMPLES = 1


def generate_normal_random_numbers(samples=1):
    rng = default_rng()
    return rng.standard_normal(samples)


if __name__ == '__main__':

    # parse arguments
    parser = argparse.ArgumentParser(
        description="Generate random numbers from the standard normal distribution")
    parser.add_argument("samples", type=int, nargs="?",
                        help="Number of samples")
    args = parser.parse_args()

    if args.samples:
        samples = args.samples
    else:
        samples = NUMBER_OF_SAMPLES

    # generate random numbers
    numbers = generate_normal_random_numbers(samples)
    print(numbers)
