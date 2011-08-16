/******************************************************************************
 *  Sieve of Eratosthenes                                                     *
 *  16 August 2011                                                            *
 *                                                                            *
 *  The Sieve of Eratosthenes starts by making a list of all the numbers up to*
 *  a desired maximum; we'll illustrate the method by calculating the prime   *
 *  numbers through thirty:                                                   *
 *  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28  *
 *  29 30                                                                     *
 *  Now take the first number of the list, 2, and cross off every second num- *
 *  ber:                                                                      *
 *  2 3 5 7 9 11 13 15 17 19 21 23 25 27 29                                   *
 *  Next, take the next number on the list that isn't crossed off, and cross  *
 *  off every third number; some of them have already been crossed off:       *
 *  2 3 5 7 11 13 17 19 23 25 29                                              *
 *  Repeat that last step for the next un-crossed number on the list, 5:      *
 *  2 3 5 7 11 13 17 19 23 29                                                 *
 *                                                                            *
 *  This method is called a sieve because it sweeps through a range of num-   *
 *  bers, with each prime number, as it is discovered, blocking all its multi-*
 *  ples from falling through as prime numbers. The sieve admits several opti-*
 *  mizations. First, only odd numbers are considered, since the initial      *
 *  shifting crosses off all the even numbers except 2, which is handled      *
 *  seperately. Second, crossing off starts at the square of the number being *
 *  sifted, since all smaller primes have been crossed off by previous steps  *
 *  of the sieve; for instance, sifting by 3 starts at 9 since 6 was crossed  *
 *  off when sifting by 2. Third, sifting stops at the square root of the     *
 *  maximum number in the sieve, since any non-primes larger than the square  *
 *  root must have already been crossed off at previous levels of the sieve;  *
 *  thus, in the above example there is no need to sieve on the prime number  *
 *  7, or any larger prime number, since the square of 7 is greater than 30,  *
 *  which is the largest number in the list.                                  *
 *                                                                            *
 *  Write a function that takes a single argument n and returns a list of     *
 *  prime numbers less than or equal to n using the optimized sieve algorithm *
 *  described above. Apply the function to the argument 15485863 and count the*
 *  number of primes returned.                                                *
 *****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

/******************************************************************************
 *  Fill the list from 2..size.                                               *
 *  @note Only fills with odd values (limit = 30, size = (limit/2)+1          *
 *  @param long size                                                          *
 *  @return long* pointer to the list.                                        *
 *****************************************************************************/
long* fill_list(long size) {
    long i = 0;
    long* list = (long *) malloc(size*sizeof(long));
    assert(list != NULL);

    list[0] = 2;
    for(i = 1; i < size; i++) {
        list[i] = (i*2)+1;
    }

    return list;
}

void print_list(long *list, long length) {
    long i = 0;
    for(i = 0; i < length; i++) {
        printf("%ld ", list[i]);
    }
    printf("\n");
}

/******************************************************************************
 *  Shift removed values (-1) to the end of the list.                         *
 *  @param long* list                                                         *
 *  @param long length                                                        *
 *****************************************************************************/
void shift_list(long* list, long length) {
    long i = 0;
    long j = 0;

    while(i < length) {
        if(list[i] == -1) {
            for(j = i; j < length-1; j++) {
                list[j]   = list[j+1];
                list[j+1] = -1;
            }
        }
        i++;
    }
}

/*****************************************************************************
 *  Create a new list, copy all valid entries over.                          *
 *  @param long* list                                                        *
 *  @param long size                                                         *
 *  @note assumes that list has already been shifted.                        *
 *  @note malloc on the new list.                                            *
 *  @note free on the old list.                                              *
 *  @return pointer to the new list.                                         *
 ****************************************************************************/
long* create_new_list(long* list, long size) {
    long* new_list = (long *)malloc(size*sizeof(long));
    assert(new_list != NULL);

    long i = 0;
    for(i = 0; i < size; i++) {
        if(list[i] == -1) { break; }
        new_list[i] = list[i];
    }
    
    free(list);

    return new_list;
}

/******************************************************************************
 *  Iterates through the list and crosses off (set to -1) multiples of x      *
 *  @param long* list                                                         *
 *  @param long length                                                        *
 *  @param long start - Where to start in the array.                          *
 *  @param long num   - Value of the current location (x).                    *
 *  @return long      - Number of values crossed off.                         *
 *****************************************************************************/
long cross_off_list(long* list, long length, long start, long num) {
    long i = 0;
    long j = 0;
    for(i = start; i < length; i++) {
        if(list[i] % num == 0) {
            list[i] = -1;
            j++;
        }
    }

    return j;
}

/******************************************************************************
 *  Find the position of a given value in the array.                          *
 *  @param long* list                                                         *
 *  @param long length                                                        *
 *  @param long value                                                        *
 *  @return long - The position of the value if found, else -1.               *
 *****************************************************************************/
long find_position(long* list, long length, long value) {
    long i = 0;
    for(i = 0; i < length; i++) {
        if(list[i] == value) { return i; }
    }

    return -1;
}

int main(int argc, char* argv[]) {
    long maximum;//, size;
    long size;
    long stop,i;
    long num_removed;
    long* list;
    long value, pos;

    if(argc == 2) {
        maximum = atol(argv[1]);    // Upper bound
        size    = ceil(((double) maximum)/2.0);
        
        list = fill_list(size);
        
        // Run sieve algorithm.
        i = 0;
        stop = floor(sqrt(maximum));
        while((i+2) <= stop) {
            // Start at square(x)
            pos = find_position(list,size,pow(i+2,2));
            
            if(pos > -1) {
                num_removed = cross_off_list(list, size, pos, i+2);
                shift_list(list, size);
                size -= num_removed;
                //list  = create_new_list(list,size);           
            }

            i++;
        }

        // Final output
        //print_list(list,size);
        printf("Count: %ld\n", size);

        free((void *) list);
    }else {
        printf("./%s [maximum]\n",argv[1]);
    }
    
    return 0;
}

