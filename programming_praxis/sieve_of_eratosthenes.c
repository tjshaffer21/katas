/******************************************************************************
 *  Sieve of Eratosthenes                                                     *
 *  16 August 2011                                                            *
 *                                                                            *
 *  Algorithm:                                                                *
 *      1) Fill an array from 0..n set to TRUE (1).                           *
 *      2) Starting at the square(x), cross off all multiples.                *
 *      3) Stop at sqrt(n)                                                    *
 *                                                                            *
 *  Compared to previous version:                                             *
 *      - Uses a list of chars and does not resize.                           *
 *      - Fill list sets all evens, except 2, to 0 (prev. did not add evens)  *
 *      - Loop skips evens (known to not be prime)                            *
 *                                                                            *
 *  Write a function that takes a single argument n and returns a list of     *
 *  prime numbers less than or equal to n using the optimized sieve algorithm *
 *  described above. Apply the function to the argument 15485863 and count the*
 *  number of primes returned.                                                *
 *****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

/******************************************************************************
 *  Fill the list from 2..size.                                               *
 *  @note Only fills with odd values (limit = 30, size = (limit/2)+1          *
 *  @param long size                                                          *
 *  @return char* pointer to the list.                                        *
 *****************************************************************************/
inline char* fill_list(long size) {
    long i;
    char* list = (char *) malloc(size*sizeof(char));
    assert(list != NULL);

    list[0] = 0;    // Ignore
    list[1] = 0;    // Ignore
    list[2] = 1;
    for(i = 3; i < size; i++) {
        ((i % 2) == 0) ? (list[i] = 0) : (list[i] = 1);
    }

    return list;
}

inline void print_list(char *list, long length) {
    long i;
    long count = 1;     // Know 2 is prime
    for(i = 3; i < length; i=i+1) {
        if(list[i] == 1) {
            count++;
        }
    }

    printf("Count: %ld\n", count);
}

/******************************************************************************
 *  Iterates through the list and crosses off (set to 0) multiples of x       *
 *  @param char* list                                                         *
 *  @param long length                                                        *
 *  @param long start - Where to start in the array.                          *
 *  @param long num   - Value of the current location (x).                    *
 *****************************************************************************/
inline void cross_off_list(char* list, long length, long start, long num) {
    long i;

    for(i = start; i < length; i=i+2) {
        if(i % num == 0) { list[i] = 0; }
    }
}

int main(int argc, char* argv[]) {
    long maximum;
    long stop,i,pos;
    char* list;

    if(argc == 2) {
        maximum = atol(argv[1]);    // Upper bound
        maximum++;
        list = fill_list(maximum);
        
        i = 3;
        stop = sqrt(maximum);
        while(i <= stop) {
            pos = i*i;
            cross_off_list(list, maximum, pos, i);
            i += 2;
        }

        print_list(list,maximum);

        free((void *) list);
    }else {
        printf("%s [maximum]\n",argv[1]);
    }
    
    return 0;
}

