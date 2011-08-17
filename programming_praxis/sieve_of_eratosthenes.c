/******************************************************************************
 *  @name Sieve of Eratosthenes                                               *
 *  @updated 17 August 2011                                                   *
 *                                                                            *
 *  Algorithm:                                                                *
 *      1) Array from 0..n, odd pos. set to TRUE, even pos. set to FALSE      *
 *         except for 2.                                                      *
 *      2) Starting at the square(x), cross off all multiples.                *
 *      3) Stop at sqrt(n)                                                    *
 *                                                                            *
 *  @see README for more details.                                             *
 *****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

/******************************************************************************                                               *
 *  @note Even is set to 0, Odds set to 1 (except 2)                          *
 *  @note Calls malloc.                                                       *
 *  @param long size                                                          *
 *  @return char* pointer to the list.                                        *
 *****************************************************************************/
inline char* fill_list(long size) {
    long i;
    char* list = (char *) malloc(size*sizeof(char));
    assert(list != NULL);

    list[0] = 0;
    list[1] = 0;
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

    switch(argc) {
        case 2:
            maximum = atol(argv[1]);
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
            break;
        default:
            printf("%s [maximum]\n",argv[1]);
    }
    
    return 0;
}
