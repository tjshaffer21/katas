/******************************************************************************
 *  Sieve Of Atkin                                                            *
 *  17 August 2011                                                            *
 *                                                                            *
 *  Atkin's sieve begins with a boolean array (bits are fine) of length n     *
 *  equal to the number of items to be sieved; each element of the array is   *
 *  initially false.                                                          *
 *                                                                            *
 *  Squarefree potential primes are marked like this: For each element k of   *
 *  the array for which k cr 1 (mod 12) or k cr 5 (mod 12) and there exists   *
 *  some 4x^2 + y^2 = k for positive integers x and y, flip the sieve element *
 *  (set true elements to false, and false elements to true). For each        *
 *  element k of the array for which k cr 7 (mod 12) and there exists some    *
 *  3x^2 + y^2 = k for positive integers x and y, flip the sieve element.     *
 *  For each element k of the array for which k cr 11 (mod 1) and there       *
 *  exists some 3x^2 - y^2 = k for positive integers x and y with x > y, flip *
 *  the sieve element.                                                        *
 *                                                                            *
 *  Once this preprocessing is complete, the actual sieving is performed by   *
 *  running through the sieve starting at 7. For each true element of the     *
 *  array, mark all multiples of the square of the element as false           *
 *  (regardless of their current setting). The remaining true elements are    *
 *  prime.                                                                    *
 *                                                                            *
 *  Your task is to write a function to sieve primes using the Sieve of Atkin.*
 *****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

inline char flip(char v) {
    (v == 0) ? (v = 1) :(v = 0);

    return v;
}

/******************************************************************************
 *  Algorithm:                                                                *
 *    For each element                                                        *
 *      1) If (k cr 1 (mod 12) || k cr 5 (mod 12)) && 4x^2 + y^2 = k for +x,  *
 *         +y, flip the sieve element.                                        *
 *      2) If k cr 7 (mod 12) && 3x^2 + y^2 = k for +x, +y, flip the element. *
 *      3) If k cr 11 (mod 12) && 3x^2 - y^2 = k for +x, +y where x > y, flip *
 *         the element.                                                       *
 *****************************************************************************/
void preprocess_list(char* list, long maximum) {
    long x,y;
    long ylimit, xlimit;
    long k,mod,xx,yy;

    ylimit = ceil(sqrt(maximum));
    xlimit = sqrt((maximum+1)/2);
    for(x = 0; x < xlimit; x++) {
        xx = x * x;

        for(y = 0; y < ylimit; y++) {
            yy = y * y;

            // 4x^2 + y^2
            k   = (xx << 2) + yy;
            mod = k % 12;
            if(k <= maximum && (mod == 1 || mod == 5)) {
                list[k] = flip(list[k]);
            }
            
            // 3x^2 + y^2
            k  -= xx;
            mod = k % 12;

            if(k <= maximum && mod == 7) {
                list[k] = flip(list[k]);
            }

            // 3x^2 - y^2;
            if(x > y) {
                k  -= (yy << 1);
                mod = k % 12;
                if(k <= maximum && mod == 11) {
                    list[k] = flip(list[k]);
                }
            }
        }
    }
}

inline void sieve(char* list, long maximum) {
    long i,j,k,ii;
    long sq = ceil(sqrt(maximum));
    for(i = 5; i < sq; i=i+2) {
        if(list[i]) {
            ii = i * i;
            k = 2;
            for(j = ii; j < maximum; j=k*ii) {
                if(j % i == 0) { list[j] = 0; }
                k++;
            }
        }
    }
}

/******************************************************************************
 *  Create the initial list.                                                  *
 *  @param long maximum - Upper range.                                        *
 *  @note Using char for lack of bit array.                                   *
 *  @note calls malloc                                                        *
 *  @return char*                                                             *
 *****************************************************************************/
inline char* create_list(long maximum) {
    long i;
    char *list = (char *)malloc(maximum * sizeof(char));

    list[0] = 0;
    list[1] = 0;
    list[2] = 1;
    list[3] = 1;
    list[4] = 0;
    for(i = 5; i < maximum; i++) {
        list[i] = 0;    // Set to false
    }

    return list;
}

int main(int argc, char* argv[]) {
    long i,n,count;
    char* list;

    switch(argc) {
        case 2:
            n = atol(argv[1]);
            n++;

            list = create_list(n);

            preprocess_list(list, n);
            sieve(list, n);
 /* 
            printf("2\n");
            for(i = 3; i < n; i=i+2) {
                if(list[i] == 1) { printf("%ld\n",i); }
            }
*/
            count = 1;
            for(i = 3; i < n; i=i+2) {
                if(list[i] == 1) { count++; }
            }
            printf("Count: %ld\n",count);

            free((void *)list);
            break;
        default:
            printf("%s [length]\n",argv[0]);
    }

    return 0;
}
