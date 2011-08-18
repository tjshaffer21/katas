/******************************************************************************
 *  @name Rotate An Array                                                     *
 *  @modified 17 August 2011                                                  *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define BUFFER 1024

/******************************************************************************
 *  Iterate through a string and check if it is a numerical value.            *
 *  @param char* str                                                          *
 *  @return 0 if false, 1 if true                                             *
 *****************************************************************************/
short is_digit(char* str) {
    short i = 0;
    short j = 0;
    char c;
    while((c=*str++)) {
        if(c=='-') { 
            i++;
            if(i == 1) { return 0; }
            continue;
        }
            
        if(c == '.') { 
            j++;
            if(j > 1) { return 0; }
            continue;
        }

        if(!isdigit(c)) { return 0; }
    }

    return 1;
}

inline void rotate_a(int* iarray, short len, short rotate) {
    short i,tmp;
    if(rotate > len) { rotate = rotate % len; }

    for(i = 0; i <= len >> 1; i++) {
        tmp = iarray[i];
        iarray[i]        = iarray[i+rotate];
        iarray[i+rotate] = tmp;
    }
}

int main() {
    short i,len,rotate;
    char array[BUFFER];
    int *iarray = (int *)malloc(BUFFER*sizeof(int));
    
    printf("Array: ");
    fgets(array, BUFFER, stdin);
    
    i = 0;
    char* pch = strtok(array, " ");
    while(pch != NULL) {
        switch(is_digit(pch)) {
            case 1:
                iarray[i] = atoi(pch);
                i++;
        }

        pch = strtok(NULL, " \n");
    }

    len    = i;
    iarray = realloc(iarray, len);
   
    printf("Rotate: ");
    fgets(array, BUFFER, stdin);

    pch = strtok(array, " \n");
    if(is_digit(pch)) { rotate = atoi(pch); }

    rotate_a(iarray, len, rotate);

    for(i = 0; i < len; i++) {
        printf("%d ", iarray[i]);
    }
    printf("\n");
    
    free((void *) iarray);
    return 0;
}
