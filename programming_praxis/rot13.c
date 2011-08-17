/******************************************************************************
 *  ROT13                                                                     *
 *  16 August 2011                                                            *
 *                                                                            *
 *  Write a function that takes a string and returns the ROT13 version of the *
 *  string; you may assume that the character set is ascii. What is the       *
 *  meaning of "Cebtenzzvat Cenkvf vf sha!"                                   *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

inline void convert(char* str, int len) {
    int i;

    inline char get_new_value(char val, int up_low) {
        if(val-13 >= up_low) {
            val -= 13;
        } else {
            val += 13;
        }
        
        return val;
    }

    for(i = 0; i < len; i++) {
        if(isalpha(str[i])) {
            if(isupper(str[i])) {
                str[i] = get_new_value(str[i], 65);
            } else {
                str[i] = get_new_value(str[i], 97);
            }
        }
    }
}


int main(int argc, char* argv[]) {
    if(argc >= 2) {
        int len;
        int i;
        for(i = 1; i < argc; i++) {
            len = strlen(argv[i])+1;
            char str_c[len];
            strcpy(str_c, argv[i]);
            convert(str_c,len);
            printf("%s ", str_c);
        }
        printf("\n");
    } else {
        printf("%s [ascii string]\n",argv[0]);
    }

    return 0;
}
