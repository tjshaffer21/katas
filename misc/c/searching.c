#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/******************************************************************************
 *  Perform binary search on an array.                                        *
 *  @param int* sorted_array                                                  *
 *  @param int length                                                         *
 *  @param int value                                                          *
 *                                                                            *
 *  Algorithm:                                                                *
 *      1. Divide the array at the mid-point.                                 *
 *      2.                                                                    *
 *          a) If mid_value == value, return true                             *
 *          b) If mid_value < value, binary_search(array[mid+1...]...)        *
 *          c) If mid_value > value, binary_search(array[mid-1...]...)        *
 *          d) If length == 1, if array[0] == value then true else false      *
 *****************************************************************************/
int binary_search(int* sorted_array,int length,int value) {
    if(length == 2) {
        if(sorted_array[0] == value) {
            return 1;
        }
        
        if(sorted_array[1] == value) {
            return 1;
        }

        return 0;
    } else if(length == 1) {
        if(sorted_array[0] == value) {
            return 1;
        }
        
        return 0;
    } else if(length <= 0) {
        return 0;
    }

    int i            = 0;
    int j            = 0;
    int mid_position = floor(length / 2);
    int mid_value    = sorted_array[mid_position-1];
    int* array;

    
    if(mid_value == value) {
        return 1;
    }

    array = (int *) malloc(mid_position * sizeof(int));
    if(array == NULL) {
        return -1;
    }

    if(mid_value < value) {
        for(i = mid_position+1; i < length; i++) {
            array[j] = sorted_array[i];
            j++;
        }
    } else if(mid_value > value) {
        for(i = 0; i <= mid_position-1; i++) {
            array[i] = sorted_array[i];
            j++;
        }
    }

    int result = binary_search(array, j, value);
    free((void *) array);

    return result;
}

int main(int argc, char* argv[]) {
    int array_length   = 108;
    int sorted_array[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,150,300,400,500,600,700,800,900};

    printf("%d\n",binary_search(sorted_array,array_length,50));     // 1
    printf("%d\n",binary_search(sorted_array,array_length,901));    // 0
    printf("%d\n",binary_search(sorted_array,array_length,1));      // 1
    printf("%d\n",binary_search(sorted_array,array_length,0));      // 0
    printf("%d\n",binary_search(sorted_array,array_length,-1));     // 0
    printf("%d\n",binary_search(sorted_array,array_length,54));     // 1
    printf("%d\n",binary_search(sorted_array,array_length,900));    // 1
    return 0;
}
