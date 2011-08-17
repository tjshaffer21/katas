/*****************************************************************************
 *  RPN Calculator                                                           *
 *  15 August 2011                                                           *
 *                                                                           *
 *  Implement an RPN calculator that takes an expression like                *
 *  19 2.14 + 4.5 2 4.3 / - * which is usually expressed as (19 + 2.14) *    *
 *  (4.5 - 2 / 4.3) and responds with 85.2974. The program should read       *
 *  expressions from standard input and print the top of the stack to        *
 *  standard output when a newline is encountered. The program should retain *
 *  the state of the operand stack between expressions.                      *
 ****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stack.h"

#define BUFFERSIZE 10000

struct stack_struct* operand;

/******************************************************************************
 *  Check if the string is numerical.                                         *
 *  @param char* str                                                          *
 *  @return 0 if not a number, else 1.                                        *
 *****************************************************************************/
short is_digit(char* str) {
    short i = 0;
    char c;
    while((c=*str++)) {
        if(i==0 && c=='-') {
            i++;
            continue;
        }

        if(c == '.') {
            continue;
        }

        if(!isdigit(c)) {
            return 0;
        }
    }

    if(i == 1) { return 0; }
    return 1;
}

void perform_operation(char* pch) {
    double *r   = ((double *) pop(operand));
    double *l   = ((double *) pop(operand));
    double *res = (double *) malloc(sizeof(res));
            
    switch(pch[0]) {
        case '+':
            *res = *l + *r;
            push(operand,(void *) res);
            break;
        case '-':
            *res = *l - *r;
            push(operand,(void *) res);
            break;
        case '*':
            *res = *l * *r;
            push(operand,(void *) res);
            break;
        case '/':
            *res = *l / *r;
            push(operand, (void *) res);
            break;
        default:
            free((void *) res);
    }

    free((void *) r);
    free((void *) l);
}

void parse_input(char* input) {
    char *pch = strtok(input, " ");
    while(pch != NULL) {
        if(is_digit(pch)){
            double *v = malloc(sizeof(double));
            *v        = atof(pch);
            push(operand,v);
        }else {
            perform_operation(pch);
        }
        
        pch = strtok(NULL," ");
    }
}

int main(int argc, char* argv[]) {
    int i = 0;
    char buffer[BUFFERSIZE];

    operand  = init_stack(operand);
    
    printf("Enter expression: ");
    fgets(buffer, sizeof(buffer),stdin);

    parse_input(buffer);

    double *result = (double *) pop(operand);
    printf("= %.4f\n", *result);

    free(result);
    deinit_stack(operand);
    free(operand);
}

