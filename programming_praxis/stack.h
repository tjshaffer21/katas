#ifndef __STACK_H__
#define __STACH_H__
#include <stdlib.h>
#include <assert.h>

struct stack_struct {
    void* element;
    struct stack_struct* prev;
    int size;
};

/******************************************************************************
 *  Initialize stack structure.                                               *
 *  @note calls malloc                                                        *
 *  @return pointer to the top of the stack                                   *
 *****************************************************************************/
struct stack_struct* init_stack();

/******************************************************************************
 *  Deinitalize stack structure                                               *
 *  @note does not free the original stack pointer.                           *
 *****************************************************************************/
 void deinit_stack(struct stack_struct *stack);

/******************************************************************************
 * @param void* - Pointer to element to be pushed onto stack.                 *
 * @return 1 if success, else 0                                               *
 *****************************************************************************/
int push(struct stack_struct *stack, void*);

/******************************************************************************
 *  @return Pointer to first element, does not remove from stack.             *
 *****************************************************************************/
void* peek(struct stack_struct *stack);

/******************************************************************************
 *  @return Pointer to the first element, removes from stack.                 *
 *****************************************************************************/
void* pop(struct stack_struct *stack);

int get_size(struct stack_struct *stack);

#endif
