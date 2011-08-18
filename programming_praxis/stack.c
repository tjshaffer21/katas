#include "stack.h"

struct stack_struct *init_stack(struct stack_struct * stack) {
    stack = (struct stack_struct *)
      malloc(sizeof(struct stack_struct));

    assert(stack != NULL);

    stack->element = NULL;
    stack->prev    = NULL;
    stack->size    = 0;

    return stack;
}

void deinit_stack(struct stack_struct *stack) {
    if(stack->size > 0) {
        while(stack->size > 0) {
            pop(stack);
        }
    }

//    free((void*) stack);
}

void push(struct stack_struct *stack, void* ele) {
    if(stack->size == 0) {
        stack->element = ele;
        stack->size    = 1;
    } else {
        struct stack_struct *s = (struct stack_struct *)
          malloc(sizeof(struct stack_struct));

        assert(s != NULL);
        
        s->element = stack->element;
        s->size    = stack->size;
        s->prev    = stack->prev;
        
        stack->element = ele;
        stack->size++;
        stack->prev = s;
    }
}

void* peek(struct stack_struct *stack) {
    return stack->element;
}

void* pop(struct stack_struct *stack) {
    void* element = 0x0;
    if(stack->size > 1) {
        struct stack_struct *s = stack->prev;
        element = stack->element;
        stack->element = s->element;
        stack->size    = s->size;
        stack->prev    = s->prev;

        s->prev    = NULL;
        s->element = NULL;

        free((void *)s);

    } else if(stack->size == 1) {
        element = stack->element;
        stack->element = NULL;
        stack->size--;
    }

    return element;
}

size_t get_size(struct stack_struct *stack) { return stack->size; }

