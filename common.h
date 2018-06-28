#ifndef _COMMON_H_
#define _COMMON_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define SIZE 20
#define MAXTOKENLEN 40

typedef enum { VOID_t, INT_t, FLOAT_t ,STRING_t } SEMTYPE;
typedef enum { ADD_t, SUB_t, MUL_t, DIV_t, MOD_t, LT_t, LE_t, EQ_t, GE_t, GT_t, NE_t, AND_t, OR_t, NOT_t, NONE_t } OPERATOR;
typedef enum {ADDASGN_t, SUBASGN_t, MULASGN_t, DIVASGN_t, MODASGN_t} ASGN;
typedef struct rule_type
{
    int i_val, reg;
    double f_val;
    char* id;
    char* string;
    SEMTYPE type;
} RULE_TYPE;


/*hash function*/
static int hash( char * key){
    int tmp = 0;
    int i = 0;
    while(key[i] != '\0'){
        tmp = (tmp + key[i]) % SIZE;
        i++;
    }
    return tmp;
}
/*hash List*/
typedef struct hashList{
    int index;
    char name[MAXTOKENLEN]; //ID
    int type;   //int or float
    double f_data;
    int assign;
    struct hashList *down;
} *Node;

typedef struct codeLine{
    char *code;
    struct codeLine *next;
} Code;

static Node hashTable[SIZE];
Code *head;

#endif
