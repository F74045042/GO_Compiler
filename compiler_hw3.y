/*	Definition section */
%{
#include "common.h" //Extern variables that communicate with lex

extern int yylineno;
extern int yylex();

FILE *file;

void yyerror(const char* error);

/* symbol table function */
int lookup_symbol(char *name);
void create_symbol(int index, char *name, int type, double data, int assign);
void insert_symbol(int index, char *name, int type, double data, int assign);
void set_symbol(char *name, double data);
double get_symbol(char *name);
int iord(char *name);
void dump_symbol();
void undec_symbol(char *name);
void writein(char* str);

/* Counting variable */
int varcnt = 0;
int redef = 0;
int ID_flag = 0; // 0 = const, 1 = ID
int label_cnt = 0; //count label
int exit_cnt = 0; //count exit
int if_cnt = 0; //count how many if

%}

%union {
    RULE_TYPE rule_type;
    int intVal;
}

/* Token definition */
%token INC DEC
%token MTE LTE EQ NE
%token <rule_type> ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token AND OR NOT
%token PRINT PRINTLN
%token IF ELSE FOR
%token VAR
%token QUOTA
%token NEWLINE

%token <rule_type> I_CONST
%token <rule_type> F_CONST
%token <rule_type> VOID INT FLOAT STRING ID

%type <rule_type> initializer expr equality_expr relational_expr
%type <rule_type> additive_expr multiplicative_expr prefix_expr postfix_expr
%type <rule_type> primary_expr constant
%type <rule_type> type

%type <intVal> add_op mul_op print_func_op assignment_op equality_op relational_op

%start program

%right ')' ELSE

/* Grammar section */
%%


program
    : program stat
    |
;

stat
    : declaration
    | compound_stat
    | expression_stat
    | print_func
    | selection_stat
;

declaration
    : VAR ID type '=' initializer NEWLINE
    {if(lookup_symbol($2.id)!=-1){
        yyerror("re-defined for variable");
        }else{create_symbol(varcnt, $2.id, $3.type, $5.f_val, 1);}}
    | VAR ID type NEWLINE
    {if(lookup_symbol($2.id)!=-1){
        yyerror("re-defined for variable");}
        else{writeCode("\tldc 0");
            create_symbol(varcnt, $2.id, $3.type, 0.0, 0);}}
;

type
    : INT
    | FLOAT
    | VOID
;

initializer
    : equality_expr
;

compound_stat
    : '{' '}'
    | '{' block_item_list '}'
;

block_item_list
    : block_item
    | block_item_list block_item
;

block_item
    : stat
;

selection_stat: IF '(' expr
    {
        if_cnt = label_cnt;
        char str[512];
        writeCode("\tisub");
        if($3.reg == EQ_t){
            sprintf(str, "\tifne Label_%d", label_cnt);
            writeCode(str);
        }else if($3.reg == NE_t){
            sprintf(str, "\tifeq Label_%d", label_cnt);
            writeCode(str);
        }else if($3.reg == LT_t){
            sprintf(str, "\tifge Label_%d", label_cnt);
            writeCode(str);
        }else if($3.reg == GT_t){
            sprintf(str, "\tifle Label_%d", label_cnt);
            writeCode(str);
        }else if($3.reg == LE_t){
            sprintf(str, "\tifgt Label_%d", label_cnt);
            writeCode(str);
        }else if($3.reg == GE_t){
            sprintf(str, "\tiflt Label_%d", label_cnt);
            writeCode(str);
        }
    } ')' stat
    {
        char str[512];
        sprintf(str, "\tgoto EXIT_%d", exit_cnt);
        writeCode(str);
    } else_stat
;

else_stat: ELSE
    {
        char str[512];
        sprintf(str, "Label_%d:", label_cnt);
        writeCode(str);
        if_cnt++;
        label_cnt++;
    }
    stat
    {
        char str[512];
        if(if_cnt == label_cnt){
            sprintf(str, "EXIT_%d:", exit_cnt);
            writeCode(str);
            if_cnt = 0;
            exit_cnt++;
        }
    }
    |
    {
        char str[512];
        sprintf(str, "Label_%d:", label_cnt);
        writeCode(str);
        sprintf(str, "EXIT_%d:", exit_cnt);
        writeCode(str);

        label_cnt++;
    }
;

expression_stat
    : expr NEWLINE
    | NEWLINE
;

expr
    : equality_expr
    | ID '=' expr
    {
        set_symbol($1.id, $3.f_val);
        int num = lookup_symbol($1.id);
        char str[512];
        sprintf(str, "\tistore %d", num);
        writeCode(str);
    }
    | prefix_expr assignment_op expr
    {
        if($2 == ADDASGN_t){
            $$.f_val = $1.f_val + $3.f_val;
            if($1.type == INT_t){
                set_symbol($1.id, (int)($3.f_val+$1.f_val));
                writeCode("\tiadd");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
            else if($1.type == FLOAT_t){
                $$.f_val = $1.f_val + $3.f_val;
                set_symbol($1.id, ($3.f_val+$1.f_val));
                writeCode("\tfadd");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
        }else if($2 == SUBASGN_t){
            $$.f_val = $1.f_val - $3.f_val;
            if($1.type == INT_t){
                set_symbol($1.id, (int)($3.f_val-$1.f_val));
                writeCode("\tisub");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
            else if($1.type == FLOAT_t){
                $$.f_val = $1.f_val - $3.f_val;
                set_symbol($1.id, ($3.f_val-$1.f_val));
                writeCode("\tfsub");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
        }else if($2 == MULASGN_t){
            $$.f_val = $1.f_val * $3.f_val;
            if($1.type == INT_t){
                set_symbol($1.id, (int)($3.f_val*$1.f_val));
                writeCode("\timul");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
            else if($1.type == FLOAT_t){
                $$.f_val = $1.f_val * $3.f_val;
                set_symbol($1.id, ($3.f_val*$1.f_val));
                writeCode("\tfmul");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
        }else if($2 == DIVASGN_t){
            $$.f_val = $1.f_val / $3.f_val;
            if($1.type == INT_t){
                set_symbol($1.id, (int)($3.f_val/$1.f_val));
                writeCode("\tidiv");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
            else if($1.type == FLOAT_t){
                $$.f_val = $1.f_val / $3.f_val;
                set_symbol($1.id, ($3.f_val/$1.f_val));
                writeCode("\tfdiv");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
        }else if($2 == MODASGN_t){
            $$.f_val = (int)$1.f_val % (int)$3.f_val;
            if($1.type == INT_t && $3.type == INT_t){
                set_symbol($1.id, ((int)$3.f_val%(int)$1.f_val));
                writeCode("\timod");
                int num = lookup_symbol($1.id);
                char str[512];
                sprintf(str, "\tistore %d", num);
                writeCode(str);
            }
            else if($1.f_val == 0.0 || $3.f_val == 0.0){
                yyerror("Can't MOD 0");
            }else{
                yyerror("Can't MOD float");
            }
        }
    }
;

assignment_op
    : ADDASGN {$$ = ADDASGN_t;}
    | SUBASGN {$$ = SUBASGN_t;}
    | MULASGN {$$ = MULASGN_t;}
    | DIVASGN {$$ = DIVASGN_t;}
    | MODASGN {$$ = MODASGN_t;}
;

equality_expr
    : relational_expr
    | equality_expr equality_op relational_expr
    {
        $$.reg = $2;
    }

;

equality_op
    : EQ { $$ = EQ_t; }
    | NE { $$ = NE_t; }
;

relational_expr
    : additive_expr
    | relational_expr relational_op additive_expr
    {
        $$.reg = $2;
    }
;

relational_op
    : '<' { $$ = LT_t; }
    | '>' { $$ = GT_t; }
    | LTE { $$ = LE_t; }
    | MTE { $$ = GE_t; }
;

additive_expr
    : multiplicative_expr
    | additive_expr add_op multiplicative_expr
    {
        if($2 == ADD_t){
            $$.f_val = $1.f_val + $3.f_val;
            if($1.type == $3.type){
                if($1.type == INT_t){
                    $$.type = INT_t;
                    writeCode("\tiadd");
                }else if($1.type == FLOAT_t){
                    $$.type = FLOAT_t;
                    writeCode("\tfadd");
                }
            }
            else{
                //cast
            }
        }else if($2 == SUB_t){
            $$.f_val = $1.f_val - $3.f_val;
            if($1.type == $3.type){
                if($1.type == INT_t){
                    $$.type = INT_t;
                    writeCode("\tisub");
                }else if($1.type == FLOAT_t){
                    $$.type = FLOAT_t;
                    writeCode("\tfsub");
                }
            }else{
                //case
            }
        }
    }
;

add_op
    : '+' {$$ = ADD_t;}
    | '-' {$$ = SUB_t;}
;

multiplicative_expr
    : prefix_expr
    | multiplicative_expr mul_op prefix_expr
    {
        if($2 == MUL_t){
            $$.f_val = $1.f_val*$3.f_val;
            if($1.type == $3.type){
                if($1.type == INT_t){
                    $$.type = INT_t;
                    writeCode("\timul");
                }else if($1.type == FLOAT_t){
                    $$.type = FLOAT_t;
                    writeCode("\tfmul");
                }
            }else{
                //cast
            }
        }else if($2 == DIV_t){
            if($3.f_val == 0.0){
                yyerror("Can't DIV 0");
            }
            $$.f_val = $1.f_val/$3.f_val;
            if($1.type == $3.type){
                if($1.type == INT_t){
                    $$.type = INT_t;
                    writeCode("\tidiv");
                }else if($3.type == FLOAT_t){
                    $$.type = FLOAT_t;
                    writeCode("\tfdiv");
                }
            }else{
                //cast
            }
        }else if($2 == MOD_t){
            if($1.type == FLOAT_t || $3.type == FLOAT_t){
                yyerror("Cant't MOD float");
            }
            if($3.f_val == 0.0){
                yyerror("Can't MOD 0");
            }
            $$.f_val = (int)$1.f_val%(int)$3.f_val;
            if($1.type == $3.type){
                $$.type = INT_t;
                writeCode("\timod");
            }else{
                //cast
            }
        }
    }
;

mul_op
    : '*' {$$ = MUL_t;}
    | '/' {$$ = DIV_t;}
    | '%' {$$ = MOD_t;}
;

prefix_expr
    : postfix_expr
    | INC prefix_expr
    {
        $$.f_val = $2.f_val + 1.0;
        writeCode("\tldc 1");
        char str[512];
        writeCode("\tiadd");
        sprintf(str, "\tistore %d", lookup_symbol($2.id));
        writeCode(str);
    }
    | DEC prefix_expr
    {
        $$.f_val = $2.f_val - 1.0;
        writeCode("\tldc 1");
        char str[512];
        writeCode("\tisub");
        sprintf(str, "\tistore %d", lookup_symbol($2.id));
        writeCode(str);
    }
;

postfix_expr
    : primary_expr
    | postfix_expr INC
    {
        $$.f_val = $1.f_val + 1.0;
        writeCode("\tldc 1");
        char str[512];
        writeCode("\tiadd");
        sprintf(str, "\tistore %d", lookup_symbol($1.id));
        writeCode(str);
    }
    | postfix_expr DEC
    {
        $$.f_val = $1.f_val - 1.0;
        writeCode("\tldc 1");
        char str[512];
        writeCode("\tisub");
        sprintf(str, "\tistore %d", lookup_symbol($1.id));
        writeCode(str);
    }
;

primary_expr
    : ID
    {
        $$.type = iord($1.id);
        ID_flag = 1;
        char str[512];
        sprintf(str, "\tiload %d", lookup_symbol($1.id));
        writeCode(str);
    }
    | constant
    {
        ID_flag = 0;
    }
    | '(' expr ')'
    {
        $$ = $2;
    }
;

constant
    : I_CONST
    {
        $$.type = INT_t;
        char str[512];
        sprintf(str, "\tldc %d", (int)$1.f_val);
        writeCode(str);
    }
    | F_CONST
    {
        $$.type = FLOAT_t;
        char str[512];
        sprintf(str, "\tldc %f", $1.f_val);
        writeCode(str);
    }
;

print_func
    : print_func_op '(' equality_expr ')' NEWLINE
    {
        writeCode("\tgetstatic java/lang/System/out Ljava/io/PrintStream;");
        writeCode("\tswap");
        if($1 == 0){
            if($3.type == INT_t){
                writeCode("\tinvokevirtual java/io/PrintStream/print(I)V");
            }
            else if($3.type == FLOAT_t){
                writeCode("\tinvokevirtual java/io/PrintStream/print(F)V");
            }
        }else if($1 == 1){
            if($3.type == INT_t){
                writeCode("\tinvokevirtual java/io/PrintStream/println(I)V");
            }
            else if($3.type == FLOAT_t){
                writeCode("\tinvokevirtual java/io/PrintStream/println(F)V");
            }
        }
    }
    | print_func_op '(' QUOTA STRING QUOTA ')' NEWLINE
    {
        char str[512];
        sprintf(str, "\tldc \"%s\"", $4.string);
        writeCode(str);
        writeCode("\tgetstatic java/lang/System/out Ljava/io/PrintStream;");
        writeCode("\tswap");
        if($1 == 0){
            writeCode("\tinvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V");
        }
        else if($1 == 1){
            writeCode("\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V");
        }
        printf("%s\n", $4.string);
    }
;

print_func_op
    : PRINT
    {
        $$ = 0;
    }
    | PRINTLN
    {
        $$ = 1;
    }
;

%%

/* C code section */

int main(int argc, char** argv)
{
    //setup code
    writeCode(".class public main");
    writeCode(".super java/lang/Object");
    writeCode(".method public static main([Ljava/lang/String;)V");
    writeCode(".limit stack 10");
    writeCode(".limit locals 10");

    yylineno = 0;
    yyparse();

    //setup code
    writeCode("\treturn");
    writeCode(".end method");

    //write .j file
    writeFile();

    dump_symbol();
    return 0;
}
void yyerror(const char* error){
    printf("<Error>: %s (line %d)\n", error, yylineno+1);
}
void create_symbol(int index, char *name, int type, double data, int assign) {
    if(!varcnt){
        printf("Create a symbol table\n");
    }
    insert_symbol(index, name, type, data, assign);
}
void insert_symbol(int index, char *name, int type, double data, int assign) {
    /*Not in hash table*/
    if(lookup_symbol(name) == -1){
        int key = hash(name);
        Node l = hashTable[key];
        while(l != NULL && (strcmp(l->name, name) != 0))
            l = l->down;
        /*variable not in the table*/
        if(l == NULL){
            l = malloc(sizeof(struct hashList));
            l->index = index;   //Index
            strcpy(l->name, name);  //ID
            l->type = type; //int or float
            l->f_data = data;
            l->assign = assign;
            l->down = hashTable[key];
            hashTable[key] = l;
        }
        /*variable in the table*/
        else{
            //save it's value maybe?
        }
        printf("Insert a symbol: %s\n", name);
        varcnt++;
        char store[100];
        if(type == INT_t){
            sprintf(store, "\tistore %d", index);
        }else if(type == FLOAT_t){
            sprintf(store, "\tfstore %d", index);
        }
        writeCode(store);
    }else{/*In hash table*/
        redef++;
    }
}
void set_symbol(char *name, double data){
    int key = hash(name);
    Node l = hashTable[key];
    while((l != NULL) && (strcmp(l->name, name) != 0)){
        l = l->down;
    }
    /*variable not in hash table*/
    if(l == NULL)
        printf("<Error> NULL set node.\n");
    /*variable in hash table return index*/
    else{
        l->f_data = data;
        l->assign = 1;
    }
}
double get_symbol(char *name){
    int key = hash(name);
    Node l = hashTable[key];
    while((l != NULL) && (strcmp(l->name, name) != 0)){
        l = l->down;
    }
    /*variable not in hash table*/
    if(l == NULL){
        //printf("<Error> NULL get node.\n");
        return -1;
    }
    /*variable in hash table return data*/
    else{
        return l->f_data;
    }
}
int iord(char *name){
    int key = hash(name);
    Node l = hashTable[key];
    while((l != NULL) && (strcmp(l->name, name) != 0)){
        l = l->down;
    }
    /*variable not in hash table*/
    if(l == NULL){
        //printf("<Error> NULL int or double.\n");
        return -1;
    }
    /*variable in hash table return index*/
    else{
        return l->type;
    }
}
int lookup_symbol(char *name) {
    int key = hash(name);
    Node l = hashTable[key];
    while((l != NULL) && (strcmp(l->name, name) != 0)){
        l = l->down;
    }
    /*variable not in hash table*/
    if(l == NULL)
        return -1;
    /*variable in hash table return index*/
    else
        return l->index;
}
void dump_symbol() {
    printf("Index\t ID\t Type\t\t Data\n");
    for(int i=0; i<SIZE; i++){
        if(hashTable[i] != NULL){
            Node l = hashTable[i];
            while(l != NULL){
                if(l->type == 1){
                        printf("%d \t %s \t int\t\t %d\n", l->index, l->name, (int)l->f_data);
                }else{
                        printf("%d \t %s \t float32\t %lf\n", l->index, l->name, l->f_data);
                }
                l = l->down;
            }
        }
    }
}
void writeCode(char* str){
    if(head == NULL){
        head = (Code *)malloc(sizeof(Code));
        head->code = (char *)malloc(512*sizeof(char));
        strcpy(head->code, str);
        head->next = NULL;
    }
    else{
        Code *tmp = head;
        while(tmp->next != NULL){
            tmp = tmp->next;
        }
        tmp->next = (Code *)malloc(sizeof(Code));
        tmp->next->code = (char *)malloc(512*sizeof(char));
        strcpy(tmp->next->code, str);
        tmp->next->next = NULL;
    }
}
void writeFile(){
    file = fopen("Computer.j","w");
    Code *tmp = head;
    while(tmp != NULL){
        fprintf(file, "%s\n", tmp->code);
        tmp = tmp->next;
    }
}
