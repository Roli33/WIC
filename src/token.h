#ifndef SRC_TOKEN_H
#define SRC_TOKEN_H

typedef struct TOKEN_TYPES
{
    char* val;
    enum type
    {
        TokId,
        TokEq,
        TokParOpen,
        TokParClose,
        TokComma,
        TokAssign,
        TokCurlyOpen,
        TokCurlyClose,
        TokInt
    };
} Token_t;
 
Token_t* init(char* val, Token_t::type type);

#endif
