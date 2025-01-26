#ifndef FOX_PREPROC_H_
#define FOX_PREPROC_H_

#include <stdio.h>

typedef enum {
   ERR_EOF,
   ERR_UNEXPECTED,
} TokenizeError;

typedef enum {
   TOK_ERROR,
   TOK_IDENTIFIER,
   TOK_NUMBER,
   TOK_CHAR,
   TOK_STR,
   TOK_PLUS,
   TOK_MINUS,
   TOK_STAR,
   TOK_SLASH,
   TOK_PERCENT,
   TOK_BANG,
   TOK_AMPERSAND,
   TOK_PIPE,
   TOK_UP_CARET,
   TOK_DOUBLE_LCHEVRON,
   TOK_DOUBLE_RCHEVRON,
   TOK_PLUS_EQUAL,
   TOK_MINUS_EQUAL,
   TOK_STAR_EQUAL,
   TOK_SLASH_EQUAL,
   TOK_PERCENT_EQUAL,
   TOK_BANG_EQUAL,
   TOK_AMPERSAND_EQUAL,
   TOK_PIPE_EQUAL,
   TOK_UP_CARET_EQUAL,
   TOK_LBRACKET,
   TOK_RBRACKET,
   TOK_LPAREN,
   TOK_RPAREN,
   TOK_LBRACE,
   TOK_RBRACE,
   TOK_DOUBLE_AMPERSAND,
   TOK_DOUBLE_PIPE,
   TOK_LCHEVRON,
   TOK_RCHEVRON,
   TOK_LCHEVRON_EQUAL,
   TOK_RCHEVRON_EQUAL,
   TOK_EQUAL_EQUAL,
   TOK_EQUAL,
   TOK_TILDE,
   TOK_COMMA,
   TOK_COLON,
   TOK_SEMICOLON,
   TOK_ARROW,
   TOK_DOT,
   TOK_ELIPSES,
   TOK_MISC, // non-whitespace, non-comment character that isn't any of the above
} TokenType;

typedef struct {
   TokenType type;
   union {
      char char_data;
      char *str_data;
      TokenizeError err_data;
   };
} Token;

void tokenize(const char *src, size_t size);

#endif // !FOX_PREPROC_H_
