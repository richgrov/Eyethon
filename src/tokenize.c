#include "tokenize.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chars.h"
#include "config.h"

static void token_free(Token *tok) {
   switch (tok->type) {
   case TOK_IDENTIFIER:
   case TOK_CHAR:
   case TOK_NUMBER:
   case TOK_STR:
      free(tok->str_data);
   default:
      break;
   }
}

static Token operator_token(TokenType type) {
   Token result = {.type = type};
   return result;
}

static Token unexpected_char_token() {
   Token result = {.type = TOK_ERROR, .err_data = ERR_UNEXPECTED};
   return result;
}

typedef struct {
   const char *src;
   size_t size;
   size_t read_index;
} Preprocessor;

static char try_line_splice(Preprocessor *proc) {
   if (proc->read_index + 2 >= proc->size) {
      return '\0';
   }

   if (proc->src[proc->read_index] != '\\' || proc->src[proc->read_index + 1] != '\n') {
      return '\0';
   }

   return proc->src[proc->read_index + 2];
}

static char peek(Preprocessor *proc) {
   if (proc->read_index >= proc->size) {
      return '\0';
   }

   char next_line_char = try_line_splice(proc);
   if (next_line_char != '\0') {
      return next_line_char;
   }

   return proc->src[proc->read_index];
}

static char next(Preprocessor *proc) {
   if (proc->read_index >= proc->size) {
      return '\0';
   }

   char next_line_char = try_line_splice(proc);
   if (next_line_char != '\0') {
      proc->read_index += 3;
      return next_line_char;
   }

   return proc->src[proc->read_index++];
}

static bool is_whitespace(char c) {
   switch (c) {
   case ' ':
   case '\t':
   case '\n':
   case VERTICAL_TAB:
   case FORM_FEED:
      return true;
   default:
      return false;
   }
}

static void skip_whitespace(Preprocessor *proc) {
   while (proc->read_index < proc->size && is_whitespace(proc->src[proc->read_index])) {
      ++proc->read_index;
   }
}

static Token number(Preprocessor *proc, char first) {
   char number[128] = {first};
   int number_index = 1;

   bool prev_is_e = false;

   while (proc->read_index < proc->size) {
      // TODO: limit to 128 chars
      char c = peek(proc);

      bool is_plus_minus = prev_is_e && (c == '+' || c == '-');
      if (!is_alpha(c) && !is_digit(c) && c != '_' && c != '.' && !is_plus_minus) {
         break;
      }

      prev_is_e = c == 'e' || c == 'E';
      number[number_index++] = c;
      next(proc);
   }

   char *str = malloc(number_index + 1);
   memcpy(str, number, number_index + 1);

   Token result = {
      .type = TOK_NUMBER,
      .str_data = str,
   };
   return result;
}

static int escape_sequence(Preprocessor *proc, char *str_buf, int buf_size) {
   char c = next(proc);
   switch (c) {
   case '\'':
   case '"':
   case '?':
   case '\\':
   case 'a':
   case 'b':
   case 'f':
   case 'n':
   case 'r':
   case 't':
   case 'v':
      if (buf_size > 0) {
         *str_buf = c;
         return 1;
      }
      return 0;
   }

   int octal_count = 0;
   do {
      if (is_octal_digit(c)) {
         if (++octal_count > buf_size) {
            return 0;
         }

         *str_buf++ = c;
         c = next(proc);
      }
   } while (octal_count <= 3);

   if (octal_count > 0) {
      return octal_count;
   }

   if (c != 'x') {
      return -c;
   }

   int hex_count = 0;
   for (c = peek(proc); is_hex_digit(c); c = peek(proc)) {
      next(proc);
      if (++hex_count > buf_size) {
         return 0;
      }

      *str_buf++ = c;
   }

   return hex_count;
}

static Token
quoted_literal(Preprocessor *proc, char terminator, TokenType result_type, bool allow_escape) {
   char contents[FOX_MAX_STR_LITERAL_LEN] = {0};
   int contents_len = 0;

   for (char c = next(proc); c != terminator; c = next(proc)) {
      // TODO length limit
      contents[contents_len++] = c;

      if (allow_escape && c == '\\') {
         int len = escape_sequence(proc, contents + contents_len, sizeof(contents) - contents_len);
         if (len == 0) {
            return unexpected_char_token();
         }

         contents_len += len;
         continue;
      }

      if (!is_alpha(c) && !is_digit(c) && !is_source_symbol(c)) {
         return unexpected_char_token();
      }
   }

   char *heap_contents = malloc(contents_len + 1);
   memcpy(heap_contents, contents, contents_len + 1);

   Token result = {
      .type = result_type,
      .str_data = heap_contents,
   };
   return result;
}

static Token identifier(Preprocessor *proc, char first) {
   char identifier[128] = {first};
   int identifier_index = 1;

   while (proc->read_index < proc->size) {
      // TODO: limit to 128 chars
      char c = peek(proc);
      if (!is_alpha(c) && !is_digit(c) && c != '_') {
         break;
      }

      identifier[identifier_index++] = c;
      next(proc);
   }

   switch (identifier[0]) {
   case 'a':
      if (strcmp(identifier, "as") == 0) {
         return operator_token(TOK_AS);
      } else if (strcmp(identifier, "assert") == 0) {
         return operator_token(TOK_ASSERT);
      }
      break;
   case 'b':
      if (strcmp(identifier, "break") == 0) {
         return operator_token(TOK_BREAK);
      } else if (strcmp(identifier, "breakpoint") == 0) {
         return operator_token(TOK_BREAKPOINT);
      }
      break;
   case 'c':
      if (strcmp(identifier, "class") == 0) {
         return operator_token(TOK_CLASS);
      } else if (strcmp(identifier, "class_name") == 0) {
         return operator_token(TOK_CLASS_NAME);
      } else if (strcmp(identifier, "const") == 0) {
         return operator_token(TOK_CONST);
      } else if (strcmp(identifier, "continue") == 0) {
         return operator_token(TOK_CONTINUE);
      }
      break;
   case 'e':
      if (strcmp(identifier, "elif") == 0) {
         return operator_token(TOK_ELIF);
      } else if (strcmp(identifier, "else") == 0) {
         return operator_token(TOK_ELSE);
      } else if (strcmp(identifier, "enum") == 0) {
         return operator_token(TOK_ENUM);
      } else if (strcmp(identifier, "extends") == 0) {
         return operator_token(TOK_EXTENDS);
      }
      break;
   case 'f':
      if (strcmp(identifier, "false") == 0) {
         return operator_token(TOK_FALSE);
      } else if (strcmp(identifier, "for") == 0) {
         return operator_token(TOK_FOR);
      } else if (strcmp(identifier, "func") == 0) {
         return operator_token(TOK_FUNC);
      }
      break;
   case 'i':
      if (strcmp(identifier, "if") == 0) {
         return operator_token(TOK_IF);
      } else if (strcmp(identifier, "in") == 0) {
         return operator_token(TOK_IN);
      } else if (strcmp(identifier, "is") == 0) {
         return operator_token(TOK_IS);
      }
      break;
   case 'm':
      if (strcmp(identifier, "match") == 0) {
         return operator_token(TOK_MATCH);
      }
      break;
   case 'n':
      if (strcmp(identifier, "null") == 0) {
         return operator_token(TOK_NULL);
      }
      break;
   case 'p':
      if (strcmp(identifier, "pass") == 0) {
         return operator_token(TOK_PASS);
      }
      break;
   case 'r':
      if (strcmp(identifier, "return") == 0) {
         return operator_token(TOK_RETURN);
      }
      break;
   case 's':
      if (strcmp(identifier, "self") == 0) {
         return operator_token(TOK_SELF);
      } else if (strcmp(identifier, "signal") == 0) {
         return operator_token(TOK_SIGNAL);
      } else if (strcmp(identifier, "static") == 0) {
         return operator_token(TOK_STATIC);
      } else if (strcmp(identifier, "super") == 0) {
         return operator_token(TOK_SUPER);
      }
      break;
   case 't':
      if (strcmp(identifier, "true") == 0) {
         return operator_token(TOK_TRUE);
      }
      break;
   case 'v':
      if (strcmp(identifier, "var") == 0) {
         return operator_token(TOK_VAR);
      }
      break;
   case 'w':
      if (strcmp(identifier, "when") == 0) {
         return operator_token(TOK_WHEN);
      } else if (strcmp(identifier, "while") == 0) {
         return operator_token(TOK_WHILE);
      }
      break;
   case 'y':
      if (strcmp(identifier, "yield") == 0) {
         return operator_token(TOK_YIELD);
      }
      break;
   }

   char *str = malloc(identifier_index + 1);
   memcpy(str, identifier, identifier_index + 1);

   Token result = {
      .type = TOK_IDENTIFIER,
      .str_data = str,
   };
   return result;
}

static Token token(Preprocessor *proc) {
   skip_whitespace(proc);

   Token result = {0};

   char c = next(proc);
   switch (c) {
   case '\0':
      result.type = TOK_ERROR;
      result.err_data = ERR_EOF;
      return result;

   case '+':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_PLUS_EQUAL);
      }
      return operator_token(TOK_PLUS);

   case '-':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_MINUS_EQUAL);
      } else if (peek(proc) == '>') {
         next(proc);
         return operator_token(TOK_ARROW);
      }
      return operator_token(TOK_MINUS);

   case '*':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_STAR_EQUAL);
      }
      return operator_token(TOK_STAR);

   case '/':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_SLASH_EQUAL);
      }
      return operator_token(TOK_SLASH);

   case '%':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_PERCENT_EQUAL);
      }
      return operator_token(TOK_PERCENT);

   case '!':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_BANG_EQUAL);
      }
      return operator_token(TOK_BANG);

   case '&':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_AMPERSAND_EQUAL);
      }
      return operator_token(TOK_AMPERSAND);

   case '|':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_PIPE_EQUAL);
      }
      return operator_token(TOK_PIPE);

   case '^':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_UP_CARET_EQUAL);
      }
      return operator_token(TOK_UP_CARET);

   case '<':
      if (peek(proc) == '<') {
         next(proc);
         return operator_token(TOK_DOUBLE_LCHEVRON);
      } else if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_LCHEVRON_EQUAL);
      }

      return operator_token(TOK_LCHEVRON);

   case '>':
      if (peek(proc) == '>') {
         next(proc);
         return operator_token(TOK_DOUBLE_RCHEVRON);
      } else if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_RCHEVRON_EQUAL);
      }

      return operator_token(TOK_LCHEVRON);

   case '[':
      return operator_token(TOK_LBRACKET);

   case ']':
      return operator_token(TOK_RBRACKET);

   case '(':
      return operator_token(TOK_LPAREN);

   case ')':
      return operator_token(TOK_RPAREN);

   case '{':
      return operator_token(TOK_LBRACE);

   case '}':
      return operator_token(TOK_RBRACE);

   case '=':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(TOK_EQUAL_EQUAL);
      }
      return operator_token(TOK_EQUAL);

   case '~':
      return operator_token(TOK_TILDE);

   case ',':
      return operator_token(TOK_COMMA);

   case ':':
      return operator_token(TOK_COLON);

   case ';':
      return operator_token(TOK_SEMICOLON);

   case '.': {
      char peek_char = peek(proc);

      if (is_digit(peek_char)) {
         return number(proc, '.');
      }

      if (peek_char != '.') {
         return operator_token(TOK_DOT);
      }
      next(proc);

      if (peek(proc) != '.') {
         return unexpected_char_token();
      }
      next(proc);
   }
      return operator_token(TOK_ELIPSES);

   case '\'':
      return quoted_literal(proc, '\'', TOK_CHAR, true);

   case '"':
      return quoted_literal(proc, '"', TOK_STR, true);

   default:
      if (is_digit(c) || c == '.') {
         return number(proc, c);
      }

      if (is_alpha(c) || c == '_') {
         return identifier(proc, c);
      }
      break;
   }

   result.char_data = c;
   result.type = TOK_MISC;
   return result;
}

const char *tok_to_str(Token tok) {
   switch (tok.type) {
   case TOK_ERROR:
      if (tok.err_data == ERR_UNEXPECTED) {
         return "Unexpected character";
      } else {
         return "End of file";
      }

   case TOK_IDENTIFIER:
   case TOK_CHAR:
   case TOK_NUMBER:
   case TOK_STR:
      return tok.str_data;

   case TOK_PLUS:
      return "+";
   case TOK_MINUS:
      return "-";
   case TOK_STAR:
      return "*";
   case TOK_SLASH:
      return "/";
   case TOK_PERCENT:
      return "%";
   case TOK_BANG:
      return "!";
   case TOK_AMPERSAND:
      return "&";
   case TOK_PIPE:
      return "|";
   case TOK_UP_CARET:
      return "^";
   case TOK_DOUBLE_LCHEVRON:
      return "<<";
   case TOK_DOUBLE_RCHEVRON:
      return ">>";
   case TOK_PLUS_EQUAL:
      return "+=";
   case TOK_MINUS_EQUAL:
      return "-=";
   case TOK_STAR_EQUAL:
      return "*=";
   case TOK_SLASH_EQUAL:
      return "/=";
   case TOK_PERCENT_EQUAL:
      return "%=";
   case TOK_BANG_EQUAL:
      return "!=";
   case TOK_AMPERSAND_EQUAL:
      return "&=";
   case TOK_PIPE_EQUAL:
      return "|=";
   case TOK_UP_CARET_EQUAL:
      return "^=";
   case TOK_LBRACKET:
      return "[";
   case TOK_RBRACKET:
      return "]";
   case TOK_LPAREN:
      return "(";
   case TOK_RPAREN:
      return ")";
   case TOK_LBRACE:
      return "{";
   case TOK_RBRACE:
      return "}";
   case TOK_DOUBLE_AMPERSAND:
      return "&&";
   case TOK_DOUBLE_PIPE:
      return "||";
   case TOK_LCHEVRON:
      return "<";
   case TOK_RCHEVRON:
      return ">";
   case TOK_LCHEVRON_EQUAL:
      return "<=";
   case TOK_RCHEVRON_EQUAL:
      return ">=";
   case TOK_EQUAL_EQUAL:
      return "==";
   case TOK_EQUAL:
      return "=";
   case TOK_TILDE:
      return "~";
   case TOK_COMMA:
      return ",";
   case TOK_COLON:
      return ":";
   case TOK_SEMICOLON:
      return ";";
   case TOK_ARROW:
      return "->";
   case TOK_DOT:
      return ".";
   case TOK_ELIPSES:
      return "...";

   case TOK_AS:
      return "as";
   case TOK_ASSERT:
      return "assert";
   case TOK_BREAK:
      return "break";
   case TOK_BREAKPOINT:
      return "breakpoint";
   case TOK_CLASS:
      return "class";
   case TOK_CLASS_NAME:
      return "class_name";
   case TOK_CONST:
      return "const";
   case TOK_CONTINUE:
      return "continue";
   case TOK_ELIF:
      return "elif";
   case TOK_ELSE:
      return "else";
   case TOK_ENUM:
      return "enum";
   case TOK_EXTENDS:
      return "extends";
   case TOK_FALSE:
      return "false";
   case TOK_FOR:
      return "for";
   case TOK_FUNC:
      return "func";
   case TOK_IF:
      return "if";
   case TOK_IN:
      return "in";
   case TOK_IS:
      return "is";
   case TOK_MATCH:
      return "match";
   case TOK_NULL:
      return "null";
   case TOK_PASS:
      return "pass";
   case TOK_RETURN:
      return "return";
   case TOK_SELF:
      return "self";
   case TOK_SIGNAL:
      return "signal";
   case TOK_STATIC:
      return "static";
   case TOK_SUPER:
      return "super";
   case TOK_TRUE:
      return "true";
   case TOK_VAR:
      return "var";
   case TOK_WHEN:
      return "when";
   case TOK_WHILE:
      return "while";
   case TOK_YIELD:
      return "yield";

   default:
      return "???";
   }
}

void tokenize(const char *src, size_t size) {
   Preprocessor proc = {
      .src = src,
      .size = size,
      .read_index = 0,
   };

   Token tok = token(&proc);
   while (true) {
      printf("%s\n", tok_to_str(tok));

      token_free(&tok);
      if (tok.type == TOK_ERROR) {
         break;
      }

      tok = token(&proc);
   }
}
