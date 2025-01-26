#include "tokenize.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chars.h"
#include "config.h"
#include "operator.h"

static void token_free(Token *tok) {
   switch (tok->type) {
   case PROC_IDENTIFIER:
   case PROC_CHAR:
   case PROC_NUMBER:
   case PROC_STR:
      free(tok->str_data);
   default:
      break;
   }
}

static Token operator_token(Operator op) {
   Token result = {.type = PROC_OPERATOR, .op_data = op};
   return result;
}

static Token unexpected_char_token() {
   Token result = {.type = PROC_ERROR, .err_data = ERR_UNEXPECTED};
   return result;
}

typedef struct {
   const char *src;
   size_t size;
   size_t read_index;
} Preprocessor;

static char try_decode_trigraph(Preprocessor *proc) {
   if (proc->read_index + 2 >= proc->size) {
      return '\0';
   }

   if (proc->src[proc->read_index] != '?' || proc->src[proc->read_index + 1] != '?') {
      return '\0';
   }

   switch (proc->src[proc->read_index + 2]) {
   case '=':
      return '#';
   case '(':
      return '[';
   case '/':
      return '\\';
   case ')':
      return ']';
   case '\'':
      return '^';
   case '<':
      return '{';
   case '!':
      return '|';
   case '>':
      return '}';
   case '-':
      return '~';
   default:
      return '\0';
   }
}

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

   char trigraph = try_decode_trigraph(proc);
   if (trigraph != '\0') {
      return trigraph;
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

   char trigraph = try_decode_trigraph(proc);
   if (trigraph != '\0') {
      proc->read_index += 3;
      return trigraph;
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
      .type = PROC_NUMBER,
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

   char *str = malloc(identifier_index + 1);
   memcpy(str, identifier, identifier_index + 1);

   Token result = {
      .type = PROC_IDENTIFIER,
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
      result.type = PROC_ERROR;
      result.err_data = ERR_EOF;
      return result;

   case '+':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_PLUS_EQUAL);
      }
      return operator_token(OP_PLUS);

   case '-':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_MINUS_EQUAL);
      } else if (peek(proc) == '>') {
         next(proc);
         return operator_token(OP_ARROW);
      }
      return operator_token(OP_MINUS);

   case '*':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_STAR_EQUAL);
      }
      return operator_token(OP_STAR);

   case '/':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_SLASH_EQUAL);
      }
      return operator_token(OP_SLASH);

   case '%':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_PERCENT_EQUAL);
      }
      return operator_token(OP_PERCENT);

   case '!':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_BANG_EQUAL);
      }
      return operator_token(OP_BANG);

   case '&':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_AMPERSAND_EQUAL);
      }
      return operator_token(OP_AMPERSAND);

   case '|':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_PIPE_EQUAL);
      }
      return operator_token(OP_PIPE);

   case '^':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_UP_CARET_EQUAL);
      }
      return operator_token(OP_UP_CARET);

   case '<':
      if (peek(proc) == '<') {
         next(proc);
         return operator_token(OP_DOUBLE_LCHEVRON);
      } else if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_LCHEVRON_EQUAL);
      }

      return operator_token(OP_LCHEVRON);

   case '>':
      if (peek(proc) == '>') {
         next(proc);
         return operator_token(OP_DOUBLE_RCHEVRON);
      } else if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_RCHEVRON_EQUAL);
      }

      return operator_token(OP_LCHEVRON);

   case '[':
      return operator_token(OP_LBRACKET);

   case ']':
      return operator_token(OP_RBRACKET);

   case '(':
      return operator_token(OP_LPAREN);

   case ')':
      return operator_token(OP_RPAREN);

   case '{':
      return operator_token(OP_LBRACE);

   case '}':
      return operator_token(OP_RBRACE);

   case '=':
      if (peek(proc) == '=') {
         next(proc);
         return operator_token(OP_EQUAL_EQUAL);
      }
      return operator_token(OP_EQUAL);

   case '~':
      return operator_token(OP_TILDE);

   case ',':
      return operator_token(OP_COMMA);

   case ':':
      return operator_token(OP_COLON);

   case ';':
      return operator_token(OP_SEMICOLON);

   case '.': {
      char peek_char = peek(proc);

      if (is_digit(peek_char)) {
         return number(proc, '.');
      }

      if (peek_char != '.') {
         return operator_token(OP_DOT);
      }
      next(proc);

      if (peek(proc) != '.') {
         return unexpected_char_token();
      }
      next(proc);
   }
      return operator_token(OP_ELIPSES);

   case '\'':
      return quoted_literal(proc, '\'', PROC_CHAR, true);

   case '"':
      return quoted_literal(proc, '"', PROC_STR, true);

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
   result.type = PROC_MISC;
   return result;
}

void tokenize(const char *src, size_t size) {
   Preprocessor proc = {
      .src = src,
      .size = size,
      .read_index = 0,
   };

   Token tok = token(&proc);
   while (true) {
      switch (tok.type) {
      case PROC_IDENTIFIER:
      case PROC_CHAR:
      case PROC_NUMBER:
      case PROC_STR:

      case PROC_OPERATOR:
         printf("%s\n", operator_to_str(tok.op_data));
         break;

      case PROC_MISC:
         if (tok.char_data != '\r') {
            printf("%c\n", tok.char_data);
         }
         break;

      case PROC_ERROR:
         if (tok.err_data == ERR_UNEXPECTED) {
            printf("Unexpected character\n");
         }
         break;
      }

      token_free(&tok);
      if (tok.type == PROC_ERROR) {
         break;
      }

      tok = token(&proc);
   }
}
