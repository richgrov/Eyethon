#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "tokenize.h"

int main(int argc, char **argv) {
   if (argc < 2) {
      puts("error: no files specified");
      return -1;
   }

   FILE *file = fopen(argv[1], "rb");
   if (file == NULL) {
      perrorf("failed to open %s", argv[1]);
      return errno;
   }

   if (fseek(file, 0L, SEEK_END) != 0) {
      perror("fseek end failed");
      goto close_file;
   }

   long size = ftell(file);
   if (size == -1) {
      perror("ftell failed");
      goto close_file;
   }

   char *buf = malloc(size);
   if (buf == NULL) {
      perrorf("failed to allocate %ld bytes for file", size);
      goto close_file;
   }

   if (fseek(file, 0L, SEEK_SET) != 0) {
      perror("fseek set failed");
      goto free_buffer;
   }

   size_t read = fread(buf, 1, size, file);
   if (read != size) {
      perrorf("failed to read file");
      goto free_buffer;
   }

   tokenize(buf, size);

free_buffer:
   free(buf);

close_file:
   fclose(file);
   return errno;
}
