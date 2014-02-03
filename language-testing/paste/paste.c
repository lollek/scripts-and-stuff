#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void usage(char **argv) {
  fprintf(stderr, "Usage: %s file1 file2 [-d DELIM]\n", argv[0]);
  exit(1);
}

/* Remove first newline (and tail) found in string */
void chomp(char *line) {
  while (*line != '\n' && *line != '\0') {
    line++;
  }
  if (*line == '\n') {
    *line = '\0';
  }
}

void paste(const char *filename1, const char *filename2, const char *delim) {
  FILE *file1 = fopen(filename1, "r");
  FILE *file2 = fopen(filename2, "r");

  if (file1 == NULL || file2 == NULL) {
    fprintf(stderr, "Unable to open %s!\n",
      file1 == NULL ? filename1 : filename2);
    return;
  }

  /* Read them line by line and print them like this:
   * line1 delim line2 */
  char *line1 = NULL;
  char *line2 = NULL;

  for (;;) {
    unsigned int line_size;
    ssize_t linestatus1 = getline(&line1, &line_size, file1);
    ssize_t linestatus2 = getline(&line2, &line_size, file2);
    if (linestatus1 == -1 && linestatus2 == -1) {
      break;
    }

    if (linestatus1 != -1) {
      chomp(line1);
    }
    if (linestatus2 != -1) {
      chomp(line2);
    }
    printf("%s%s%s\n", linestatus1 == -1 ? "" : line1,
                delim, linestatus2 == -1 ? "" : line2);
  }

  /* Close files and clean up */
  free(line1);
  free(line2);
  fclose(file1);
  fclose(file2);
}

int main(int argc, char *argv[]) {
  const char *file1 = NULL;
  const char *file2 = NULL;
  const char *delim = NULL;
  int c;

  /* Hande optional options */
  while ((c = getopt(argc, argv, "d:")) != -1) {
    switch (c) {
      case 'd': delim = optarg; break;
      default: usage(argv); break;
    }
  }

  /* Handle required options */
  for (int index = optind; index < argc; index++) {
    if (file1 == NULL) {
      file1 = argv[index];
    } else if (file2 == NULL) {
      file2 = argv[index];
    } else {
      usage(argv);
    }
  }

  /* Check that we have everything */
  if (file1 == NULL || file2 == NULL) {
    usage(argv);
  }

  paste(file1, file2, delim == NULL ? " " : delim);
  return 0;
}
