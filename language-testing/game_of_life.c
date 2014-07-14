#include <stdlib.h>
#include <time.h>
#include <curses.h>

/* Swap pointer data with each other */
void swap(int **ptr1, int **ptr2) {
  int *tmp = *ptr1;
  *ptr1 = *ptr2;
  *ptr2 = tmp;
}

/* Draw a map to screen (1s are white, 0s are black) */
void draw_screen(int *map, int max_x, int max_y) {
  const chtype WHITE_SYM = ' ' | A_REVERSE;
  const chtype BLACK_SYM = ' ';
  int x, y;

  for (y = 0; y < max_y; ++y) {
    for (x = 0; x < max_x; ++x) {
      mvaddch(y, x, map[y * max_x + x] ? WHITE_SYM : BLACK_SYM);
    }
  }
  refresh();
}

/* Get number of white neighbours to a point at a map */
int get_num_neighbours(int *map, int x, int y, int maxx, int maxy) {
  int neighbours = 0;
  int x2, y2;

  for (y2 = y -1; y2 <= y +1; ++y2) {
    for (x2 = x -1; x2 <= x +1; ++x2) {
      if (0 <= y2 && y2 <= maxy &&
          0 <= x2 && x2 <= maxx &&
          !(x2 == x && y2 == y) &&
          map[y2 * maxx + x2] == 1) {
        ++neighbours;
      }
    }
  }
  return neighbours;
}

/* Generate the next iteration in the map */
void evolve(int *mapfrom, int *mapto, int maxx, int maxy) {
  int x, y;

  for (y = 0; y < maxy; ++y) {
    for (x = 0; x < maxx; ++x) {
      int neighbours = get_num_neighbours(mapfrom, x, y, maxx, maxy);
      if (mapfrom[y * maxx + x] == 0) {
        if (neighbours == 3) {
          mapto[y * maxx + x] = 1;
          continue;
        }

      } else if (neighbours == 2 || neighbours == 3) {
        mapto[y * maxx + x] = 1;
        continue;
      }

      mapto[y * maxx + x] = 0;
    }
  }
}

void get_screen_size_or_exit(int argc, char *argv[], int *maxx, int *maxy) {
  if (argc != 3) {
    endwin();
    fprintf(stderr, "Usage: %s <width> <height>\n", argv[0]);
    exit(1);
  }

  *maxx = atoi(argv[1]);
  *maxy = atoi(argv[2]);

  if (0 >= *maxx || *maxx >= getmaxx(stdscr)) {
    endwin();
    fprintf(stderr, "Error: width must be between 1 and %d\n",
            getmaxx(stdscr) -1);
    exit(1);
  } else if (0 >= *maxy || *maxy >= getmaxy(stdscr)) {
    endwin();
    fprintf(stderr, "Error: height must be between 1 and %d\n",
            getmaxy(stdscr) -1);
    exit(1);
  }
}

void init_game_maps_or_exit(int **stdmap, int **auxmap, int maxx, int maxy) {
  int i;

  if ((*stdmap = malloc(sizeof *stdmap * maxx * maxy)) == NULL ||
      (*auxmap = malloc(sizeof *stdmap * maxx * maxy)) == NULL)
  {
    endwin();
    fprintf(stderr, "Virtual memory exceeded!\n");
    exit(1);
  }

  srand(time(NULL));
  for (i = maxx * maxy -1; i >= 0; --i) {
    (*stdmap)[i] = rand() % 5 == 1;
  }
}

int main(int argc, char* argv[]) {
  int maxy, maxx;
  int *stdmap = NULL;
  int *auxmap = NULL;

  /* Init curses */
  initscr();
  curs_set(0);

  /* Parse screen size */
  get_screen_size_or_exit(argc, argv, &maxx, &maxy);

  /* Init the rest */
  init_game_maps_or_exit(&stdmap, &auxmap, maxx, maxy);

  /* Main */
  timeout(100);
  while (getch() != 'q') {
    draw_screen(stdmap, maxx, maxy);
    evolve(stdmap, auxmap, maxx, maxy);
    swap(&stdmap, &auxmap);
  }

  /* Clean up and exit */
  endwin();
  free(stdmap);
  free(auxmap);
  return 0;
}

/* TAIL INFO:
Name: Game of Life
Language: C
State: Done
Compile: cc game_of_life.c -std=c99 -O3 -lcurses -o game_of_life

Display randomized game of life

Example: ./game_of_life 150 50

*/
