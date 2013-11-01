
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <curses.h>

/* malloc a map and fill it with random numbers */
int init_random_map(int** map, int maxyx) {
  
  if (*map != NULL) {
    fprintf(stderr, "map already malloc'd\n");
    free(*map);
    return 1;
  }

  *map = (int *)malloc(sizeof(int) * maxyx);
  if (*map == NULL) {
    fprintf(stderr, "Failed to malloc map\n");
    return 1;
  }
  
  /* Fill map with 1's and 0's: (>1 will later be treated as a 0) */
  for (int i = 0, *mapp = map[0]; i < (maxyx); i++, mapp++)
    *mapp = (rand() % 5);
  
  return 0;
}

/* Draw a map to screen (1s are white, 0s are black) */
int draw_screen(int **map, int maxx, int maxy) {
  
  int err = 0;
  int maxyx = maxy * maxx;

  for (int i = 0, *mapp = map[0]; i < maxyx; i++, mapp++) {
    if (*mapp == 1) {
      attron(A_REVERSE);
      err += mvaddch(i / maxx, i % maxx, ' ');
      attroff(A_REVERSE);
    } 
    else {
      err += mvaddch(i / maxx, i % maxx, ' ');
    }
  }
  refresh();
  return err;
}

int wrap_around(int num, int maxyx) {
  if (num < 0) return maxyx -1 -num;
  if (num > maxyx -1) return num -maxyx -1;
  return num;
}

void evolve(int **mapfrom, int **mapto, int maxx, int maxy) {
  
  int maxyx = maxx * maxy;

  for (int i = 0, *mappf = mapfrom[0], *mappt = mapto[0]; 
       i < maxyx; i++, mappf++, mappt++) {
    int nabo = 0;

    if (*(mapfrom[0] + wrap_around(i -maxx -1, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i -maxx, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i -maxx +1, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i -1, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i +1, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i +maxx -1, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i +maxx, maxyx)) == 1) nabo++;
    if (*(mapfrom[0] + wrap_around(i +maxx +1, maxyx)) == 1) nabo++;

    if (*mappf == 0 && nabo == 3) *mappt = 1;
    else if (*mappf == 1 && (nabo == 2 || nabo == 3)) *mappt = 1;
    else *mappt = 0;
  }
}

int main(int argc, char* argv[]) {

  /* Check that argv is fine: */
  if (argc != 3) {
    printf("Usage: %s <width> <height>\n", argv[0]);
    return 1;
  }
  initscr();
  curs_set(0);
  
  /* If user gives strange width/height, we'll just set it to max */
  int maxy, maxx;
  getmaxyx(stdscr, maxy, maxx);
  int tmp_maxx = atoi(argv[1]);
  int tmp_maxy = atoi(argv[2]);
 
  if (0 < tmp_maxx && tmp_maxx < --maxx)
    maxx = tmp_maxx;
  if (0 < tmp_maxy && tmp_maxy < --maxy)
    maxy = tmp_maxy;
 
  /* Start random-number-generator: */
  srand(time(NULL));

  /* Init random map: */
  int *stdmap = NULL, *auxmap = NULL;
  if (init_random_map(&stdmap, maxx*maxy) != 0)
    return 1;
  if (init_random_map(&auxmap, maxx*maxy) != 0)
    return 1;
  
  int err = draw_screen(&stdmap, maxx, maxy);

  timeout(100);
  int i = 0, *mapp1 = NULL, *mapp2 = NULL;
  for (int c = getch(); c != 'q'; c = getch()) {
    if (i++ % 2) {
      mapp1 = stdmap;
      mapp2 = auxmap;
    }
    else {
      mapp1 = auxmap;
      mapp2 = stdmap;
    }
    evolve(&mapp2, &mapp1, maxx, maxy);
    err += draw_screen(&mapp1, maxx, maxy);
  }

  endwin();
  free(stdmap);
  free(auxmap);
  printf("Maxx: %d, maxy: %d\n", maxx, maxy);
  printf("Errors: %d\n", err);
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
