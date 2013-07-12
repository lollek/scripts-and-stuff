#include <cstdlib>
#include <cstdio>

/* Card images
 * P = Purple
 * R = Red
 * Y = Yellow
 * G = Green */

#define P_HEAD 1
#define P_TAIL -1

#define R_HEAD 2
#define R_TAIL -2

#define Y_HEAD 3
#define Y_TAIL -3

#define G_HEAD 4
#define G_TAIL -4

class Card 
{
private:
  signed int up;
  signed int right;
  signed int down;
  signed int left;

public:
  unsigned int num;
  bool is_in_use;

  Card(unsigned int num, signed int up, signed int right, signed int down, signed int left);
  ~Card(){}

  void rotate();
  bool fits_with(Card *&other_card, int direction);
};

Card::Card(unsigned int num, signed int up, signed int right, signed int down, signed int left)
{
  this->num = num;
  this->up = up;
  this->right = right;
  this->down = down;
  this->left = left;
  this->is_in_use = false;
}

void Card::rotate()
{
  signed int tmp = this->up;
  this->up = this->left;
  this->left = this->down;
  this->down = this->right;
  this->right = tmp;
}

bool Card::fits_with(Card *&other_card, int direction)
{
  signed int _this, _other;

  /* This checks above card to this */
  if (direction == 1)
  {
    _this = this->up;
    _other = other_card->down;
  }
  /* This checks the card to the left to this */
  else // direction == 4
  {
    _this = this->left;
    _other = other_card->right;
  }
  
  /* If the color is same but one is head and one tail,
   * adding them together will return 0 */
  if (_this + _other)
    return false;
  return true;
}

static Card deck[] = {
  Card(1, G_HEAD, R_TAIL, P_TAIL, P_HEAD),
  Card(2, G_TAIL, Y_TAIL, G_HEAD, R_HEAD),
  Card(3, R_TAIL, Y_TAIL, G_HEAD, Y_HEAD),
  Card(4, P_HEAD, R_TAIL, P_TAIL, G_HEAD),
  Card(5, G_TAIL, Y_TAIL, P_HEAD, R_HEAD),
  Card(6, G_TAIL, R_TAIL, P_HEAD, Y_HEAD),
  Card(7, P_HEAD, Y_TAIL, R_TAIL, G_HEAD),
  Card(8, P_TAIL, R_TAIL, P_HEAD, Y_HEAD),
  Card(9, P_TAIL, Y_TAIL, G_HEAD, R_HEAD)
};

static Card *solution[9];


void manage_level(int current_level)
{ /* Check all cards for each position */
  for (int i = 0; i < 9; i++) 
  { /* Check all rotations of the card */
    for (int r = 0; r < 4; r++) 
    {
      deck[i].rotate();
      bool go_next = false;

      /* If a card already in the slot, remove it */
      if (solution[current_level -1] != NULL)
      {
        solution[current_level -1]->is_in_use = false;
        solution[current_level -1] = NULL;
      }

      /* If the card already is in use, go to next */
      if (deck[i].is_in_use)
        continue;
      
      /* If it's the first card, there's no need for checking */
      if (current_level == 1)
        go_next = true;

      /* Card 2 and 3 only need to check to the left */
      else if (current_level == 2 || current_level == 3)
      {
        if (!deck[i].fits_with(solution[current_level -2], 4))
          continue;
        go_next = true;
      }

      /* Card 4 and 7 only need to check up */
      else if (current_level == 4 || current_level == 7)
      {
        if (!deck[i].fits_with(solution[current_level -4], 1))
          continue;
        go_next = true;
      }

      /* The rest will need to check both left and up */
      else if (current_level == 5 || current_level == 6 ||
               current_level == 8 || current_level == 9)
      {
        if (!deck[i].fits_with(solution[current_level -2], 4))
          continue;
        else if (!deck[i].fits_with(solution[current_level -4], 1))
          continue;
        go_next = true;
      }

      /* True if the card fits */
      if (go_next)
      {
        /* Place the card in the slot */
        solution[current_level -1] = &deck[i];
        solution[current_level -1]->is_in_use = true;

        /* If all cards are set, print the solution */
        if (current_level == 9)
        {
          printf("Solved:\n"
                  "%d %d %d\n"
                  "%d %d %d\n"
                  "%d %d %d\n",
                  solution[0]->num, solution[1]->num, solution[2]->num,
                  solution[3]->num, solution[4]->num, solution[5]->num,
                  solution[6]->num, solution[7]->num, solution[8]->num);
          continue;
        }
        /* Otherwise, go to next slot */
        else
          manage_level(current_level +1);
      }
    }
  }
  /* Before we go back to a previous slot, remove the card */
  if (solution[current_level -1] != NULL)
  {
    solution[current_level -1]->is_in_use = false;
    solution[current_level -1] = NULL;
  }
}

int main()
{
  for (int i = 0; i < 9; i++)
    solution[i] = NULL;
  manage_level(1);
  return 0;
}


/* TAIL INFO:
Name: Witch Game Solver
Language: c++
Compile: g++ witchsolver.cpp -o witchsolver
State: Done

This application solves the Witch game (crazykamo)
Result is written as numbers, which are the cards in the game
Compare result to crazykamo/kamo.png for true result
Example: ./witchsolver
*/
