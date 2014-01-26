#! /usr/bin/env python3

P_HEAD, P_TAIL = 1, -1
R_HEAD, R_TAIL = 2, -2
Y_HEAD, Y_TAIL = 3, -3
G_HEAD, G_TAIL = 4, -4

class Card:

    def __init__(self, num, up, right, down, left):

        self.num = num
        self.up = up
        self.right = right
        self.down = down
        self.left = left

        self.is_in_use = False

    def rotate(self):

        self.up, self.right, self.down, self.left = self.left, self.up, self.right, self.down

    def fits_with(self, other_card, direction):

        if direction == 1:
            _this = self.up
            _other = other_card.down
        else: #direction == 4
            _this = self.left
            _other = other_card.right

        if _this + _other:
            return False
        return True

def manage_level():
    global current_card
    for card in deck:
        for rot in range(4):
            card.rotate()

            if solution[current_card-1] is not None:
                solution[current_card-1].is_in_use = False
                solution[current_card-1] = None

            #print(" ".join(["lvl: %d:%d |" % (current_card, card.num)] + [str(x) if x is None else str(x.num) for x in solution]))
            go_next = False

            if card.is_in_use: 
                continue

            elif current_card == 1:
                go_next = True

            elif current_card in (2, 3):
                if not card.fits_with(solution[current_card-2], 4):
                    continue
                go_next = True
            
            elif current_card in (4, 7):
                if not card.fits_with(solution[current_card-4], 1):
                    continue
                go_next = True

            elif current_card in (5, 6, 8, 9):
                if not card.fits_with(solution[current_card-2], 4):
                    continue
                elif not card.fits_with(solution[current_card-4], 1):
                    continue
                go_next = True
            

            if go_next:
                solution[current_card-1] = card
                card.is_in_use = True

                if current_card == 9:
                    print("Solved:",
                          "%d %d %d" % (solution[0].num, solution[1].num, solution[2].num),
                          "%d %d %d" % (solution[3].num, solution[4].num, solution[5].num),
                          "%d %d %d" % (solution[6].num, solution[7].num, solution[8].num),
                          sep="\n")
                    continue
                else:
                    current_card += 1
                    manage_level()

    if solution[current_card-1] is not None:
        solution[current_card-1].is_in_use = False
        solution[current_card-1] = None
    current_card -= 1

def spawn_deck():        

    deck = []
    deck.append(Card(1, G_HEAD, R_TAIL, P_TAIL, P_HEAD))
    deck.append(Card(2, G_TAIL, Y_TAIL, G_HEAD, R_HEAD))
    deck.append(Card(3, R_TAIL, Y_TAIL, G_HEAD, Y_HEAD))
    deck.append(Card(4, P_HEAD, R_TAIL, P_TAIL, G_HEAD))
    deck.append(Card(5, G_TAIL, Y_TAIL, P_HEAD, R_HEAD))
    deck.append(Card(6, G_TAIL, R_TAIL, P_HEAD, Y_HEAD))
    deck.append(Card(7, P_HEAD, Y_TAIL, R_TAIL, G_HEAD))
    deck.append(Card(8, P_TAIL, R_TAIL, P_HEAD, Y_HEAD))
    deck.append(Card(9, P_TAIL, Y_TAIL, G_HEAD, R_HEAD))
    return deck

if __name__ == "__main__":

    deck = spawn_deck()
    solution = 9 * [None]
    current_card = 1
    manage_level()


    
""" TAIL INFO:
Name: Witch Game Solver
Language: Python 3
State: Done

This application solves the Witch game (crazykamo)
Result is written as numbers, which are the cards in the game
Compare result to crazykamo/kamo.png for true result

example: ./witchsolver.py
"""
