#! /usr/bin/env ruby

P_HEAD, P_TAIL = 1, -1
R_HEAD, R_TAIL = 2, -2
Y_HEAD, Y_TAIL = 3, -3
G_HEAD, G_TAIL = 4, -4

class Card

  def initialize (num, up, right, down, left)
    @num = num
    @up = up
    @right = right
    @down = down
    @left = left
  end

  def rotated (times)
    if times == 0 then
      return Card.new(@num, @up, @right, @down, @left)
    elsif times == 1 then
      return Card.new(@num, @left, @up, @right, @down)
    elsif times == 2 then
      return Card.new(@num, @down, @left, @up, @right)
    else 
      return Card.new(@num, @right, @down, @left, @up)
    end
  end

  def fits_with (other_card, direction)

    if direction == 1 then
      _this = @up
      _other = other_card.instance_variable_get(:@down)
    else #direction == 4
      _this = @left
      _other = other_card.instance_variable_get(:@right)
    end

    if _this + _other == 0 then
      return true
    else 
      return false
    end
  end
end

def manage_level (level, deck, solution=[])
  if level == 0 then
    9.times {|z| 4.times {|x| manage_level 1, deck - [deck[z]], [deck[z].rotated(x)] } }

  elsif level < 9 then
    (9-level).times do |z| 
      4.times do |x|
        new_card = deck[z].rotated(x)
        if level <3 and new_card.fits_with solution[level-1], 1 then
          manage_level level+1, deck - [deck[z]], solution + [new_card]
        elsif (level == 3 or level == 6) and new_card.fits_with solution[level-3], 4 then
          manage_level level+1, deck - [deck[z]], solution + [new_card]
        elsif new_card.fits_with solution[level-1], 1 and 
            new_card.fits_with solution[level-3], 4 then
          manage_level level+1, deck - [deck[z]], solution + [new_card]
        end
      end
    end
  else
    puts "Solved!\n"
    (0..2).each {|z| print "#{solution[z].instance_variable_get(:@num)} "}; puts
    (3..5).each {|z| print "#{solution[z].instance_variable_get(:@num)} "}; puts
    (6..8).each {|z| print "#{solution[z].instance_variable_get(:@num)} "}; puts
  end
end

def main
  
  deck = []
  deck << Card.new(1, G_HEAD, R_TAIL, P_TAIL, P_HEAD)
  deck << Card.new(2, G_TAIL, Y_TAIL, G_HEAD, R_HEAD)
  deck << Card.new(3, R_TAIL, Y_TAIL, G_HEAD, Y_HEAD)
  deck << Card.new(4, P_HEAD, R_TAIL, P_TAIL, G_HEAD)
  deck << Card.new(5, G_TAIL, Y_TAIL, P_HEAD, R_HEAD)
  deck << Card.new(6, G_TAIL, R_TAIL, P_HEAD, Y_HEAD)
  deck << Card.new(7, P_HEAD, Y_TAIL, R_TAIL, G_HEAD)
  deck << Card.new(8, P_TAIL, R_TAIL, P_HEAD, Y_HEAD)
  deck << Card.new(9, P_TAIL, Y_TAIL, G_HEAD, R_HEAD)

  manage_level 0, deck
end
main

# TAIL INFO:
# Name: Witch Game Solver
# Language: Ruby
# State: Done
# Created: 2013-08-08
#
#
# This application solves the Witch game (crazykamo)
# Result is written in number, representing the cards in the game
# Compare result to crazykamo/kamo.png for true result
#
