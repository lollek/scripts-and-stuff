#! /usr/bin/env ruby

class Cube
  attr_accessor :data, :current
  attr_reader :data_size

  def initialize
    # The cube's edges are named like this
    # in @neighbours_data
    # 0-----1
    # |\    |\
    # | 4-----5
    # 3-|---2 |
    #  \|    \|
    #   7-----6
    @current = nil
    @data_size = 8
    @data = [nil] * @data_size
    @neighbours_data =
      [[1,3,4], [0,2,5], [1,3,6], [0,2,7],
       [0,5,7], [1,4,6], [2,5,7], [3,4,6]]
  end

  def available_paths
    @neighbours_data[@current].select {|z| @data[z].nil? }
  end

  def next(pos)
    this = self.dup
    this.data = self.data.dup
    this.data[pos] = @data_size - @data.compact.size
    this.current = pos
    this
  end
end

$paths = 0
def try_next(cube)
  if cube.data.select(&:nil?).empty?
    $paths += 1
  elsif not cube.available_paths.empty?
    cube.available_paths.each do |n|
      try_next(cube.next(n))
    end
  end
end

cube = Cube.new
(0...cube.data_size).each do |i|
  try_next(cube.next(i))
end

puts $paths/2
