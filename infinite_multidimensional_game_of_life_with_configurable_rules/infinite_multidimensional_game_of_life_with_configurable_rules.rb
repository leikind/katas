# Yuri Leikind && Dmitry Adamushko :)

require 'set'



module GameOfLife


  module Permutator # To understand recursion you have to understand recursion
    class << self
      def permutations(list, i = 0, *a)
        return [a] if i == list.size
        list[i].map do |x|
          permutations(list, i+1, *(a + [x]))
        end.inject([]) {|m, x| m + x}
      end
    end
  end


  class Coordinates

    attr_reader :dimension

    def initialize(*args)
      @coords = args
      @dimension = args.size
    end

    def self.[](*args)
      self.new(*args)
    end

    def [](i)
      @coords[i]
    end

    def hash
      @coords.hash
    end

    def around
      Permutator.permutations(@coords.map{|a|
        [a-1, a , a+1]
      }).reject{|coord|
        coord == @coords
      }.map{|coord|
        Coordinates[*coord]
      }
    end

    def inspect
      @coords.inspect
    end

    def eql?(another_coord)
      @coords.eql?(another_coord.coords)
    end

    alias_method :==, :eql?

    protected

    # don't want to add a public getter, this is only for #eql?
    def coords
      @coords
    end

  end


  class Rule

    def initialize(*ranges)
      @ranges = ranges
    end

    def is_triggered_on?(number_of_neighbours)
      @ranges.detect do |range|
        range.include?(number_of_neighbours)
      end
    end
  end


  class Universe
    def initialize(dimension, rule_for_live_cells, rule_for_dead_cells)
      @live_cells = Set.new
      @rule_for_live_cells = rule_for_live_cells
      @rule_for_dead_cells = rule_for_dead_cells
      @dimension = dimension
    end


    def set(coordinates)
      ensure_correct_dimension(coordinates)
      @live_cells.add(coordinates)
    end


    def tick

      next_universe_state = Set.new
      calculated_cells    = Set.new

      make_decision = lambda{|rule, coords|
        if rule.is_triggered_on?(neighbours_for(coords))
          next_universe_state.add(coords)
        end
      }

      # first we calculate populated cells
      @live_cells.each do |coordinates|
        calculated_cells.add(coordinates)

        make_decision.call(@rule_for_live_cells, coordinates)
      end

      # and then all neighbour cells

      @live_cells.each do |live_cell_coordinates|

        live_cell_coordinates.around.each do |coordinates|
          next if calculated_cells.member?(coordinates)
          calculated_cells.add(coordinates)

          # a neighbour cell can be empty or populated, but all populated cells have
          # already been calculated and are in calculated_cells, so by this time
          # we are guaranteed to only calculate empty cells, hence, @rule_for_dead_cells

          make_decision.call(@rule_for_dead_cells, coordinates)
        end
      end

      @live_cells = next_universe_state

    end

    def dump
      @live_cells.map{|coordinates| coordinates.inspect}.join("\n")
    end

    private

    def neighbours_for(coordinates)
      coordinates.around.select{|n_coordinates| alive_at?(n_coordinates)}.size
    end

    def alive_at?(coordinates)
      @live_cells.member?(coordinates)
    end


    def ensure_correct_dimension(coordinates)
      unless coordinates.dimension == @dimension
        raise "#{coordinates.dimension}-dimension coordinates are not valid for the #{@dimension}-dimension world"
      end
    end

  end

  class TwoDimensionalWorld < Universe
    def initialize
      rule_for_live_cells = Rule.new(2..3)
      rule_for_dead_cells = Rule.new(3..3)
      super(2, rule_for_live_cells, rule_for_dead_cells)
    end

    def set(x,y)
      super(Coordinates[x,y])
    end


    def view

      min_x, max_x, min_y, max_y = define_border_of_the_inhabitable_fragment
      res = ''
      min_x.upto(max_x) do |x|
        min_y.upto(max_y) do |y|
          if alive_at?(Coordinates[x,y])
            res << '+'
          else
            res << ' '
          end
        end
        res << "\n"
      end
      res
    end


    def define_border_of_the_inhabitable_fragment
      [0,1].map do |dim_index|
        all = @live_cells.map{|coord| coord[dim_index]}
        [all.min, all.max]
      end.flatten
    end
  end

end


if $0 == __FILE__

  life_2_dimensions = GameOfLife::TwoDimensionalWorld.new

  # Blinker
  life_2_dimensions.set(1,2)
  life_2_dimensions.set(1,3)
  life_2_dimensions.set(1,4)

  # Toad
  life_2_dimensions.set(0,9)
  life_2_dimensions.set(1,8)
  life_2_dimensions.set(1,9)
  life_2_dimensions.set(2,8)
  life_2_dimensions.set(2,9)
  life_2_dimensions.set(3,8)


  puts "=== Classic 2-dimensional infinite world, canonic rules  ===\n\n"
  puts "Blinker and toad oscillators (simplest patterns), see http://en.wikipedia.org/wiki/Conway's_Game_of_Life#Examples_of_patterns"

  5.times do
    puts
    # puts life_2_dimensions.dump
    puts life_2_dimensions.view
    puts '_____________________'
    life_2_dimensions.tick
  end
end