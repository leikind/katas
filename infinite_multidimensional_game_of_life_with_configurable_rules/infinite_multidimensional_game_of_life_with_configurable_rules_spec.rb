require './infinite_multidimensional_game_of_life_with_configurable_rules.rb'


describe GameOfLife::Permutator do
  it 'should give all possible permitations of 2 2-element tuples' do

    perms =  GameOfLife::Permutator.permutations [[1,2], [3,4]]

    perms.size.should == 4

    [[1, 3], [1, 4], [2, 3], [2, 4]].each do |tuple|
      perms.should include(tuple)
    end
  end

  it 'should give all possible permitations of 3 2-element tuples' do
    perms =  GameOfLife::Permutator.permutations [[1,2], [3,4], [5,6]]

    perms.size.should == 8

    [[1, 3, 5], [1, 3, 6], [1, 4, 5], [1, 4, 6], [2, 3, 5], [2, 3, 6], [2, 4, 5], [2, 4, 6]].each do |tuple|
      perms.should include(tuple)
    end
  end

  it 'should give all possible permitations of 2 3-element tuples' do
    perms =  GameOfLife::Permutator.permutations [[1,2, 3], [4,5, 6]]

    perms.size.should == 9

    [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4], [3, 5], [3, 6]].each do |tuple|
      perms.should include(tuple)
    end

  end
end


describe GameOfLife::Coordinates do
  it 'should know its dimension' do
    GameOfLife::Coordinates[1,2,3].dimension.should == 3
    GameOfLife::Coordinates[1,2,3,1].dimension.should == 4
  end


  it 'should be equal to the same coordinates' do
    GameOfLife::Coordinates[1,2,3].eql?(GameOfLife::Coordinates[1,2,3]).should be_true
  end


  it 'should return neighbouring coordinates for a 2 dimensional coordinate' do
    neighbours = GameOfLife::Coordinates[1,2].around
    neighbours.size.should == 8

    [[0, 1], [0, 2], [0, 3], [1, 1], [1, 3], [2, 1], [2, 2], [2, 3]].each do |x,y|
      neighbours.should include(GameOfLife::Coordinates[x,y])
    end
  end

  it 'should return neighbouring coordinates for a 3 dimensional coordinate' do
    neighbours = GameOfLife::Coordinates[1,2,3].around

    neighbours.size.should == 26

    [[0, 1, 2], [0, 1, 3], [0, 1, 4], [0, 2, 2], [0, 2, 3], [0, 2, 4], [0, 3, 2], [0, 3, 3], [0, 3, 4], [1, 1, 2], [1, 1, 3],
        [1, 1, 4], [1, 2, 2], [1, 2, 4], [1, 3, 2], [1, 3, 3], [1, 3, 4], [2, 1, 2], [2, 1, 3], [2, 1, 4], [2, 2, 2], [2, 2, 3],
        [2, 2, 4], [2, 3, 2], [2, 3, 3], [2, 3, 4]].each do |x,y,z|
      neighbours.should include(GameOfLife::Coordinates[x,y,z])
    end
  end
end

describe GameOfLife::Rule do
  it 'should be initializable with one range' do
    GameOfLife::Rule.new(1..2).should be_an_instance_of(GameOfLife::Rule)
  end

  it 'should be intializable with multiple range' do
    GameOfLife::Rule.new(1..2, 5..6).should be_an_instance_of(GameOfLife::Rule)
  end

  it 'should be triggered correctly' do
    rule = GameOfLife::Rule.new(1..2, 5..6)

    rule.is_triggered_on?(0).should be_false
    rule.is_triggered_on?(1).should be_true
    rule.is_triggered_on?(2).should be_true
    rule.is_triggered_on?(3).should be_false
    rule.is_triggered_on?(4).should be_false
    rule.is_triggered_on?(5).should be_true
    rule.is_triggered_on?(6).should be_true
    rule.is_triggered_on?(7).should be_false
  end


end

describe GameOfLife::TwoDimensionalWorld do

  before(:each) do
    GameOfLife::TwoDimensionalWorld.send(:public, :alive_at?)

    @life_2_dimensions = GameOfLife::TwoDimensionalWorld.new

    @life_2_dimensions.set(1,2)
    @life_2_dimensions.set(1,3)
    @life_2_dimensions.set(1,4)
  end

  it 'should be possible to create an initial world' do

    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[0,0]).should be_false
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,2]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,3]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,4]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[5,6]).should be_false

  end


  it 'should change state with a blinker pattern' do
    @life_2_dimensions.tick.should be_true

    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[0,3]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,3]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[2,3]).should be_true

    @life_2_dimensions.tick.should be_true

    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,2]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,3]).should be_true
    @life_2_dimensions.alive_at?(GameOfLife::Coordinates[1,4]).should be_true
  end

  it 'should change according to the toad pattern'  do
    @life_2_dimensions.set(0,9)
    @life_2_dimensions.set(1,8)
    @life_2_dimensions.set(1,9)
    @life_2_dimensions.set(2,8)
    @life_2_dimensions.set(2,9)
    @life_2_dimensions.set(3,8)

    @life_2_dimensions.tick.should be_true

    [[1, 3], [0, 9], [3, 8], [0, 3], [2, 3], [0, 8], [1, 10], [2, 7], [3, 9]].each do |x,y|
      @life_2_dimensions.alive_at?(GameOfLife::Coordinates[x,y]).should be_true
    end


  end

end