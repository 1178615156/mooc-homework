# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  All_My_Pieces = Piece::All_Pieces +
      [
          [
              [[-1, 0], [0, 0], [1, 0], [2, 0], [3, 0]],
              [[0, -1], [0, 0], [0, 1], [0, 2], [0, 3]]
          ],
          Piece.rotations([[0, -1], [0, 0], [0, 1], [1, 0], [1, 1]]),
          Piece.rotations([[0, 0], [-1, 0], [0, 1]])
      ]


  def self.cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end

  Cheat_Piece =[
      [[0, 0]]
  ]


end

class MyBoard < Board
  # your enhancements here
  @is_cheat = false

  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end

  def next_piece
    if @is_cheat && score >= 100
      @current_block = MyPiece.cheat_piece (self)
      @current_pos = nil
      @is_cheat = false
      @score -= 100
    else
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each {|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def clockwise180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    @is_cheat = true
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end


  def key_bindings
    super
    @root.bind('u', proc {@board.clockwise180})
    @root.bind('c', proc {@board.cheat})
  end

end

