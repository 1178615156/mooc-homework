
require "./hw7.rb"


x = LineSegment.new(5.0,7.0,9.0,9.0).intersect(LineSegment.new(5.0,7.0,6.0,-1.0))
puts x.x
puts x.y
