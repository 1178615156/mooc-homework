class A
  def initialize()
    @hello = 1
  end
  def hello
    @hello
  end
end

class B < A
end
a = A.new()
a.hello=2
