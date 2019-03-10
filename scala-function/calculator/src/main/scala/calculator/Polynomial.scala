package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(
      b() * b() - 4 * a() * c()
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      delta() match {
        case e if e < 0  => Set()
        case e if e == 0 => Set(-b() / (2 * a()))
        case e if e > 0  => Set(
          (-b() + math.sqrt(e)) / (2 * a()),
          (-b() - math.sqrt(e)) / (2 * a())
        )
      }
    )
  }
}
