package life

case class Matrix[A](data: Vector[Vector[A]]):
    require(data.forall(row => row.length == data(0).length))

    def apply(row: Int, col: Int): A = data(row)(col)

    val dim: (Int, Int) = (data.length, data(0).length)

    def updated(row: Int, col: Int)(value: A): Matrix[A] = 
        Matrix(data.updated(row, data(row).updated(col, value)))
    
    def foreachIndex(f: (Int, Int) => Unit): Unit = 
        for r <- data.indices; c <- data(r).indices do f(r, c)
    
    override def toString = 
        s"""Matrix of dim $dim:\n${ data.map(_.mkString(" ")).mkString("\n") }""" 


object Matrix:
  def fill[A](dim: (Int, Int))(value: A): Matrix[A] =
    Matrix[A](Vector.fill(dim._1, dim._2)(value))