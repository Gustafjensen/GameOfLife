package life

case class Life(cells: Matrix[Boolean]):

  /** Ger true om cellen på plats (row, col) är vid liv annars false.
    * Ger false om indexeringen är utanför universums gränser.
    */
  def apply(row: Int, col: Int): Boolean = 
    if row > Main.dim._1 || col > Main.dim._2 || row < 0 || col < 0 then false 
    else cells(row, col)

  /** Sätter status på cellen på plats (row, col) till value. */
  def updated(row: Int, col: Int, value: Boolean): Life = 
    Life(cells.updated(row, col)(value))

  /** Växlar status på cellen på plats (row, col). */
  def toggled(row: Int, col: Int): Life = 
    updated(row, col, !apply(row, col))
    

  /** Räknar antalet levande grannar till cellen i (row, col). */
  def nbrOfNeighbours(row: Int, col: Int): Int = 
    var x = 0
    if apply(row + 1, col - 1)  then x += 1 
    else if apply(row + 1, col)      then x += 1 
    else if apply(row + 1, col + 1)  then x += 1 
    else if apply(row , col + 1)     then x += 1 
    else if apply(row - 1, col + 1)  then x += 1 
    else if apply(row - 1, col)      then x += 1 
    else if apply(row - 1, col - 1)  then x += 1 
    else if apply(row, col - 1)      then x += 1 
    x
    
    /** Skapar en ny Life-instans med nästa generation av universum.
     * Detta sker genom att applicera funktionen \code{rule} på cellerna.
    */
  def evolved(rule: (Int, Int, Life) => Boolean = Life.defaultRule): Life =
    var nextGeneration = Life.empty(cells.dim)
    cells.foreachIndex( (r,c) => nextGeneration.updated(r, c, rule(r, c, this))
    )
    nextGeneration

  /** Radseparerad text där 0 är levande cell och - är död cell. */
  override def toString = 
    s"""Matrix of dim ${cells.dim}:\n${ cells.data.map(_.map(i => if i then "0"; else "-").mkString(" ")).mkString("\n") }"""

object Life:
  /** Skapar ett universum med döda celler. */
  def empty(dim: (Int, Int)): Life = 
    Life(Matrix(Vector.fill(dim(0), dim(1))(false)))

  /** Skapar ett unviversum med slumpmässigt liv. */
  def random(dim: (Int, Int)): Life = 
    Life(Matrix(Vector.fill(dim(0), dim(1))(scala.util.Random.nextBoolean())))
    
  /** Implementerar reglerna enligt Conways Game of Life. 
   * 

  */
  def defaultRule(row: Int, col: Int, current: Life): Boolean = 
    var x = current.nbrOfNeighbours(row, col)
    if current(row, col) && x == 2 || x == 3 then true
    else if !current(row, col) && x == 3 then true
    else false 


