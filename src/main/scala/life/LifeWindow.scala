//> using scala "3.1.3"
//> using lib "se.lth.cs::introprog:1.3.1"

package life

import introprog.PixelWindow
import introprog.PixelWindow.Event
import java.awt.{Color as JColor}

object LifeWindow:
  val EventMaxWait = 1 // milliseconds
  var NextGenerationDelay = 200 
  var blocksize = 15
  val black = new JColor( 0, 0, 0)
  val alive = new JColor(242, 128, 161)
  val white = new JColor(255, 255, 255)
  // milliseconds
  // lägg till fler användbara konstanter här tex färger etc.

class LifeWindow(rows: Int, cols: Int):
  import LifeWindow.* // importera namn från kompanjon

  var life = Life.empty(rows, cols)
  val window: PixelWindow = 
    new PixelWindow(rows * blocksize, cols * blocksize, "GAME OF LIFE", black)
  var quit = false
  var play = false

  def drawGrid(): Unit = 
    for i <- 0 to rows do
      window.line(i * blocksize, 0, i * blocksize, cols * blocksize, white)
    for y <- 0 to cols do
      window.line(0, y * blocksize, 0, rows * blocksize, white)

  def drawCell(row: Int, col: Int, value: Boolean): Unit = 
    window.fill(row + 1, col + 1, blocksize - 1, blocksize - 1, if value then alive else black)

  def update(newLife: Life): Unit =
    val oldLife = life
    life = newLife
    life.cells.foreachIndex{ (r, c) => drawCell(r, c, apply(r, c))  }

  def handleKey(key: String): Unit = 
    if key == " " && !play then 
      play == false
    
    else if key == " " && play then 
      play == true

    else if key == "r" then 
      update(Life.random(rows, cols))

    else if key == "Enter" then
      play == false
      update(Life.evolved()) 

    else if key == "Backspace" then
      update(Life.empty(rows, cols))

  def handleClick(pos: (Int, Int)): Unit = 
    toggled((pos(0) / blocksize).round.toInt, (pos(1) / blocksize).round.toInt)

  def loopUntilQuit(): Unit = while !quit do
    val t0 = System.currentTimeMillis
    if play then update(life.evolved())
    window.awaitEvent(EventMaxWait)
    while window.lastEventType != PixelWindow.Event.Undefined do
      window.lastEventType match
        case Event.KeyPressed  =>  handleKey(window.lastKey)
        case Event.MousePressed => handleClick(window.lastMousePos)
        case Event.WindowClosed => quit = true
        case _ =>
      window.awaitEvent(EventMaxWait)
    val elapsed = System.currentTimeMillis - t0
    Thread.sleep((NextGenerationDelay - elapsed) max 0)

  def start(): Unit = { drawGrid(); loopUntilQuit() }
