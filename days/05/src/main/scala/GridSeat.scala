object GridSeat {
  case class Dimension(rowMin: Int, rowMax: Int, columnMin: Int, columnMax: Int) {
    def apply(seat: GridSeat): Boolean =
      rowMin <= seat.row && seat.row <= rowMax &&
      columnMin <= seat.column && seat.column <= columnMax
  }
 
  def all(using dim: Dimension): Set[GridSeat] = (
    for {
      row <- dim.rowMin to dim.rowMax
      column <- dim.columnMin to dim.columnMax
    } yield GridSeat(row, column)
  ).toSet
}

case class GridSeat(row: Int, column: Int) {
  val seatId = column * 8 + row
  
  def neighbors(using dim: GridSeat.Dimension): Set[GridSeat] = Set(
    GridSeat(row, column - 1),
    GridSeat(row + 1, column),
    GridSeat(row, column + 1),
    GridSeat(row - 1, column),
  ).filter(dim(_))
}
