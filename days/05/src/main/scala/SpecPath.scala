object SpecPath {
  enum SpecDimension {
    case Row, Column
  }
  enum Step(val char: Char, val dimension: SpecDimension) {
    case Front extends Step('F', SpecDimension.Column)
    case Back extends Step('B', SpecDimension.Column)
    case Right extends Step('R', SpecDimension.Row)
    case Left extends Step('L', SpecDimension.Row)
  }
  enum Error {
    case UnderspecifiedSeat(spec: SpecPath)
    case Overspecification(spec: SpecPath)

    def errorMessage: String = this match
      case UnderspecifiedSeat(path) => s"Underspecified seat: ${toString(path)}" +
        (if !path.isSpecified(SpecDimension.Row) then s"\n\tcolumn is too vague with ${path.colMin} to ${path.colMax}" else "") +
        (if !path.isSpecified(SpecDimension.Column) then s"\n\trow is too vague with ${path.rowMin} to ${path.rowMax}" else "")
      case Overspecification(path) => s"Overspecified $path with '${path.specs.last}' from '${toString(path)}'!'"

    private[this] def toString(specPath: SpecPath): String = specPath.specs.map(_.char).mkString
  }
}

case class SpecPath(colMin: Int, colMax: Int, rowMin: Int, rowMax: Int, specs: List[SpecPath.Step] = List.empty) {
  require((rowMax - rowMin) % 2 != 0 || rowMax == rowMin, s"Illegal specification for rows $rowMin to $rowMax is not evenly splittable")
  require((colMax - colMin) % 2 != 0 || colMax == colMin, s"Illegal specification for columns $colMin to $colMax is not evenly splittable")
  import SpecPath.SpecDimension._
  import SpecPath._
  import SpecPath.Step._
  
  def step(specification: Step): Either[Error, SpecPath] = { 
    val appended = copy(specs = specs :+ specification)
    val step = specification.dimension match
      case Column => (colMax - colMin) / 2
      case Row => (rowMax - rowMin) / 2
    specification match
      case overSpec if isSpecified(overSpec.dimension) => Left(Error.Overspecification(appended))
      case Step.Front => Right(appended.copy(colMax = this.colMax-1 - step))
      case Step.Back => Right(appended.copy(colMin = this.colMin+1 + step))
      case Step.Right => Right(appended.copy(rowMin = this.rowMin+1 + step))
      case Step.Left => Right(appended.copy(rowMax = this.rowMax-1 - step))
  }

  def isSpecified(dimension: SpecDimension): Boolean = dimension match
    case Column => colMax == colMin
    case Row => rowMax == rowMin
  
  def isSpecified: Boolean = SpecDimension.values.forall(isSpecified(_))
  def seatId: Either[Error, Int] = if isSpecified then Right(colMin * 8 + rowMin) 
                                                else Left(Error.UnderspecifiedSeat(this)) 

  override def toString: String = s"Spec[rows: $rowMin-$rowMax / cols: $colMin-$colMax]"
}