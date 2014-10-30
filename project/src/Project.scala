import scala.collection.immutable.List

/** Possible labelings for a project - either funded or not */
object ProjectLabel extends Data.Label{
  val FUNDED, NOT_FUNDED = Value
}

/** An instance represents a project */
class Project(override val label : ProjectLabel.Value, override val vals : Map[Int, Double]) extends Data[ProjectLabel.Value](label, vals) {

  //TODO - specify length?
  override def lengthOk = (a : Int) => true
  
  /** Returns the possible labelings for this project */
  override def labels = ProjectLabel.values.toList
}