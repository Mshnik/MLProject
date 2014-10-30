import scala.collection.immutable.List

/** Companion object for classes implementing KaggleData */
object KaggleData{
  //TODO - If possible, common behavior between the svmString methods goes here
}

/** Parent class for all types of data gotten from kaggle
 *  All kaggle data should be able to return a string of itself as a row for svm-processing */
abstract class KaggleData[T <: Data.Label#Value](override val label : T, override val vals : Map[Int, Double]) extends Data[T](label, vals){
  
  /** Returns a string representing this data that can be put into SVM-Light */
  def toSVMString : String
}

/** Possible labelings for a project */
object ProjectLabel extends Data.Label{
  
}

/** An instance represents a project */
class Project(override val label : ProjectLabel.Value, override val vals : Map[Int, Double]) extends KaggleData[ProjectLabel.Value](label, vals) {

  //TODO - specify length?
  override def lengthOk = (a : Int) => true
    
  /** The possible labelings for this project */
  override val labels = ProjectLabel.values.toList
  
  /** Transform the given project into a string for use in SVM processing */
  override def toSVMString : String = {
    //TODO
    ""
  }
}

/** Possible labelings for an outcome */
object OutcomeLabel extends Data.Label{

}

/** An instance represents an outcome */
class Outcome(override val label : OutcomeLabel.Value, override val vals : Map[Int, Double]) extends KaggleData[OutcomeLabel.Value](label, vals) {

  //TODO - specify length?
  override def lengthOk = (a : Int) => true
  
  /** The possible labelings for this project */
  override val labels = OutcomeLabel.values.toList
  
  /** Transform the given outcome into a string for use in SVM processing */
  override def toSVMString : String = {
    //TODO
    ""
  }
}

/** Possible labelings for a donation */
object DonationLabel extends Data.Label{

}

/** An instance represents an donation */
class Donation(override val label : DonationLabel.Value, override val vals : Map[Int, Double]) extends KaggleData[DonationLabel.Value](label, vals) {

  //TODO - specify length?
  override def lengthOk = (a : Int) => true
  
  /** The possible labelings for this donation */
  override val labels = DonationLabel.values.toList
  
  /** Transform the given donation into a string for use in SVM processing */
  override def toSVMString : String = {
    //TODO
    ""
  }
}