import scala.collection.immutable.List

/** Companion object for classes implementing KaggleData */
object KaggleData{
  //TODO
  /** field index in kaggleData instances that corresponds to the label */
  val labelIndex = -1
  
  //TODO
  /** Number of donation fields */
  val donationFields = -1
  
  //TODO
  /** Number of outcome fields */
  val outcomeFields = -1
  
  //TODO
  /** Number of project fields */
  val projectFields = -1
  
  //TODO
  /** Number of resource fields */
  val resourceFields = -1
  
  /** Initializes a KaggleData instance from the given id and
   *  donation map, outcome map, projects map, resources map
   */
  def init(id : String, donationVals : Map[Int, Double],
           outcomeVals : Map[Int, Double], projectVals : Map[Int, Double],
           resourceVals : Map[Int, Double]) : KaggleData = {
    //TODO - compile maps into one, call other init method
    return null
  }
  
  
  /** Initializes a KaggleData instance from the given map and id */
  def init(id : String, vals : Map[Int, Double]) : KaggleData = {
    new KaggleData(id, KaggleLabel.fromInt(vals(labelIndex).toInt), vals)
  }
}

/** Possible labelings for a KaggleData - multi class, not multi label */
object KaggleLabel extends Data.Label{
  
  //TODO
  /** Mapping of int to its corresponding KaggleLabel */
  private val intToLabelMap : Map[Int, KaggleLabel.Value] = null
  
  //TODO
  /** Mapping of label to its corresponding int */
  private val labelToIntMap : Map[KaggleLabel.Value, Int] = null
  
  /** Returns the corresponding KaggleLabel for the given double.
   *  Default KaggleLabel.NONE */
  def fromInt(i : Int) : KaggleLabel.Value = {
    intToLabelMap.getOrElse(i, KaggleLabel.NONE)
  }
  
    /** Returns the corresponding double for the given kaggleLabel
     *  Default -9999 */
  def toInt(v : KaggleLabel.Value) : Int = {
    labelToIntMap.getOrElse(v, -9999)
  }
}

/** Unioned data type of data gotten from kaggle.
 *  All kaggle data should be able to return a string of itself as a row for svm-processing */
class KaggleData(val id : String, override val label : KaggleLabel.Value, override val vals : Map[Int, Double]) 
	extends Data[KaggleLabel.Value](label, vals){
  
  /** Valid length for KaggleData is any length. Maybe change this? */
  override def lengthOk = (a : Int) => true
  
  /** Possible labelings of a Kaggle Data */
  override val labels = KaggleLabel.values.toList
  
  /** Returns a string representing this data that can be put into SVM-Light */
  def toSVMString : String = {
    //TODO
    ""
  }
}