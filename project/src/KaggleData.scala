import scala.collection.immutable.List
import scala.collection.immutable.HashMap

/** Companion object for classes implementing KaggleData */
object KaggleData{
  
  //index in input data that represents the id
  val idIndex = 0
  
  //TODO - index in input data that represents the label.
  //Tentatively: fully_funded
  val labelIndex = 37
  
  //Indices in combined_*.csv that are already numbers
  val numericIndices = List(3, 4, 5, 8, 28, 29, 30, 31, 43, 44, 45)
  
  //Indices in combined_*.csv that are unique strings - use hashcode
  val stringIndices = List(0, 1, 2, 6, 7, 10, 11, 21, 22, 23, 24)
  
  //Indices that are boolean values - either t or f
  val booleanIndices = List(12, 13, 14, 15, 16, 17, 19, 20, 32, 33, 35, 36, 37, 38, 39, 40, 41, 42)
  
  //Indices that are enum values - finite list of possible values
  val enumIndices = List(9, 18, 25, 26, 27)
  
  //Indices that require special parsing as dates
  val dateIndices = List(34)
  
  //All indices contatinated together and sortd. Should be perfect range [0, 45]. Use for testing.
  val allIndices = (numericIndices ++ stringIndices ++ booleanIndices ++ enumIndices ++ dateIndices).sortWith((a, b) => a < b)

  if(! allIndices.zip(0 to 45).foldLeft(true)((acc, e) => acc && e._1.equals(e._2))){
    println("Error with all Indices for KaggleData reading")
  }
  
  /** Returns a double value for the given string. 
   *  The index of the field (in combined_*.csv) is given.
   *  s may be empty - must handle that poor data case.
   *  Always returns Integer.MIN_VALUE in the case that the input string is empty
   */
  @throws[RuntimeException]
  def convertValue(fieldIndex : Int, s : String) : Double = {
    if(s.equals("")) Integer.MIN_VALUE
    else{
      if(numericIndices.contains(fieldIndex)){ //Parse the string, should be a double already
        s.toDouble 
      } else if(stringIndices.contains(fieldIndex)){ //Is non-finite string, return its hashcode (bit normalized)
        s.hashCode() / 100000
      } else if(booleanIndices.contains(fieldIndex)){ //should be t or f, return 1 for t, 0 for f
        if(s.equalsIgnoreCase("t")) 1 
        else if(s.equalsIgnoreCase("f")) 0
        else throw new RuntimeException("Bad boolean string encountered: " + s)
      } else if(enumIndices.contains(fieldIndex)){  //Outsource enum parsing to other method
        parseEnum(fieldIndex, s)
      } else if(dateIndices.contains(fieldIndex)){ //Parse as a date
        //Gives two digits for each field. A bit of overkill, but easiest to re-parse to date.
        //In order year, month, day
        val mString = s.substring(0, s.indexOf('/'))
        val dString = s.substring(s.indexOf('/') + 1, s.lastIndexOf('/'))
        val yString = s.substring(s.lastIndexOf('/') + 1)
        yString.toInt + mString.toInt / 100 + dString.toInt / 10000
      }
      else{
        throw new RuntimeException("Bad fieldIndex : " + fieldIndex) 
      }
    }
  }
  
  private val enumMap : Map[Int, List[String]] = 
   Map(9 -> List("rural", "suburban", "urban"), 
       18 -> List("Mr.", "Ms.", "Mrs.", "Dr."),
       25 -> List("Books", "Supplies", "Technology", "Trips", "Visitors", "Other"),
       26 -> List("low poverty", "moderate poverty", "high poverty", "highest poverty"),
       27 -> List("Grades PreK-2", "Grades 3-5", "Grades 6-8", "Grades 9-12")
      )
  
  /** Returns a double corresponding to the given string, for the given fieldIndex */
  @throws[RuntimeException]
  private def parseEnum(fieldIndex : Int, s : String) : Double = {
    enumMap.get(fieldIndex) match{
      case None => throw new RuntimeException(fieldIndex + " Isn't an enum field")
      case Some(lst) => {
        val l = lst.indexOf(s)
        if(l == -1 ) throw new RuntimeException(s + " Isn't a valid value of enumlist " + lst)
        l
      }
    } 
  }
  
  /** Initializes a KaggleData instance from the given map and id. Remove the
   *  label and id indices from the input map before constructing */
  def init(id : String, vals : Map[Int, Double]) : KaggleData = {
    new KaggleData(id, KaggleLabel.fromInt(vals(labelIndex).toInt), vals - labelIndex - idIndex)
  }
}

/** Possible labelings for a KaggleData - multi class, not multi label */
object KaggleLabel extends Data.Label{
  val FALSE, TRUE = Value
  
  //TODO - current setting is for fully_funded
  /** Mapping of int to its corresponding KaggleLabel */
  private val intToLabelMap : Map[Int, KaggleLabel.Value] = Map(0 -> FALSE, 1 -> TRUE)
  
  //TODO - current setting for is fully_funded
  /** Mapping of label to its corresponding int */
  private val labelToIntMap : Map[KaggleLabel.Value, Int] = Map(FALSE -> 0, TRUE -> 1)
  
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
  
  /** Hash a KaggleData on its id */
  override def hashCode : Int = {
    id.hashCode()
  }
  
  /** Two KaggleDatas are equal if they have the same id
   *  Use ids to distinguish between kaggledatas
   */
  override def equals(o : Any) : Boolean = {
    if(! o.isInstanceOf[KaggleData]) false
    else id.equals(o.asInstanceOf[KaggleData].id)
  }
  
  //TODO
  /** Valid length for KaggleData is any length. Maybe change this? */
  override def lengthOk = (a : Int) => true
  
  /** Possible labelings of a Kaggle Data */
  override val labels = KaggleLabel.values.toList
  
  /** Returns a string representing this data that can be put into SVM-Light */
  def toSVMString : String = {
    vals.toList.sortWith((a, b) => a._1 < b._1).foldLeft(KaggleLabel.toInt(label) + " ")(
        (a, b) => a + b._1 + ":" + b._2 + " ") + "#" + id
  }
}