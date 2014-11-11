package data

import scala.collection.immutable.List

/** Companion object for classes implementing KaggleData */
object KaggleData{
  
  /** index in input data that represents the id */
  val idIndex = 0
  
  /** Tentatively: fully_funded */
  val labelIndex = 37
  
  /** Indices in combined_*.csv that are already numbers */
  val numericIndices = List(4, 5, 29, 30, 31)
  
  /** Indices that are boolean values - either t or f */
  val booleanIndices = List(12, 13, 14, 15, 16, 17, 19, 20, 32, 33, 37)
  
  /** Indices that are enum values - finite list of possible values */
  val enumIndices = List(9, 18, 21, 22, 23, 24, 25, 26, 27)
  
  /** Indices that require special parsing as dates */
  val dateIndices = List(34)
  
  /** Indices to ignore (give value 0 no matter what input) */
  val ignoreIndices = List(0, 1, 2, 3, 6, 7, 8, 10, 11, 28, 35, 36, 38, 39, 40, 41, 42, 43, 44, 45)
  
  /** All indices contatinated together and sorted. Should be perfect range [0, 45]. Use for testing. */
  val allIndices = (numericIndices ++ booleanIndices ++ enumIndices ++ dateIndices ++ ignoreIndices).sortWith((a, b) => a < b)

  /** Use to translate index into meaning in combined_*.csv */
  val indexName = Map(0 -> "ID", 4 -> "Latitude", 5 -> "Longitude", 9 -> "School Metro", 12 -> "School Charter",
                      13 -> "School Magnet", 14 -> "School Year Round", 15 -> "School Nlns", 16 -> "School Kipp",
                      17 -> "School Charter Ready Promise", 18 -> "Teacher Prefix", 19 -> "Teach For America",
                      20 -> "Teacher NY Teaching Fellow", 21 -> "Primary Focus Subject", 22 -> "Primary Focus Area",
                      23 -> "Secondary Focus Subject", 24 -> "Secondary Focus Area", 25 -> "Resource Type",
                      26 -> "Poverty Level", 27 -> "Grade Level", 29 -> "Total Price", 30 -> "Total Price Plus Opt",
                      31 -> "Students Reached", 32 -> "Eligible For Double Your Impact", 33 -> "Eligible Almost Home Match",
                      34 -> "Date Posted", 37 -> "Is Fully Funded")
  
  if(! allIndices.zip(0 to 45).foldLeft(true)((acc, e) => acc && e._1.equals(e._2))){
    println("Error with all Indices for KaggleData reading")
  }
  
  /** Miliseconds from epoch to jan 1 2000 */
  private val twoThousand = new java.util.Date("1/1/2000").getTime()
  
  /** Returns a double value for the given string. 
   *  The index of the field (in combined_*.csv) is given.
   *  s may be empty - must handle that poor data case.
   *  Always returns Integer.MIN_VALUE in the case that the input string is empty
   */
  @throws[RuntimeException]
  def convertValue(fieldIndex : Int, s : String) : Double = {
    if(s.equals("")) 0.0
    else{
      if(numericIndices.contains(fieldIndex)){ //Parse the string, should be a double already
        s.toDouble 
      } else if(booleanIndices.contains(fieldIndex)){ //should be t or f, return 1 for t, 0 for f
        if(s.equalsIgnoreCase("t")) 1 
        else if(s.equalsIgnoreCase("f")) -1
        else throw new RuntimeException("Bad boolean string encountered: " + s)
      } else if(enumIndices.contains(fieldIndex)){  //Outsource enum parsing to other method
        parseEnum(fieldIndex, s)
      } else if(dateIndices.contains(fieldIndex)){ //Parse as a date
        //Write as (epoch ms - time in jan 1 2000) / (1000 * 60 * 60 * 24) = days since 2000
        val d = new java.util.Date(s)
        (d.getTime() - twoThousand) / (1000 * 60 * 60 * 24)
      } else if (ignoreIndices.contains(fieldIndex)){
        0.0 //Put 0 here, so this field ends up ignored and removed.
      }
      else{
        throw new RuntimeException("Bad fieldIndex : " + fieldIndex) 
      }
    }
  }
  
  @throws[RuntimeException]
  def unconvertValue(fieldIndex : Int, d : Double) : String = {
    if(d.equals(0.0)) ""
    else{
      if(numericIndices.contains(fieldIndex)){ //Parse the string, should be a double already
        d.toString
      } else if(booleanIndices.contains(fieldIndex)){ //should be t or f, return 1 for t, 0 for f
        if(d.equals(1)) "t" 
        else if(d.equals(-1)) "f"
        else throw new RuntimeException("Bad boolean number encountered: " + d)
      } else if(enumIndices.contains(fieldIndex)){  //Outsource enum parsing to other method
        unparseEnum(fieldIndex, d)
      } else if(dateIndices.contains(fieldIndex)){ //Parse as a date
        //Write as (epoch ms - time in jan 1 2000) / (1000 * 60 * 60 * 24) = days since 2000
        val dat = new java.util.Date((d * (1000 * 60 * 60 * 24) + twoThousand).toLong)
        dat.toString()
      } else if (ignoreIndices.contains(fieldIndex)){
        "" //Put nothing here here, so this field ends up ignored and removed.
      }
      else{
        throw new RuntimeException("Bad fieldIndex : " + fieldIndex) 
      }
    }
  }
  
  private val enumMap : Map[Int, List[String]] ={
    val m = Map(9 -> List("rural", "suburban", "urban"), 
       18 -> List("Mr.", "Ms.", "Mrs.", "Dr."),
       21 -> List("Performing Arts", "Health & Life Science", "Applied Sciences", "Sports", "Other", "Music", 
                  "Early Development", "Mathematics", "Character Education", "Social Sciences", "Nutrition", 
                  "Environmental Science", "Health & Wellness", "Parent Involvement", "Gym & Fitness", 
                  "History & Geography", "Literacy", "Economics", "Community Service", "Extracurricular", 
                  "Foreign Languages", "College & Career Prep", "ESL", "Visual Arts", "Literature & Writing", 
                  "Special Needs", "Civics & Government"),
       22 -> List("Special Needs", "Health & Sports", "Math & Science", "Music & The Arts", "History & Civics", 
                  "Literacy & Language", "Applied Learning"),
       25 -> List("Books", "Supplies", "Technology", "Trips", "Visitors", "Other"),
       26 -> List("low poverty", "moderate poverty", "high poverty", "highest poverty"),
       27 -> List("Grades PreK-2", "Grades 3-5", "Grades 6-8", "Grades 9-12")
      )
    m + ((23, m(21)), (24, m(22)))
  } 
   
  
  /** Returns a double corresponding to the given string, for the given fieldIndex */
  @throws[RuntimeException]
  private def parseEnum(fieldIndex : Int, s : String) : Double = {
    enumMap.get(fieldIndex) match{
      case None => throw new RuntimeException(fieldIndex + " Isn't an enum field")
      case Some(lst) => {
        val l = lst.indexOf(s) + 1
        if(l == -1 ) throw new RuntimeException(s + " Isn't a valid value of enumlist " + lst)
        l
      }
    } 
  }
  
  @throws[RuntimeException]
  /** Returns a string corresponding to the given double, for the given fieldIndex */
  private def unparseEnum(fieldIndex : Int, d : Double) : String = {
    enumMap.get(fieldIndex) match{
      case None => throw new RuntimeException(fieldIndex + " Isn't an enum field")
      case Some(lst) => {
        lst(d.toInt + 1)
      }
    } 
  }
  
  /** Initializes a KaggleData instance from the given map and id. Remove the
   *  label and id indices from the input map before constructing */
  def init(id : String, vals : Map[Int, Double]) : KaggleData = {
    new KaggleData(id, KaggleLabel.fromInt(vals(labelIndex).toInt), vals.filter(a => ! a._2.equals(0.0)) - labelIndex - idIndex )
  }
  
  /** Return a new list of data that has each attribute normalized (minus mean, div std dev) */
  def normalize(data : List[KaggleData]) : List[KaggleData] = {
    val m = Data.mean(data)
    val s = Data.stDev(data, m)
    data.map(a => initNormalized(a.id, a.label, a.vals, m, s))
  }
  
  /** Initializes a new KaggleData instance from the given map and id, and
   *  normalizes each value with the given map of means and standard deviations
   */
  private def initNormalized(id : String, l : KaggleLabel.Value, vals : Map[Int, Double], 
      means : Map[Int, Double], stDev : Map[Int, Double]) : KaggleData = {
     new KaggleData(id, l, vals.map(a => (a._1, (a._2 - means(a._1))/stDev(a._1) )))
  }
}

/** Possible labelings for a KaggleData - multi class, not multi label */
object KaggleLabel extends Data.Label{
  val FALSE, TRUE = Value
  
  /** Mapping of int to its corresponding KaggleLabel */
  private val intToLabelMap : Map[Int, KaggleLabel.Value] = Map(-1 -> FALSE, 1 -> TRUE)
  
  /** Mapping of label to its corresponding int */
  private val labelToIntMap : Map[KaggleLabel.Value, Int] = Map(FALSE -> -1, TRUE -> 1)
  
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
  
  /** Returns the value of a label as a boolean */
  def toBool(v : KaggleLabel.Value) : Boolean = {
    v.equals(TRUE)
  }
}

/** Unioned data type of data gotten from kaggle.
 *  All kaggle data should be able to return a string of itself as a row for svm-processing */
class KaggleData(val id : String, override val label : KaggleLabel.Value, override val vals : Map[Int, Double]) 
	extends Data[KaggleLabel.Value](label, vals) with Operable[KaggleData]{
  
  /** Return the dot product of this with KaggleData d */
  override def dot(d : KaggleData) : Double = {
    Data.dot(vals, d.vals)
  }
  
  /** Similarity of two KaggleDatas is based on their dot products */
  override def similarity(d : KaggleData) : Double = {
    dot(d)
  }
  
  /** Use eucilidan distance for dist from this to d */
  override def distance(d : KaggleData) : Double = {
    Data.distance(vals, d.vals)
  }
  
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
  
  /** Valid length for KaggleData is any length. Maybe change this? */
  override def lengthOk = (a : Int) => true
  
  /** Possible labelings of a Kaggle Data */
  override val labels = KaggleLabel.values.toList
  
  /** Returns a string representing this dat that shows the keys of the attributes as strings */
  def toDescriptiveString : String = {
    
    
    
    vals.toList.sortWith((a, b) => a._1 < b._1).foldLeft(if(KaggleLabel.toBool(label)) "Funded " else "Not Funded ")(
        (a, b) => a + KaggleData.indexName(b._1) + ":" + b._2 + " ") + " # " + id
  }
  
  /** Returns a string representing this data that can be put into SVM-Light */
  def toSVMString : String = {
    vals.toList.sortWith((a, b) => a._1 < b._1).foldLeft(KaggleLabel.toInt(label) + " ")(
        (a, b) => a + b._1 + ":" + b._2 + " ") + "#" + id
  }
}