import java.util.Arrays
import scala.collection.JavaConversions._
import scala.collection.immutable.HashMap
import scala.io.Source

/** Implementing classes are able to perform operations with other instances of their same class */
trait Operable[D] {
  /** Returns the dot product of this and the given other data as vectors.*/
  def dot(d : D) : Double
  /** Returns the euclidean distance from this to other */
  def distance(d : D) : Double
  /** Returns a double corresponding to how similiar this is to other. Higher numbers -> more similar */
  def similarity(d : D) : Double
}

/** Implementing classes can be labeled */
trait Labelable[T <: Data.Label#Value]{
  /** Returns the label of this thing */
  val label : T
  /** Returns the possible labelings for this Labelable thing */
  def labels(): List[T]
}

/** Abstract un-generified data class - used so all data classes can access vector without generic
 *  param. of vals is index -> value */
abstract class AbsData(val vals: Map[Int, Double]){
  
  /** Simple toString that returns the vector as a string */
  override def toString() : String = {
    val s = vals.foldLeft("")((a, b) => a + b._1.toString + ":" + b._2.toString + ", ")
    "<" + s.substring(0, s.length - 2) + ">"
  }
  
  /** Returns the magnitude of this data as a vector */
  def magnitude(): Double = Data.magnitude(vals)
}

/** Companion object for Data class hosting helper and static methods */
object Data {
  
  /** Enum type all labels should extend */
  class Label extends Enumeration{
	  val NONE = Value
  }
  /** Singleton instance for accessing NONE */
  object Label extends Label{}

  /** Returns an array of the counts of the occurences of each label in the given list of data.
   *  Map is of label -> # occurances */
  def labelCount[T <: Label#Value](elms : List[Labelable[T]]) : Map[T, Int] = {
    def f(acc : HashMap[T, Int], e : Labelable[T]) : HashMap[T, Int] = {
      if(acc.contains(e.label)){
        val v = acc(e.label)
        acc - e.label + ((e.label, v + 1))
      } else{
        acc + ((e.label, 1))
      }
    }
    elms.foldLeft(new HashMap[T, Int]())(f)
  }
  
  /** Returns a map of lists, each list is similarly labeled data. Map is label -> list of data */
  def splitByLabel[T <: Label#Value](elms : List[Labelable[T]]) : Map[T, List[Labelable[T]]] = {
    def f(acc : HashMap[T, List[Labelable[T]]], e : Labelable[T]) : HashMap[T, List[Labelable[T]]] = {
      if(acc.contains(e.label)){
        val l = acc(e.label)
        acc - e.label + ((e.label, (e :: l)))
      } else{
        acc + ((e.label, List(e)))
      }
    }
    elms.foldLeft(new HashMap[T, List[Labelable[T]]]())(f) 
  }
  
  /** Returns the length of the vector represented by the given array */
  def magnitude(vec: Map[Int, Double]): Double = {
    Math.sqrt(vec.foldLeft(0.0)( (acc, v) => acc + Math.pow(v._2, 2)))
  }
  
  /** Returns the highest magnitude among the data passed in.
   *  Returns -1 if the list is empty/null */
  def maxMagnitude[D <: AbsData](dat : List[D]) : Double = {
    def f(a : Double, b : D) : Double = {
      Math.max(a, magnitude(b.vals))
    }
    if(dat == null) -1.0
    else dat.foldLeft(-1.0)(f)
  }

  /** Returns the dot product of the two given vectors represented by maps of index -> value */
  def dot(v1: Map[Int, Double], v2: Map[Int, Double]): Double = {
    v1.foldLeft(0.0)((acc, t) => acc + t._2 * v2.getOrElse(t._1, 0.0))
  }
  
  /** Returns the euclidean distance between the two given vectors, represented as maps of index -> value */
  def distance(v1 : Map[Int, Double], v2 : Map[Int, Double]) : Double = {
    Math.sqrt(v1.foldLeft(0.0)((acc, t) => acc + Math.pow(t._2 - v2.applyOrElse(t._1, ( (a : Int) => 0.0)), 2)))
  }

  /** Returns a vector that represents the mean of each field of the list of data provided.
   *  Output mapping is of index -> mean value */
  def mean[D <: AbsData](data: List[D]): Map[Int, Double] = {
    val count : Double = data.length
    data.foldLeft(new HashMap[Int, Double])((map, d) =>
      d.vals.foldLeft(map)((map, t) => map + ((t._1, t._2/count + map.getOrElse(t._1, 0.0))))
    )
  }
  
  /** Returns a vector that represents the standard deviation of each field of the list of data provided.
   *  Output mapping is of index -> standard deviation value*/
  def stDev[D <: AbsData](data : List[D]) : Map[Int, Double] = stDev(data, mean(data))
  
  /** Returns a vector that represents the standard deviation of each field of the list of data provided.
   *  Output has length equal to data.expectedSize() (one value per field in each input data).
   *  Takes the mean of the dataset as an input - prevents redundant calculations of mean */
  private def stDev[D <: AbsData](data: List[D], means : Map[Int, Double]): Map[Int, Double] = {
    val count : Double = data.length
    data.foldLeft(new HashMap[Int, Double])((map, d) =>
      d.vals.foldLeft(map)((map, t) => map + ((t._1, Math.pow(t._2 - means(t._1), 2) / count + map.getOrElse(t._1, 0.0)) ))
    )
  }
  
  /** Read and return a list of the correct subclass of data from the given file, in SVMLight data input format.
   *  Parameters are the path to the data file, a map of string to correct labeling,
   *  and a dummy instance of the correct class (should have the default label - NONE as label though) for use in construction.
   *  Skips the first skipLines in reading */
  def readSVMData[T <: Label#Value, E <: Data[T]](filePath : String, labelDictionary : HashMap[String, T], e : E, skipLines : Int) : List[E] = {
    val lst : List[E] = List()
    
    def f(lst : List[E], line : String) : List[E] = {
      //Split line by spaces
      val entries = line.split("\\s").toList
        
      //The correct classification of this example
      val status : T = labelDictionary.getOrElse(entries(0), e.label)
      
      def f(a : HashMap[Int, Double], s : String) : HashMap[Int,Double] = {
        val colIndex = s.indexOf(':')
        val i = s.substring(0, colIndex).toInt 
        val v = s.substring(colIndex+1).toDouble
        a + ((i,v))
      }
        
      val map = entries.tail.foldLeft(new HashMap[Int, Double]())(f)
      val a = e.getClass().getConstructors().head.newInstance(status, map).asInstanceOf[E]
      a :: lst
    }
    Source.fromFile(filePath).getLines.drop(skipLines).foldLeft(lst)(f).reverse
  }
  
}

/** Abstract data class represents a data value with a given vector length.
 *  Subclasses of Data need to specify what enum is used to label them (a subclass of Label)
 *  And various methods such as how many fields are in the given data class.
 *  Subclasses should implement the Operable trait with their own type as the generic.
 */
abstract class Data[T <: Data.Label#Value](val label: T, override val vals: Map[Int, Double]) extends AbsData(vals) with Labelable[T] {
  
  if (! lengthOk(vals.size)) throw new RuntimeException(vals + " has incorrect length")

  /** Simple toString() implementation that shows this' vector and its label */
  override def toString(): String = {
    if (label != null) 
      super.toString + " : " + label.toString 
    else 
      super.toString +" : NO_CLASS"
  }

  /** A function that determines if the number of mappings in the input vals is valid. */
  def lengthOk: Int => Boolean
}