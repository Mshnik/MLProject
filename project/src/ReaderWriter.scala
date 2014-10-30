
/** Object to handle reading data from kaggle, assembling into Data objects */
object ReaderWriter {
  
  
  /** Reads all rows of the given file, converts to kaggle data objects, then to svm strings.
   *  May need to do this in chunks, because storing all in memory may be too intensive */
  def convertToSVM(fName : String) : Unit = {
    //TODO
  }
  
  /** Process a string (one row of the csv) into a 
   *  kaggledata object of the same type as instance (a dummy instance) */
  def process[T <: Data.Label#Value, D <: KaggleData[T]](s : String, instance : D) : D = {
    //TODO
    instance
  }
  
}