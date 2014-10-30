
/** Object to handle reading data from kaggle, assembling into Data objects */
object ReaderWriter {
  
  val donationsFile = "data/donations.csv"
  val outcomesFile = "data/outcomes.csv"
  val projectsFile = "data/projects.csv"
  val resourcesFile = "data/resources.csv"
  val outputFile = "data/SVMData.txt"
  
  /** Reads from the .csv files given, writes to a new file of svm data */
  def readThenWriteToSVMData() : Unit = {
    val donationsMap = read(donationsFile)
    val outcomesMap = read(outcomesFile)
    val projectsMap = read(projectsFile)
    val resourcesMap = read(resourcesFile)
    
    val data = combine(donationsMap, outcomesMap, projectsMap, resourcesMap)
    write(data, outputFile)
  }
  
  /** Reads the entries in the given file, converts to a map of maps,
   *  where the outer map is id (String) -> ( index (int) -> val (double) )
   *  
   *  input converter function takes a string and converts it to a double
   *  in the case that a given value is not a numeric type.
   *  Conv should output the same input in the case that it is already a numeric type
   */
  def read(fName : String) : Map[String, Map[Int, Double]] = {
    val convert = KaggleData.convertValue(fName)_
    //TODO
    
    null
  }
  
  /** Combines the given data maps into a single list of KaggleData instances */
  def combine(donationsMap : Map[String, Map[Int, Double]],
		  	  outcomesMap : Map[String, Map[Int, Double]],
		  	  projectsMap : Map[String, Map[Int, Double]],
		  	  resourcesMap : Map[String, Map[Int, Double]]) : List[KaggleData] = {
    //TODO - use KaggleData.init after identifying corresponding parts of maps
    null
  }
  
  /** Writes the given list of kaggleData to the given filepath, hopefully ending in .txt */
  def write(elms : List[KaggleData], path : String) : Unit = {
    val s = elms.foldLeft("")((acc, e) => acc + e.toSVMString + "\n")
    val pw = new java.io.PrintWriter(new java.io.File(path))
    try pw.write(s) 
    finally pw.close()
  }
  
}