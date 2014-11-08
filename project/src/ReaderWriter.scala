import scala.io._

/** Object to handle reading data from kaggle, assembling into Data objects */
object ReaderWriter {
  
  val donationsFile = "data/donations.csv"
  val outcomesFile = "data/outcomes.csv"
  val projectsFile = "data/projects.csv"
  val resourcesFile = "data/resources.csv"
  val combineFilePref = "data/combined_"
    
  val outputFile = "data/SVMData.txt"
  
  /** Alter to run whichever routine is necessary */
  def main(args : Array[String]) : Unit = {
    combineAndSplit(10000, 10)
  }
  
  /** Reads from projects and outcomes files, then writes unioned data to new data files.
   *  Assumes projects and outcomes files are both sorted by their projectID field.
   *  Each file written to memory has n rows in it, plus the header row. Writes m files
   */  
  def combineAndSplit(n : Int, m : Int) : Unit = {
    val projectsIterator = Source.fromFile(projectsFile).getLines
    val outcomesIterator = Source.fromFile(outcomesFile).getLines

    val projectsHeader = projectsIterator.next
    val outcomesHeader = outcomesIterator.next
    
    //Unioned header that doesn't repeat the projectID field
    val header = projectsHeader + "," + outcomesHeader.substring(outcomesHeader.indexOf(',') + 1)
    
    /** Writes the given list of Strings to the given filepath */
    def write(elms : List[String], path : String) : Unit = {
      val s = elms.foldLeft("")((acc, e) => acc + e + "\n")
      val pw = new java.io.PrintWriter(new java.io.File(path))
      try pw.write(s) 
      finally pw.close()
    }
    
    for(i <- (0 until m)){
      println("Creating file " + (i+1))
      var lst : List[(String, String)] = List()
      while(lst.size < n){
        
        //Loop until we find a outcome and project with the same projectID.
        //Assumes outcomes are a subset of projects.
        
        val outcome = outcomesIterator.next
        var project = ""
        do{
          project = projectsIterator.next
        }while(! outcome.substring(0, outcome.indexOf(',')).equals(project.substring(0, project.indexOf(','))))
        
        val outcomeWithoutID = outcome.substring(outcome.indexOf(',') + 1)  
          
        lst = (project, "," + outcomeWithoutID) :: lst  
      }
      
      write(header :: lst.map(a => a._1 + a._2), combineFilePref + (i+1) + ".csv")
    }
  }  
    
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