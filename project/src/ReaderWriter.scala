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
    for(e <- read("data/combined_1.csv")){
      println(e)
    }
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
  
  /** Reads the entries in the given file (in combine_*.csv format), 
   *  outputs as a list of KaggleData
   */
  def read(fName : String) : List[KaggleData] = {
    val iterator = Source.fromFile(fName).getLines.drop(1) //Get iterator for data, drop header line
    
    def f(acc : List[KaggleData], e : String) : List[KaggleData] = {
      val arr = e.split(",")
      val id = arr(KaggleData.idIndex)
      val m : Map[Int, Double] = Map()
      val mp = arr.foldLeft((m, 0))((acc, d) => 
        (acc._1 + ((acc._2, KaggleData.convertValue(acc._2, d))), acc._2 + 1) )._1
      
      KaggleData.init(id, mp) :: acc
    }
    
    val lst : List[KaggleData] = List()
    iterator.foldLeft(lst)(f)
  }
  
  /** Writes the given list of kaggleData to the given filepath, hopefully ending in .txt */
  def write(elms : List[KaggleData], path : String) : Unit = {
    val s = elms.foldLeft("")((acc, e) => acc + e.toSVMString + "\n")
    val pw = new java.io.PrintWriter(new java.io.File(path))
    try pw.write(s) 
    finally pw.close()
  }
  
}