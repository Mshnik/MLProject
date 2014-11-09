import scala.io._

/** Object to handle reading data from kaggle, assembling into Data objects */
object ReaderWriter {
  
  //Where to put the files when you want to use them. Otherwise store at ../data/*.csv
  val donationsFile = "data/donations.csv"
  val outcomesFile = "data/outcomes.csv"
  val projectsFile = "data/projects.csv"
  val resourcesFile = "data/resources.csv"
    
  val rawData = "data/raw/combined_" //Add # for which file
  val rawExtension = ".csv"
  val svmData = "data/svm_data/dat_" //Add # for which file
  val svmExtension = ".txt"
    
  val numbFiles = 10 //Number of data files, both raw and converted. data indexs should be [1 .. this]
  
  /** Alter to run whichever routine is necessary */
  def main(args : Array[String]) : Unit = {
    convert
  }
  
  /** Returns a string representing rawDataFile i */
  private def rawFile(i : Int) : String = {
    rawData + i + rawExtension
  }
  
  /** Returns a string representing svmData file i */
  private def svmFile(i : Int) : String = {
    svmData + i + svmExtension
  }
  
  /** Converts raw data in data/raw/combined_* to data/svm_data/dat_* as svm data */
  def convert() : Unit = {
    for(i <- 1 to numbFiles){
      println("Reading raw data " + i)
      val dat = readRaw(rawFile(i))
      println("Writing to svm data " + i)
      writeSVM(dat, svmFile(i))
    }
  }
  
  /** Checks the raw and converted data for equality after both are read into memory
   *  If everything checks out, does nothing 
   */
  @throws[RuntimeException]
  def checkEquality() : Unit = {
    for(i <- 1 to numbFiles){
      def s(a : KaggleData, b : KaggleData) : Boolean = a.id < b.id
      
      val dat1 = readRaw(rawFile(i)).sortWith(s)
      val dat2 = readSVMData(svmFile(i), Map("1" -> KaggleLabel.TRUE, "0" -> KaggleLabel.FALSE), 0).sortWith(s)
      
      def eq(a : Boolean, e : (KaggleData, KaggleData)) : Boolean = {
        a && e._1.id.equals(e._2.id) && e._1.label.equals(e._2.label) && e._1.vals.equals(e._2.vals)  
      }
      
      val ok = dat1.zip(dat2).foldLeft(true)(eq)
      
      if(! ok){
        throw new RuntimeException("Unequal lists after reading from memory")
      }
      println("File " + i + " ok")
    }
  }
  
  /** Reads from projects and outcomes files, then writes unioned data to new data files.
   *  Assumes projects and outcomes files are both sorted by their projectID field.
   *  Each file written to memory has n rows in it, plus the header row. Writes m files.
   *  
   *  Move project and outcome files into innner data folder to use this method.
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
      
      write(header :: lst.map(a => a._1 + a._2), rawData + (i+1) + rawExtension)
    }
  }  
  
  /** Reads the entries in the given file (in combine_*.csv format), 
   *  outputs as a list of KaggleData
   */
  def readRaw(fName : String) : List[KaggleData] = {
    val iterator = Source.fromFile(fName).getLines.drop(1) //Get iterator for data, drop header line
    
    def f(acc : List[KaggleData], e : String) : List[KaggleData] = {  
      
      //See http://stackoverflow.com/questions/2700953/a-regex-to-match-a-comma-that-isnt-surrounded-by-quotes
      val arr = e.split(",\\s*(?=([^\"]*\"[^\"]*\")*[^\"]*$)") 
      val id = arr(KaggleData.idIndex)
      val m : Map[Int, Double] = Map()
      
      val mp = arr.foldLeft((m, 0))((acc, d) => 
        (acc._1 + ((acc._2, KaggleData.convertValue(acc._2, d))), acc._2 + 1) )._1
    
      KaggleData.init(id, mp) :: acc
    }
    
    val lst : List[KaggleData] = List()
    iterator.foldLeft(lst)(f)
  }
  
  /** Reads an SVM Data file */
  def readSVMData(filePath : String, labelDictionary : Map[String, KaggleLabel.Value], skipLines : Int) : List[KaggleData] = {
    val lst : List[KaggleData] = List()
    
    def f(lst : List[KaggleData], line : String) : List[KaggleData] = {
      //Split line by spaces
      val entries = line.split("\\s").toList
        
      //The correct classification of this example
      val status : KaggleLabel.Value = labelDictionary.getOrElse(entries(0), KaggleLabel.NONE)
      
      def f(a : (Map[Int, Double], String), s : String) : (Map[Int,Double], String) = {
        val colIndex = s.indexOf(':')
        if(colIndex == -1){
          (a._1, s.substring(1)) //This is the id in a label, with the first character as a #
        }
        else{
          val i = s.substring(0, colIndex).toInt 
          val v = s.substring(colIndex+1).toDouble
          (a._1 + ((i,v)), a._2)
        }
      }
      val m : (Map[Int, Double], String) = (Map(), "")
      val (map, s) = entries.tail.foldLeft(m)(f)
      val a = new KaggleData(s, status, map - KaggleData.idIndex - KaggleData.labelIndex)
      a :: lst
    }
    Source.fromFile(filePath).getLines.drop(skipLines).foldLeft(lst)(f).reverse
  }
  
  /** Writes the given list of kaggleData as their svm representations to the given filepath */
  def writeSVM(elms : List[KaggleData], path : String) : Unit = {
    val s = elms.foldLeft("")((acc, e) => acc + e.toSVMString + "\n")
    val pw = new java.io.PrintWriter(new java.io.File(path))
    try pw.write(s) 
    finally pw.close()
  }
  
}