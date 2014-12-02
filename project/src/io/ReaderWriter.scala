package io

import scala.io._
import data.Data
import data.KaggleLabel
import data.KaggleData
import scala.util.Random

/** Object to handle reading data from kaggle, assembling into Data objects */
object ReaderWriter {
  
  //Where to put the files when you want to use them. Otherwise store at ../data/*.csv
  private val donationsFile = "data/donations.csv"
  private val outcomesFile = "data/outcomes.csv"
  private val projectsFile = "data/projects.csv"
  private val resourcesFile = "data/resources.csv"
    
  private val rawData = "data/raw/combined_" //Add # for which file
  private val csvData = "data/csv/dat_"
  private val rawExtension = ".csv"
  private val svmRawData = "data/svm_raw/dat_"
  private val svmScaledData = "data/svm_norm/dat_"  
  private val svmFiftyFifty = "data/svm_fiftyfifty/dat_"
  private val svmBinData = "data/svm_bin/dat_"
  private val svmFiftyFiftyBinData = "data/svm_fiftyfifty_bin/data_"
  private val svmExtension = ".txt"
    
  val numbFiles = 10 //Number of data files, both raw and converted. data indexs should be [1 .. this]
  
  /**dat_1.txt
dat_2.txt
dat_3.txt
dat_4.txt
dat_5.txt
dat_6.txt
dat_7.txt
dat_8.txt
dat_9.txt
dat_10.txtdat_1.txt
dat_2.txt
dat_3.txt
dat_4.txt
dat_5.txt
dat_6.txt
dat_7.txt
dat_8.txt
dat_9.txt
dat_10.txt Alter to run whichever routine is necessary */
  def main(args : Array[String]) : Unit = {
    CSVize(svmRawFile, csvFile)
  }
  
  /** Returns a string representing rawDataFile i */
  def rawFile(i : Int) : String = {
    rawData + i + rawExtension
  }
  
  /** Returns a string representing csv file i */
  def csvFile(i : Int) : String = {
    csvData + i + rawExtension
  }
  
  /** Returns a string representing svmRawData file i */
  def svmRawFile(i : Int) : String = {
    svmRawData + i + svmExtension
  }
  
    /** Returns a string representing svmFiftyFifty file i */
  def svmFiftyFiftyFile(i : Int) : String = {
    svmFiftyFifty + i + svmExtension
  }
  
  /** Returns a string representing svmScaled file i */
  def svmScaledFile(i : Int) : String = {
    svmScaledData + i + svmExtension
  }
  
  /** Returns a string representing svmBinData file i */
  def svmBinFile(i : Int) : String = {
    svmBinData + i + svmExtension
  }
  
  /** Returns a string representing svmFiftyFiftyBinData file i */
  def svmFiftyFiftyBinFile(i : Int) : String = {
    svmFiftyFiftyBinData + i + svmExtension
  }
  
  /** CSV-izes each of the data in the given path to the given output path */
  def CSVize(in: Int => String, out : Int => String) : Unit = {
    for(i <- 1 to numbFiles){
      println("Processing file " + i)
      val dat = readSVMData(in(i), KaggleLabel.stringToLabelMap, 0)
      println("Read data")
      writeCSV(dat, out(i))
      println("Wrote to file")
    }
  }
  
  /** Binerizes each of the data in the given in path (svm) to the given out path (svm) */
  def binerize(in : Int => String, out : Int => String) : Unit = {
    for(i <- 1 to numbFiles){
      println("Processing file " + i)
      val dat = readSVMData(in(i), KaggleLabel.stringToLabelMap, 0)
      println("Read data")
      val bined = dat.map(a => KaggleData.binerate(a))
      println("Binned Data")
      writeSVM(bined, out(i))
      println("Wrote to file")
    }
  }
  
  /** Shaves each svm data in data/svm/dat_* to data/svm_fiftyfifty/dat_* */
  def shave() : Unit = {
    for(i <- 1 to numbFiles){
      println("Processing file " + i)
      shaveFileToFile(svmRawFile(i), svmFiftyFiftyFile(i))
    }
  }
  
  /** Converts raw data in data/raw/combined_* to data/svm_raw/dat_* as svm data */
  def convert() : Unit = {
    for(i <- 1 to numbFiles){
      println("Reading raw data " + i)
      val dat = readRaw(rawFile(i))
      println("Writing to svm data " + i)
      writeSVM(dat, svmRawFile(i))
    }
  }
  
  /** Scales each of the raw data files in data/svm_raw/dat_* to data/svm_scaled/dat_* as scaled svm data */
  def scale() : Unit = {
    for(i <- 1 to numbFiles){
      println("Reading unscaled svm data " + i)
      val dat = readSVMData(svmRawFile(i), KaggleLabel.stringToLabelMap, 0)
      println("Scaling data")
      val sData = KaggleData.normalize(dat)
      println("Writing to scaled svm data " + i)
      writeSVM(sData, svmScaledFile(i))
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
      val dat2 = readSVMData(svmRawFile(i), KaggleLabel.stringToLabelMap, 0).sortWith(s)
      
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
  
  /** shave off examples such that there are an even number of positive and negative examples,
   *  then write to the given the svm to the given path
   */
  @throws[Exception]
  def shaveFileToFile(inPath : String, outPath : String) : Unit = {
    val data = readSVMData(inPath, KaggleLabel.stringToLabelMap, 0)
    val splitData = Data.splitByLabel(data)
    val plusData = Random.shuffle(splitData(KaggleLabel.TRUE))
    val minData = Random.shuffle(splitData(KaggleLabel.FALSE))
    
    val plusShaved = plusData.drop(Math.max(0, plusData.length - minData.length))
    val minShaved = minData.drop(Math.max(0, minData.length - plusData.length))
    if(plusShaved.length != minShaved.length){
      throw new Exception("Shaved lists not of same length: " + plusShaved.length + ", " + minShaved.length)
    }
    val shaved = Random.shuffle(plusShaved ::: minShaved)
    writeSVM(shaved.map(a => a.asInstanceOf[KaggleData]), outPath)
  }
  
  
  /** Writes the given list of kaggleData as their svm representations to the given filepath */
  def writeSVM(elms : List[KaggleData], path : String) : Unit = {
    val s = elms.foldLeft("")((acc, e) => acc + e.toSVMString + "\n")
    write(s, path)
  }
  
  /** Writes the given list of kaggleData as their csv representations, with the header */
  def writeCSV(elms : List[KaggleData], path :String) : Unit = {
    val s = elms.foldLeft(KaggleData.CSVHeaders + "\n")((acc, e) => acc + e.toCSVString + "\n")
    write(s, path)
  }
  
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try { op(p) } finally { p.close() }
  }
  
  /** Writes the given string to the given path */
  def write(s : String, path : String) : Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(path))
    try pw.write(s) 
    finally pw.close()
  }
  
}