package classifier

import data._
import data.Data._
import io._
import java.io.PrintStream
import java.io.File
import scala.util.Random

/** Holder for decision tree algorithms */
object DTNode{
  val m = KaggleLabel.stringToLabelMap
  var trainList : List[KaggleData] = List()
  var validationList  : List[KaggleData] = List()
  var testList  : List[KaggleData] = List()
  
  implicit val labelMap = ReaderWriter.labelDictionary
  implicit val skipLines = ReaderWriter.skipLines
  
  /** Do runnings of the id3 algorithm here. */
  def main(args : Array[String]) : Unit = {
    tryForest()
  }
  
  /** A test of the random forest algorithm */
  def tryForest() = {
    val trainFil = 1
    val validateFil = 2
    val testFil = 3
//    val argsList = (for(n <- 3 to 11 by 2; p <- 2 to 5; d <- 1 to 10) 
//      yield ((n, 1.0/p.toDouble, d, (1.0,1.0), attributeSplits))).toList
    bigTest()
    
//    val out = "data/DT_out/forest_sizecomp.txt"
//    val o = System.out
//    
//    System.setOut(new PrintStream(new File(out)))
//    System.out.println("\nEqual-Equal Size of Forest Test")
//    trainList = ReaderWriter.readSVMData(ReaderWriter.svmFile(trainFil))
//    validationList =  ReaderWriter.readSVMData(ReaderWriter.svmFile(validateFil))
//	testList = ReaderWriter.readSVMData(ReaderWriter.svmFile(testFil))
//    for(n <- List(1, 3, 7, 13, 21, 51, 101, 201, 301, 401, 501)){
//	    o.println("Testing... " + n)
//	    val argsList = (for(p <- List(1, 2, 3); w <- List(1.0); 
//    						s <- List(1, 2, 3); d <- List(Integer.MAX_VALUE.toInt)) 
//	    			yield ((n, p.toDouble/n.toDouble, d, (1.0,w), 1.0/s.toDouble))).toList
//	    val classifier = AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
//	        trainList, validationList, testList, forestTrainer, argsList)
//    }
        
//    System.out.println("\nEqual-Equal")
//    o.println("Test 4")
//    trainList = ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(trainFil))
//    validationList =  ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(validateFil))
//    testList = ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(testFil))
//    AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
//        trainList, validationList, testList, forestTrainer, argsList)
  }
  
  def bigTest() : Unit = {
    val arg = (101,0.0297029702970297,2147483647,(1.0,1.0),1.0)
    val out = "data/DT_out/macnemar_101_manyex.txt"
    val o = System.out
    val trainFil = 1
    val validateFil = 2
    val testFils = List(3,4,5,6,7)
    System.setOut(new PrintStream(new File(out)))
    trainList = ReaderWriter.readSVMData(ReaderWriter.svmFile(trainFil))
    validationList =  ReaderWriter.readSVMData(ReaderWriter.svmFile(validateFil))
	testList = testFils.foldLeft(List[KaggleData]())((acc, a) => ReaderWriter.readSVMData(ReaderWriter.svmFile(a))::: acc)
	val classifier = AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
	        trainList, validationList, testList, forestTrainer, List(arg))
	System.out.println("Macnemar Test")
	System.out.println("True Value, Classified Value")
	for(e <- testList){
	  System.out.println(e.toClassifiedString(classifier))
	}
  }
  
  /** A routine to run for comparing split to equal */
  def compSplitEq() = {
    val out = "data/DT_out/comp.txt"
     
    val trainFil = 1
    val validateFil = 2
    val testFil = 3
    val argsList = (for(d <- 1 to 10; i <- List(0.6, 0.8, 1.0)) yield ((d, (1.0,i)))).toList
      
    val o = System.out  
    System.setOut(new PrintStream(new File(out)))
      
    System.out.println("\nSkewed-Skewed")
    o.println("Test 1")
    trainList = ReaderWriter.readSVMData(ReaderWriter.svmFile(trainFil))
    validationList =  ReaderWriter.readSVMData(ReaderWriter.svmFile(validateFil))
    testList = ReaderWriter.readSVMData(ReaderWriter.svmFile(testFil))
    AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
        trainList, validationList, testList, treeTrainer, argsList)
 
    System.out.println("\nSkewed-Equal")
    o.println("Test 2")
    trainList = ReaderWriter.readSVMData(ReaderWriter.svmFile(trainFil))
    validationList =  ReaderWriter.readSVMData(ReaderWriter.svmFile(validateFil))
    testList = ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(testFil))
    AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
        trainList, validationList, testList, treeTrainer, argsList)
    
    System.out.println("\nEqual-Skewed")
    o.println("Test 3")
    trainList = ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(trainFil))
    validationList =  ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(validateFil))
    testList = ReaderWriter.readSVMData(ReaderWriter.svmFile(testFil))
    AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
        trainList, validationList, testList, treeTrainer, argsList)
    
    System.out.println("\nEqual-Equal")
    o.println("Test 4")
    trainList = ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(trainFil))
    validationList =  ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(validateFil))
    testList = ReaderWriter.readSVMData(ReaderWriter.svm_FF_File(testFil))
    AbsClassifier.trainValidateTest(o)("Accuracy", AbsClassifier.accuracy, 
        trainList, validationList, testList, treeTrainer, argsList)
  }
  
  /** A trainer for creating decision trees */
  def treeTrainer(elms : List[KaggleData], arg : (Int, (Double, Double))) : DTNode = {
    id3(attributeSplits, combinedSplits, arg._1, arg._2)(elms, 0, null)
  }
  
  /** A trainer for creating random forests.
   *  Arg : (num of trees, 
   *  		percent of pick for each tree, 
   *    	maxDepth for each tree, 
   *     	betas for each tree,
   *     	percent of valid splits) */
  def forestTrainer(elms : List[KaggleData], arg : (Int, Double, Int, (Double, Double), Double)) : Forest = {
    // Creates tree #i
    def tree(i : Int) : DTNode = {
      val elmsSubset = Random.shuffle(elms).take((elms.length * arg._2).toInt)
      val splitsSubset = Random.shuffle(attributeSplits).take((elms.length * arg._5).toInt)
      id3(splitsSubset, combinedSplits, arg._3, arg._4)(elmsSubset, 0, null)
    } 
    
    val trees = (for(i <- 0 until arg._1) yield tree(i)).toList
    
    new Forest(trees, "" + arg)
  }
  
  /** List of attributes that can be split on */
  val attributeSplits : List[Int] =
                                    KaggleData.enumIndices ++
                                    KaggleData.dateIndices ++
                                    KaggleData.numericIndices ++
                                    KaggleData.booleanIndices.filterNot(a => a.equals(KaggleData.labelIndex))
  
  /** The map of splits for combined_*.csv data - possible things to split on, at different places*/
  val combinedSplits : Map[Int, List[Double]] = Map(
    4 -> doubleSplits(30, 40, 1),
    5 -> doubleSplits(-110, -76, 2),
    28 -> doubleSplits(0, 100, 2),
    29 -> doubleSplits(25, 1000, 25),
    31 -> doubleSplits(0, 1000, 50),
    32 -> doubleSplits(10, 200, 10),
    35 -> doubleSplits(2000, 6000, 100),
    36 -> doubleSplits(0,14,1),
    37 -> doubleSplits(1,11,1),
    49 -> (1.0::2.0::3.0::doubleSplits(4,20,2)),
    50 -> (1.0::2.0::3.0::4.0::5.0::doubleSplits(6,40,3))
  )
  
  
  /** Returns a list of doubles with the given min, max, inc */
  private def doubleSplits(min : Double, max : Double, inc : Double) : List[Double] = {
    (min to max by inc).toList
  }
  
  /** Returns a list of doubles [1.0 ... enumMap(i).length] */
  private def enumSplits(i : Int) : List[Double] = {
    (1 to KaggleData.enumMap(i).length).toList.map(a => a.toDouble)
  }
  
  /** The ID3 DT creation algorithm. Returns the node that represents the root of the tree.
   *  Restricts to the given depth. CurrentDepth should be 1 when first passed in.
   *  attributes - all attributes that can be split on. Non-numeric are split on possible values + NONE.
   *  splits - (int) different numeric attributes that can be split on, with list[Double] as the possible splitted values
   *  elms - elements to split
   *  maxDepth - maximum depth to allows a tree to go
   *  betas - weighted entropy weighting on (true portion, false portion)
   *  currentDepth - current depth of this tree. Increment on successive calls
   *  funToHere - the function that caused to get to this call. Null only in the initial call */
  def id3(attributes : List[Int], splits : Map[Int, List[Double]], maxDepth : Int, betas : (Double, Double))(
      elms : List[KaggleData], currentDepth : Int, funToHere : AttributeFunction) : DTNode = {
    //First base case - no elms. Returns null
    if (elms.isEmpty) return null
    
    val description = "Max Depth: " + maxDepth + ", betas: " + betas
    
    //Check for base case - if all elms share a classification, create and return leaf node
    val labels = List(KaggleLabel.FALSE, KaggleLabel.TRUE) //Possible (legal) labelings of elements in elms
    for(l <- labels)
      if (elms.forall(a =>a.label.equals(l)))
    	return new DTLeafNode(funToHere, l, elms, description)
    
    //Find most common label among elements - break ties with earlier occuring enum
    val labelCount = Data.labelCount(elms).toList
    var m = -1
    var k = -1
    for(i <- 0 until labelCount.length){
      if(m < labelCount(i)._2){
        m = labelCount(i)._2
        k = i
      }
    }
    if(currentDepth == maxDepth){ //Can't go any further - make the best leaf we can
      return new DTLeafNode(funToHere, labels(k), elms, description)
    }
    
    //Non-base case. Try to gain some information by splitting elms
    //Splitting criteria: attribute i (1..9) <= j (0 .. 9)
    //Keep track of ideal info gain thus far
    val currentEntropy = entropy(Data.splitByLabel(elms))
    var bestInfoGain : Double = 0
    var bestCriterias : List[AttributeFunction] = List()
    
    /**Tests the given attribute that is a double. Tries attribute i with split value j */
    def testDoubleAtt(i : Int, j : List[Double]) : (Double, List[AttributeFunction]) = {
      
      /** Tries splitting on the value k. If better, returns that, otherwise returns acc */
      def f(acc : (Double, List[AttributeFunction]), k : Double) : (Double, List[AttributeFunction]) = {
        val criteria = new AttributeFunction(i, (a : Double, b : List[Double]) => a <= b(0), List(k), 
            ("data(" + KaggleData.indexName(i) + ")<=" + k))
        val opCriteria = new AttributeFunction(criteria, "data(" + KaggleData.indexName(i) + ")>" + k)
        //Do the partition based on this criteria
        val lstOne = elms.filter(criteria.fun)
        val lstTwo = elms.filter(opCriteria.fun)
      
        //Calculate the information gain
        val entropyOne = weightedEntropy(betas._1, betas._2)(lstOne, elms.length)
        val entropyTwo = weightedEntropy(betas._1, betas._2)(lstTwo, elms.length)
        val infoGain = currentEntropy - (entropyOne + entropyTwo)
        
        //Pick this if better than acc, else acc.
        if((acc._1 < infoGain)) (infoGain, List(criteria, opCriteria)) else acc
      }
      
      //Fold over possible j values to find best
      val a : AttributeFunction = null
      j.foldLeft(-1.0, List(a, a))(f)
    }
    
    /** Tests splitting on the given boolean or enum attribute (at index i) */
    def testEnumAtt(i : Int) : (Double, List[AttributeFunction]) = {
      val m =
      (  if(KaggleData.enumIndices.contains(i)) 
           "Not Provided" :: KaggleData.enumMap(i) //Possible enum values, with Not provided (blank) as index 0.
         else 
           List("f", "Not Provided", "t")
      ).foldLeft((if(KaggleData.enumIndices.contains(i)) 0 else -1, List[(Int, String)]()))((acc, e) => (acc._1 + 1, (acc._1, e) :: acc._2))._2  
      
      /** Converts value z of enum/boolean i to a string */
      def valToStr(z : Int) : String = {
        if(KaggleData.enumIndices.contains(i)){
          if(z.equals(0)) "Not Provided" else KaggleData.enumMap(i)(z - 1)
        }else{
          if(z.equals(1)) "t" 
          else 
            if(z.equals(-1)) "f" 
            else "Not Provided"
        } 
      }
      
      //Map into a list of attribute functions
      val criterias = m.map(a => new AttributeFunction(i, 
        (x : Int) => x.equals(a._1), 
        "Data(" + KaggleData.indexName(i) + ") = " + valToStr(a._1)
      ))
      
      //Filter out criterias that are unused
      val z = criterias.map(a => elms.filter(a.fun)).zip(criterias).filter(a => a._1.length > 0)
      val x = (List[List[KaggleData]](), List[AttributeFunction]())
      val (lsts, nCriterias) = z.foldLeft(x)((acc, a) => (a._1 :: acc._1, a._2 :: acc._2))
      
      //Calculate information gain
      val entropies = lsts.foldLeft(0.0)((acc, a) => acc + weightedEntropy(betas._1, betas._2)(a, elms.length))
      val infoGain = currentEntropy - entropies
      (infoGain, nCriterias)
    }
    
    //For each index, for each splittable value (j in list at index i)
    for(i <- attributes){
      val (infoGain, criteria) = if(splits.contains(i)) testDoubleAtt(i, splits(i)) else testEnumAtt(i)
      if(infoGain > bestInfoGain){
        bestInfoGain = infoGain
        bestCriterias = criteria
      }
    }
    
    //If there is some best criteria to split on, split and recurse.
    //Otherwise, no possible fix - use an impure leaf
    if(! bestCriterias.isEmpty){
      //Recurse on the many?!?!?!? children
      val a : List[DTNode] = List()
      val children = bestCriterias.foldLeft(a)((acc, a) => id3(attributes, splits, maxDepth, betas)(elms.filter(a.fun), (currentDepth + 1), a) :: acc)
      
      //Create and return this attribute node for the given criteria
      val m = children.map(a => (a, a.f)).toMap
      return new DTAttributeNode(funToHere, m, labels(k), elms, description)
    } else{
      
      return new DTLeafNode(funToHere, labels(k), elms, description)
    }
  }
  
  /** Returns the proportional entropy by calculating the entropy and multiplying by the
   *  fraction of the set this set makes up.
   *  0 if the original set has size 0.
   */
  def weightedEntropy(beta1 : Double, beta2 : Double)(lst : List[Labelable[KaggleLabel.Value]], s : Int) : Double = {
    if(s.equals(0)) 0 else{
      val e = (lst.length.toDouble / s.toDouble) * entropy(Data.splitByLabel(lst))
      if(e != 0){
        val lbl = lst.head.label
        if(lbl.equals(KaggleLabel.TRUE)) e * beta1 else if(lbl.equals(KaggleLabel.FALSE)) e * beta2 else e
      } else 0
    }
  }
  
  /** Returns Log_2(a) */
  def log2(d : Double) : Double = {
    Math.log(d)/Math.log(2)
  }
  
  /** Returns the entorpy of the given list partition
   *  Allows input weights beta1 and beta2 which change the way the terms are added.
   *  Beta1 is how much the portion of TRUE terms is weighted, Beta2 is how much the portion of
   *  FALSE terms is weighted
   */
  def entropy(mp : Map[KaggleLabel.Value, List[Labelable[KaggleLabel.Value]]]) : Double = {
     val totalSize = mp.foldLeft(0.0)( (a : Double, b : (KaggleLabel.Value,List[Labelable[KaggleLabel.Value]])) => a + b._2.length)
    
    def f (acc : Double, b : (KaggleLabel.Value, List[Labelable[KaggleLabel.Value]])) = {
     val c = if(b._2.length == 0) 0 else
       (b._2.length.toDouble/totalSize)*log2((b._2.length.toDouble/totalSize))
     acc - c
    }
    
    mp.foldLeft(0.0)(f)
  }
}

/** Represents a forest of Decision trees */
class Forest(val trees : List[DTNode], override val description : String) extends AbsClassifier(description){
  
  /** Classifies elm by the majority of the trees in this forest */
  def classify(elm : KaggleData) : KaggleLabel.Value = {
    
    def clz(acc : Map[KaggleLabel.Value, Int], a : DTNode) : Map[KaggleLabel.Value, Int] = {
      val lbl = a.classify(elm)
      val other = lbl match{
        case KaggleLabel.TRUE => KaggleLabel.FALSE 
        case _ => KaggleLabel.TRUE
      }
      Map[KaggleLabel.Value, Int](lbl -> (acc.getOrElse(lbl, 0) + 1), other -> acc.getOrElse(other, 0))
    }
    
    val count = trees.foldLeft(Map(KaggleLabel.TRUE -> 0, KaggleLabel.FALSE -> 0))(clz)
    count.toList.maxBy(_._2)._1
  }
  
  override def toString : String = {
    ""
  }
}

/** Abstract node class that represents a Decision tree.
 *  May be asked to classify a given data to a label (KaggleLabel.Value),
 *  and may maintain a list of children of the node.
 *  
 *  Maintains a list of elements that are classified by the decision made by this node
 *  If in the middle of construction and this isn't a leaf, elms needs to be divided up by creation
 *  of children
 *  
 *  f is the function that got to here. Null only for the root node
 */
abstract class DTNode(val f : AttributeFunction, val elms : List[KaggleData], override val description : String) 
	extends AbsClassifier(description) {

  /** Classifies the given data using this node as the root of the tree.*/
  def classify (data : KaggleData) : KaggleLabel.Value
  
  /** Returns the node (of this' children) that does the classification for data */
  def classifyingNode( data : KaggleData) : DTNode
  
  /** Returns a hashmap of children of this node, mapping from child ->
   *  function that takes data and returns true if that data should be mapped
   *  to that child, false otherwise
   */
  def children () : Map[DTNode, AttributeFunction]
  
  /** Returns the size of the tree rooted at this */
  def size() : Int
  
  /** Returns the total number of leaves contained in the tree rooted at this */
  def leaves() : Int
  
  /** Returns the max depth of the tree rooted at this. Trees of size 1 have depth 1 */
  def depth() : Int
  
  /** toString method calls the recursive version of to string on this with depth 0 */
  override def toString() = toStringRec(0)
  
  /** toString implementation with the given maxDepth */
  def toStringLim(maxDepth : Int) = toStringRec(0)(maxDepth)
  
  /** Implicit max depth to allow toStrings to go as far as they need to go */
  implicit val maxDepth : Int = Integer.MAX_VALUE.toInt;
  
  /** Helper for toString implementations that keeps track of how far in depth this is */
  def toStringRec(i : Int)(implicit maxDepth : Int) : String
  
  /** Returns a concatination of i tabs */
  protected def tabs(i : Int) : String = {
      var s = ""
      for(i <- 0 until i){
        s = s + "--"
      }
      s
    }
  
  /** Returns the percent success in the tree rooted at this.
   *  Does this by trying to classify each element in elms and checking that against its true classification
   *  returns a double in [0,1] that is the percent predicted correctly
   */
  def labelingAccuracy() : Double = {
    var correctCount = 0.0
    for(e <- elms){
      if(e.label.equals(classify(e))) {
        correctCount = correctCount + 1
      }
    }
    correctCount / elms.size.toDouble
  }

}

/** An ending node in a Decision Tree - internally maintains a label
 *  and classifies all incomming data to that label.
 *  No children - returns an empty hashmap.
 */
class DTLeafNode(override val f : AttributeFunction, val label : KaggleLabel.Value, override val elms : List[KaggleData],
    override val description : String) extends DTNode(f, elms, description){
  
  val emptyMap : Map[DTNode, AttributeFunction] = Map()
  
  /** Classifies all data according to this' label. */
  override def classify (data : KaggleData) = label
  
  /** This classifies all data passed to it */
  override def classifyingNode(data : KaggleData) = this
  
  /** Leaf Nodes have no children - returns empty hashmap */
  override def children() = emptyMap
  
  /** Leaf Nodes have size 1 */
  override def size() = 1
  
  /** Leaf Nodes contain 1 leaf */
  override def leaves() = 1
  
  /** Leaf Nodes have depth 1 */
  override def depth() = 1
  
  /** Still no recursion to do, just return s */
  override def toStringRec(i : Int)(implicit maxDepth : Int) : String = tabs(i) + label.toString()
  
}

/** A condition to test on - maintains a string representation of itself */
class AttributeFunction(f : (KaggleData => Boolean), s : String){
  
  /** Constructs a Double (real valued) attribute function from components.
   *  i - index of testing in data (0 indexed, as usual)
   *  f - comparison function, with actual value as first input and v as second value
   *  v - the value to enter as the second through nth args
   *  s - a string representation of this function 
   *   */
  def this(i : Int, f : ((Double, List[Double]) => Boolean), v : List[Double], s : String) ={
    this((d : KaggleData) => f(d.vals.getOrElse(i, 0.0), v), s)
  }
  /** Constructs a enum (discrete valued) attribute function from components.
   *  f - comparison function, in the form a = (val)
   *  s - a string representation of this function
   */
  def this(i : Int, f : (Int => Boolean), s : String) = {
     this((d : KaggleData) => f(d.vals.getOrElse(i, 0.0).toInt), s)
  }
  
  /** Creates opposite of given attributefunction */
  def this(a : AttributeFunction, s : String){
    this(a.fun.andThen(a => !a), s)
  }
  
  /** A reference to the function of this attributefunction */
  val fun = f
  
  /** Returns the string representation for the toString */
  override def toString : String = s
}

/** Non-terminal node in a Decision Tree - internally maintains a hashmap
 *  of children to functions.
 *  Classifies an incoming data by iterating through the hashmap and asking each
 *  child if its function returns true for the new data. If so, passes control of
 *  classification off to that child.
 *  
 *  Maintains a naive labeling that represents what this would classify as
 *  were the tree to end here.
 */
class DTAttributeNode(override val f : AttributeFunction, val m : Map[DTNode, AttributeFunction], 
    val label : KaggleLabel.Value, override val elms : List[KaggleData], override val description : String) 
    extends DTNode(f, elms, description){
  
  /** Passes control to child for classification. Iterates over all children
   *  and checks if any is matched with a function that accepts data.
   *  Note that if multiple such functions return true, one is chosen arbitrarily.
   *  If no function matches, throws a runtime exception.
   *  Thus in construction, make sure that the functions for each child both
   *  cover all cases and don't overlap.
   */
  override def classify(data : KaggleData) : KaggleLabel.Value = {
    for((node, fun) <- m){
      if(fun.fun(data)){
        return node.classify(data)
      }
    }
    //No child to apply data to, either because
    //The data slipped through or there are no children
    // -> classify naively, as if this were a leaf
    naiveClassify(data)
  }
  
  /** Returns the child of this that does the classification for the given data */
  override def classifyingNode(data : KaggleData) : DTNode = {
    for((node, fun) <- m){
      if(fun.fun(data)){
        return node
      }
    }
    //No child to apply data to, either because
    //The data slipped through or there are no children
    // -> classify naively, as if this were a leaf
    this
  }
  
  /** Classifies Naively, using this as a leaf */
  def naiveClassify(data : KaggleData) : KaggleLabel.Value = label
  
  /** Returns the hashmap of children -> acceptance function */
  override def children() : Map[DTNode, AttributeFunction]  = m
  
  /** The size of an attributeNodeTree is itself plus its children */
  override def size() = {
    m.foldLeft(1)( (acc : Int, b : (DTNode, AttributeFunction)) => acc + b._1.size)
  }
  
  /** Returns the total number of leaves rooted at this */
    /** The size of an attributeNodeTree is itself plus its children */
  override def leaves() = {
    m.foldLeft(0)( (acc : Int, b : (DTNode, AttributeFunction)) => acc + b._1.leaves)
  }
  
  /** The depth of an attributeNodeTree is itself plus the max of its children */
  override def depth() : Int = {
    1 + m.foldLeft(0)((acc : Int, b : (DTNode, AttributeFunction)) => Math.max(acc,b._1.depth))
  }
  
  /** Recursive toString that maintains tabing along the way */
  def toStringRec(i : Int)(implicit maxDepth : Int) : String = {
    if(i >= maxDepth) tabs(i) + "..." 
    else{
      var s = ""
      for((node, fun) <- m){
        s = s + tabs(i) + (fun.toString + "\n" + node.toStringRec(i+1) + "\n")
      }
      s.substring(0, s.length() - 1)
    }
  }
  
}