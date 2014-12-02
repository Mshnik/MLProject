import java.awt.geom.Ellipse2D
import scala.collection.immutable.HashMap
import org.jfree.chart._
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.data.xy._
import scala.collection.JavaConversions._
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.axis.NumberAxis
import java.awt.geom.Rectangle2D

object Chartifier {

  def showChart(m: HashMap[String, List[(Double, Double)]], title: String, xAxis: String, yAxis: String, dotSize: Double) {
    val data = new XYSeriesCollection()
    for(key <- m.keySet){
      val s = new XYSeries(key)
      for (i <- m.get(key)) {
        i.foreach(a => s.add(new XYDataItem(a._1, a._2)))
      }
      data.addSeries(s)
    }
    val chart = ChartFactory.createScatterPlot(title, xAxis, yAxis, data)
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    val delta = dotSize / 2
    for (i <- 0 until plot.getSeriesCount) {
      plot.getRenderer.setSeriesShape(i, new Ellipse2D.Double(-delta, -delta, dotSize, dotSize))
    }
    val frame = new ChartFrame("", chart)
    frame.pack()
    frame.setVisible(true)
  }
  
    def showChart(m: HashMap[String, List[(Double, Double)]], l : (Double, (Double, Double), String), title: String, 
        xAxis: String, yAxis: String, dotSize: Double, xRange : (Double, Double), yRange : (Double, Double)) {
    val data = new XYSeriesCollection()
    for(key <- m.keySet){
      val s = new XYSeries(key)
      for (i <- m.get(key)) {
        i.foreach(a => s.add(new XYDataItem(a._1, a._2)))
      }
      data.addSeries(s)
    }
    val (b, w, st) = l
    val s = new XYSeries(st)
    var x = 0.0
    while(x < 1){
      s.add(new XYDataItem(x, -w._1/w._2 * x - b/w._2))
      x = x + 0.05
    }
    data.addSeries(s)
    
    val chart = ChartFactory.createScatterPlot(title, xAxis, yAxis, data)
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    val axis = (plot.getDomainAxis().asInstanceOf[NumberAxis], plot.getRangeAxis().asInstanceOf[NumberAxis])
    axis._1.setRange(xRange._1, xRange._2)
    axis._2.setRange(yRange._1, yRange._2)
    val renderer = plot.getRenderer().asInstanceOf[XYLineAndShapeRenderer]
        renderer.setSeriesLinesVisible(2, true)
        renderer.setSeriesShapesVisible(2, false)
        plot.setRenderer(renderer);
    val delta = dotSize / 2
    plot.getRenderer.setSeriesShape(0, new Ellipse2D.Double(-delta, -delta, dotSize, dotSize))
    plot.getRenderer.setSeriesShape(1, new Rectangle2D.Double(-delta, -delta, dotSize, dotSize))
    
    val frame = new ChartFrame("", chart)
    frame.pack()
    frame.setVisible(true)
  }
}
