package ml.wolfe.ui

import org.sameersingh.htmlgen.RawHTML
import org.sameersingh.scalaplot.{XYSeries, XYChart, Chart}

/**
 * @author Sebastian Riedel
 */
object D3Plotter {

  type XY = (Double,Double)

  def lineplot(chart:XYChart) = {
    val id = "d3line" + Math.abs(chart.hashCode()).toString
    chart.data.serieses

    def seriesDataToJson(series:XYSeries) = {
      series.points.map(p => s"{x:${p._1}, y:${p._2}}").mkString("[",",","]")
    }

    def seriesToJson(series:XYSeries) =
      s"""
         |{
         |  values: ${seriesDataToJson(series)},
         |  key: "${series.name}",
         |  color: '#ff7f0e'
         |}
       """.stripMargin



    val data = chart.data.serieses.map(seriesToJson).mkString("[",",","]")

    val style =
      s"""
        |svg text {
        | fill: #ddd;
        |}
        |
        |.tick line {
        |    stroke: white;
        |    opacity: 0.1;
        |}
        |
        |#$id svg {
        |  height: 400px;
        |}
      """.stripMargin


    var labelIndex = 0
    def xlabel(in:String) = if (in == "") "X" else in
    def ylabel(in:String) = if (in == "") "Y" else in


    val html =
      s"""
        |<div id="$id">
        |    <svg></svg>
        |</div>
        |
        |<style>
        |  $style
        |</style>
        |<script>
        |
        |    /*These lines are all chart setup.  Pick and choose which chart features you want to utilize. */
        |  nv.addGraph(function() {
        |  var chart = nv.models.lineChart()
        |                .margin({left: 100})  //Adjust chart margins to give the x-axis some breathing room.
        |                .useInteractiveGuideline(true)  //We want nice looking tooltips and a guideline!
        |                .transitionDuration(350)  //how fast do you want the lines to transition?
        |                .showLegend(true)       //Show the legend, allowing users to turn on/off line series.
        |                .showYAxis(true)        //Show the y-axis
        |                .showXAxis(true)        //Show the x-axis
        |  ;
        |
        |  chart.xAxis     //Chart x-axis settings
        |      .axisLabel("${xlabel(chart.x.label)}")
        |      .tickFormat(d3.format(',r'));
        |
        |  chart.yAxis     //Chart y-axis settings
        |      .axisLabel("${ylabel(chart.y.label)}")
        |      .tickFormat(d3.format('.02f'));
        |
        |  /* Done setting the chart up? Time to render it!*/
        |  var myData = $data;   //You need data...
        |
        |  d3.select('#$id svg')    //Select the <svg> element you want to render the chart in.
        |      .datum(myData)         //Populate the <svg> element with chart data...
        |      .call(chart);          //Finally, render the chart!
        |
        |  //Update the chart when window resizes.
        |  nv.utils.windowResize(function() { chart.update() });
        |  return chart;
        |});
        |</script>
        |
      """.stripMargin
    println(html)
    RawHTML(html)
  }

  def main(args: Array[String]) {
    import org.sameersingh.scalaplot.Implicits._
    val x = 0.0 until 2.0 * math.Pi by 0.1
    val y = x map (_ => 0.5)
    val chart = plot(x ->(math.sin(_)))
    val chart2 = Seq(Y(y,"y"))
  }

}
