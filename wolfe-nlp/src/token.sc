import breeze.linalg.DenseVector

import scala.io.Source

val lines = Source.fromFile("/Users/sriedel/Downloads/vectors-2.tsv").getLines()
val vectors = lines.map(l => new DenseVector(l.split("\t").drop(2).map(_.toDouble))).toArray
println(vectors(0).dot(vectors(1)))

def project(a:DenseVector[Double],b:DenseVector[Double]) =
  (a dot b) / b.norm()

project(vectors(0),vectors(1))
project(vectors(1),vectors(0))
vectors(0).norm()
vectors(1).norm()

def sigmoid(x:Double) = 1.0 / (1 + math.exp(-x))

sigmoid((vectors(0) dot vectors(1)) * 0.1)

