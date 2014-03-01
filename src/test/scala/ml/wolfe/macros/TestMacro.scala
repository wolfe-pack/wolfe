package ml.wolfe.macros

import ml.wolfe.WolfeEnv

/**
  * @author Sebastian Riedel
  */
object TestMacro {
   def main(args: Array[String]) {
     import WolfeEnv._
     import OptimizedWolfe._
     case class Data(rain: Boolean)

     val allData = all2(Data)

     //val best = argmax(allData)(_ => true)(_ => 1.0)

     //println(best)


   }
 }
