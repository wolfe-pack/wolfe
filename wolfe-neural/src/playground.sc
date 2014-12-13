  import BIDMat.{SBMat, CMat, CSMat, DMat, Dict, IDict, FMat, GMat, GIMat, GSMat, HMat, IMat, Mat, SMat, SDMat}
  import BIDMat.MatFunctions._
  import BIDMat.SciFunctions._
  import BIDMat.Solvers._
  import BIDMat.Plotting._


  val a = rand(100,100)

  flip; val c = a*a; val ff=gflop
  print(ff)
