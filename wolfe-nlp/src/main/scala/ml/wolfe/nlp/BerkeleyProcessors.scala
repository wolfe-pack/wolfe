package ml.wolfe.nlp

import edu.berkeley.nlp.entity.EntitySystem

/**
 * Created by narad on 1/9/15.
 */
object BerkeleyProcessors {
  val testPath = ""
  val testSize = 10
  val modelPath = "/Users/narad/Downloads/models/corefner-onto.ser.gz"

//  public static enum Mode {
//    PREDICT, PREDICT_EVALUATE, TRAIN_EVALUATE,
//    PREDICT_ACE, PREDICT_EVALUATE_ACE, TRAIN_EVALUATE_ACE,
//    COREF_TRAIN, COREF_PREDICT, COREF_TRAIN_EVALUATE, COREF_TRAIN_PREDICT,
//    MAKE_MASK_MODELS;
//  }

  def annotate(text: String, coref: Boolean = true): Document = {
    EntitySystem.runOntoPredict(testPath, testSize, modelPath)
    ???
  }
}


object BerkeleyProcessorTest extends App {

  BerkeleyProcessors.annotate("Jim said he is going to the store.")
}

//    val mode =
//    if (mode == Mode.PREDICT) {
//      EntitySystem.runOntoPredict(testPath, testSize, modelPath);
//    } else if (mode == Mode.PREDICT_EVALUATE) {
//      EntitySystem.runOntoPredictEvaluate(testPath, testSize, modelPath);
//    } else if (mode == Mode.TRAIN_EVALUATE) {
//      EntitySystem.runTrainEvaluate(trainPath, trainSize, testPath, testSize);
//    } else if (mode == Mode.PREDICT_ACE) {
//      EntitySystem.runACEPredict(testPath, testSize, modelPath);
//    } else if (mode == Mode.PREDICT_EVALUATE_ACE) {
//      EntitySystem.runACEPredictEvaluate(testPath, testSize, modelPath);
//    } else if (mode == Mode.TRAIN_EVALUATE_ACE) {
//      EntitySystem.runTrainEvaluateACE(trainPath, trainSize, testPath, testSize);
//    } else if (mode == Mode.COREF_PREDICT) {
//      CorefSystem.runPredictWriteOutput(testPath, testSize, modelPath, outputPath, doConllPostprocessing);
//    } else if (mode == Mode.COREF_TRAIN_EVALUATE) {
//      CorefSystem.runTrainEvaluate(trainPath, trainSize, testPath, testSize, modelPath);
//    } else if (mode == Mode.COREF_TRAIN_PREDICT) {
//      CorefSystem.runTrainPredict(trainPath, trainSize, testPath, testSize, modelPath, outputPath, doConllPostprocessing);
//    } else if (mode == Mode.MAKE_MASK_MODELS) {
//      CorefPrunerJavaHack.trainAndSaveKFoldModels(trainPath, trainSize, maskNumFolds, maskOutPath);
//    } else {
//      throw new RuntimeException("Unknown mode: " + mode);
//    }
