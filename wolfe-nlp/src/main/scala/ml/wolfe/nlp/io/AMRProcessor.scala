//package ml.wolfe.nlp.io
//
///**
// * Created by narad on 12/1/14.
// */
//import edu.cmu.lti.nlp.amr.GraphDecoder
//import edu.cmu.lti.nlp.amr.ConceptInvoke
//import edu.cmu.lti.nlp.amr.ConceptInvoke.OptionMap
//import scala.io.Source
//import edu.cmu.lti.nlp.amr.Corpus
//import edu.cmu.lti.nlp.amr._
//
//import scala.collection.mutable.HashMap
//import java.text.SimpleDateFormat
//import java.io.{StringWriter, PrintWriter}
//import java.util.Date
//
//
//class AMRProcessor(args: Array[String]) {
//  val VERSION = "BLAH"
//
//  def parseOptions(hm: scala.collection.mutable.Map[Symbol, String], list: List[String]): OptionMap = {
//    val map = hm.asInstanceOf[OptionMap]
//    def isSwitch(s: String) = (s(0) == '-')
//    list match {
//      case Nil => map
//      case "--stage1-only" :: l => parseOptions(map + ('stage1Only -> "true"), l)
//      case "--stage1-oracle" :: l => parseOptions(map + ('stage1Oracle -> "true"), l)
//      case "--stage1-train" :: l => parseOptions(map + ('stage1Train -> "true"), l)
//      case "--stage1-eval" :: l => parseOptions(map + ('stage1Eval -> "true"), l)
//      case "--stage1-features" :: value :: l => parseOptions(map + ('stage1Features -> value), l)
//      case "--stage1-weights" :: value :: l => parseOptions(map + ('stage1Weights -> value), l)
//      case "--stage1-concept-table" :: v :: l => parseOptions(map + ('stage1ConceptTable -> v), l)
//      case "--stage2-decoder" :: value :: l => parseOptions(map + ('stage2Decoder -> value), l)
//      case "--stage2-train" :: l => parseOptions(map + ('stage2Train -> "true"), l)
//      case "--stage2-features" :: value :: l => parseOptions(map + ('stage2Features -> value), l)
//      case "--stage2-weights" :: value :: l => parseOptions(map + ('stage2Weights -> value), l)
//      case "--stage2-labelset" :: value :: l => parseOptions(map + ('stage2Labelset -> value), l)
//      case "--stage2-not-connected" :: l => parseOptions(map + ('stage2NotConnected -> "true"), l)
//      case "--training-loss" :: value :: l => parseOptions(map + ('trainingLoss -> value), l)
//      case "--training-cost-scale" :: value :: l => parseOptions(map + ('trainingCostScale -> value), l)
//      case "--training-prec-recall" :: value :: l => parseOptions(map + ('trainingPrecRecallTradeoff -> value), l)
//      case "--training-l2-strength" :: value :: l => parseOptions(map + ('trainingL2RegularizerStrength -> value), l)
//      case "--training-optimizer" :: value :: l => parseOptions(map + ('trainingOptimizer -> value), l)
//      case "--training-output" :: value :: l => parseOptions(map + ('trainingOutputFile -> value), l)
//      case "--training-stepsize" :: value :: l => parseOptions(map + ('trainingStepsize -> value), l)
//      case "--training-passes" :: value :: l => parseOptions(map + ('trainingPasses -> value), l)
//      case "--training-avg-weights" :: l => parseOptions(map + ('trainingAvgWeights -> "true"), l)
//      case "--training-save-interval" :: value :: l => parseOptions(map + ('trainingSaveInterval -> value), l)
//      case "--training-data" :: value :: tail => parseOptions(map + ('trainingData -> value), tail) // used to be "--amr-oracle-data"
//      case "--training-dev" :: value :: tail => parseOptions(map + ('trainingDev -> value), tail)
//      //case "--amr-oracle-data" :: value :: tail => parseOptions(map + ('amrOracleData -> value), tail)
//      case "--smatch-eval" :: value :: tail => parseOptions(map + ('smatchEval -> value), tail)
//      case "--output-format" :: value :: l => parseOptions(map + ('outputFormat -> value), l)
//      case "--ignore-parser-errors" :: l => parseOptions(map + ('ignoreParserErrors -> "true"), l)
//      case "--print-stack-trace-on-errors" :: l => parseOptions(map + ('printStackTraceOnErrors -> "true"), l)
//      case "--dependencies" :: value :: tail => parseOptions(map + ('dependencies -> value), tail)
//      case "--ner" :: value :: tail => parseOptions(map + ('ner -> value), tail)
//      case "--snt" :: value :: tail => parseOptions(map ++ Map('notTokenized -> value), tail)
//      case "--tok" :: value :: tail => parseOptions(map ++ Map('tokenized -> value), tail)
//      case "-v" :: value :: tail => parseOptions(map ++ Map('verbosity -> value), tail)
//
//      //case string :: opt2 :: tail if isSwitch(opt2) => parseOptions(map ++ Map('infile -> string), list.tail)
//      //case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
//      case option :: tail => println("Error: Unknown option " + option)
//        sys.exit(1)
//    }
//  }
//
//  def time[A](a: => A) = {
//    val now = System.nanoTime
//    val result = a
//    val micros = (System.nanoTime - now) / 1000
//    System.err.println("Decoded in %,d microseconds".format(micros))
//    result
//  }
//
//  def build(sentences: Array[String]) {
//
//    //  type OptionMap = Map[Symbol, String]
//
//    val usage = ""
//
//    if (args.length == 0) { println(usage); sys.exit(1) }
//    val options = parseOptions(HashMap[Symbol, String](), args.toList)
//    println(options.mkString("\n"))
//
//    val verbosity = options.getOrElse('verbosity, "0").toInt
//
//    val outputFormat = options.getOrElse('outputFormat, "triples").split(",").toList
//    // Output format is comma separated list of: nodes,edges,AMR triples
//
//    println("Stage1 Decoder")
//    val stage1: ConceptInvoke.Decoder = {
//      if (!options.contains('stage1Oracle) && !options.contains('stage2Train)) {
//        ConceptInvoke.Decoder(options, oracle = false)
//      } else {
//        assert(!options.contains('stage1Train), "Error: --stage1-oracle should not be specified with --stage1-train")
//        ConceptInvoke.Decoder(options, oracle = true)
//      }
//    }
//
//    println("Stage2 Decoder")
//    val stage2: Option[GraphDecoder.Decoder] = {
//      if ((options.contains('stage1Only) || options.contains('stage1Train)) && !options.contains('stage2Train)) {
//        None
//      } else {
//        Some(GraphDecoder.Decoder(options))
//      }
//    }
//
//    println("Stage2Oracle")
//    val stage2Oracle: Option[GraphDecoder.Decoder] = {
//      if (options.contains('trainingData) || options.contains('stage2Train)) {
//        Some(GraphDecoder.Oracle(options))
//      } else {
//        None
//      }
//    }
//
//    println("Stage1 train")
//    if (options.contains('stage1Train) || options.contains('stage2Train)) {
//
//      ////////////////// Training  ////////////////
//
//      if (options.contains('stage1Train) && options.contains('stage2Train)) {
//        System.err.println("Error: please specify either stage1 training or stage2 training (not both)")
//        sys.exit(1)
//      }
//
//      if (options.contains('stage1Train)) {
//
//        val stage1 = new ConceptInvoke.TrainObj(options)
//        stage1.train
//
//      }
//
//      if (options.contains('stage2Train)) {
//        println("Stage2 Train)")
//
//        val stage2 = new GraphDecoder.TrainObj(options)
//        stage2.train
//
//      }
//
//    } else {
//
//      /////////////////// Decoding /////////////////
//
//      println("Decoding...")
//      if (!options.contains('stage1Weights)) {
//        System.err.println("Error: No stage1 weights file specified");
//        sys.exit(1)
//      }
//      stage1.features.weights.read(Source.fromFile(options('stage1Weights).asInstanceOf[String]).getLines())
//
//      //logger(0, "Stage1 weights:\n"+stage1.features.weights.toString)
//
//      if (!options.contains('stage2Weights)) {
//        System.err.println("Error: No stage2 weights file specified")
//        sys.exit(1)
//      }
//      val stage2weightfile: String = options('stage2Weights)
//
//      //     logger(0, "Reading weights")
//      if (stage2 != None) {
//        stage2.get.features.weights.read(Source.fromFile(stage2weightfile).getLines())
//        if (stage2Oracle != None) {
//          stage2Oracle.get.features.weights.read(Source.fromFile(stage2weightfile).getLines())
//        }
//      }
//      //      logger(0, "done")
//
//      val input = sentences // Source.stdin.getLines.toArray
//      println("Parsing %d sentences...".format(sentences.size))
//      val tokenized = Source.fromFile(options('tokenized).asInstanceOf[String]).getLines /*.map(x => x)*/ .toArray
//      val nerFile = Corpus.splitOnNewline(Source.fromFile(options('ner).asInstanceOf[String]).getLines).toArray
//      val oracleData: Array[String] = if (options.contains('trainingData)) {
//        Corpus.getAmrBlocks(Source.fromFile(options('trainingData)).getLines()).toArray
//      } else {
//        new Array(0)
//      }
//      val dependencies: Array[String] = if (options.contains('dependencies)) {
//        (for {
//          block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies).asInstanceOf[String]).getLines())
//        } yield block.replaceAllLiterally("-LRB-", "(").replaceAllLiterally("-RRB-", ")").replaceAllLiterally( """\/""", "/")).toArray
//      } else {
//        new Array(0)
//      }
//
//      case class F1(var correct: Double, var predicted: Double, var total: Double) {
//        def precision: Double = correct / predicted
//        def recall: Double = correct / total
//        def f1: Double = 2 * (precision * recall) / (precision + recall)
//        override def toString: String = { "Precision: " + precision.toString + "\nRecall: " + recall.toString + "\nF1: " + f1.toString }
//      }
//      val spanF1 = F1(0, 0, 0)
//
//      for ((block, i) <- input.zipWithIndex) {
//        try {
//          time {
//            val line = input(i)
//            //          logger(0, "Sentence: "+line+"\n")
//            val tok = tokenized(i)
//            val ner = nerFile(i)
//            val inputGraph = if (options.contains('stage1Oracle)) { Some(AMRTrainingData(oracleData(i)).toInputGraph) } else { None }
//            val stage1Result = stage1.decode(new Input(inputGraph,
//              tok.split(" "),
//              line.split(" "),
//              dependencies(i),
//              ner))
//            //         logger(1, "Concepts:")
//            for ((id, node) <- stage1Result.graph.getNodeById) {
//              //           logger(1, "id = "+id+" concept = "+node.concept)
//            }
//            //         logger(0, "Spans:")
//            for ((span, i) <- stage1Result.graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
//              //           logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
//            }
//            //         logger(0, "")
//
//            stage1Result.graph.normalizeInverseRelations
//            stage1Result.graph.addVariableToSpans
//
//            var decoderResultGraph = stage1Result.graph // TODO: in future just do decoderResult.graph instead (when BasicFeatureVector is removed from stage1)
//
//            // TODO: clean up this code
//
//            if (!options.contains('stage1Only)) {
//              val decoder = stage2.get
//              decoderResultGraph = decoder.decode(new Input(stage1Result.graph, // TODO: what about stage1Oracle
//                tok.split(" "),
//                dependencies(i))).graph
//            } //endif (!options.contains('stage1Only))
//
//            if (options.contains('trainingData)) {
//              val amrdata2 = AMRTrainingData(oracleData(i)) // 2nd copy for oracle
//              //       logger(1, "Node.spans:")
//              for (node <- amrdata2.graph.nodes) {
//                //         logger(1, node.concept+" "+node.spans.toList)
//              }
//
//              val oracle = stage2Oracle.get
//              val oracleResult = oracle.decode(new Input(amrdata2, dependencies(i), oracle = true))
//              for ((span, i) <- amrdata2.graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
//                //         logger(0, "Oracle Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
//              }
//              //        logger(0, "")
//              if (options.contains('stage1Eval)) {
//                for (span <- stage1Result.graph.spans) {
//                  //if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end /*&& x.amr.prettyString(detail = 0, pretty = false).replaceAll("""\([^ ]* :name ""","") == span.amr.prettyString(detail = 0, pretty = false).replaceAll("""\([^ ]* :name ""","")*/) > 0) {
//                  //if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.prettyString(detail = 0, pretty = false, vars = Set()).replaceAll("""\([^ ]* :name ""","") == span.amr.prettyString(detail = 0, pretty = false, vars = Set()).replaceAll("""\([^ ]* :name ""","")) > 0) {
//                  if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.toString == span.amr.toString) > 0) {
//                    spanF1.correct += 1
//                  } else {
//                    if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end) > 0) {
//                      //                 logger(0, "Incorrect span: "+span.words+" => "+span.amr)
//                    } else {
//                      //                 logger(0, "Extra span: "+span.words+" => "+span.amr)
//                    }
//                  }
//                }
//                for (span <- oracleResult.graph.spans) {
//                  if (stage1Result.graph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.toString == span.amr.toString) == 0) {
//                    //                 logger(0, "Missing span: "+span.words+" => "+span.amr)
//                  }
//                }
//                spanF1.predicted += stage1Result.graph.spans.size
//                spanF1.total += oracleResult.graph.spans.size
//              }
//              //           logger(0, "Dependencies:\n"+dependencies(i)+"\n")
//              //           logger(0, "Oracle:\n"+oracleResult.graph.printTriples(detail = 1, extra = (node1, node2, relation) => {
//
//              //              "" //TODO: put back in "\t"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+oracle.features.localScore(node1, node2, relation).toString
//              //              //"\n"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\nScore = "+decoder.features.localScore(node1, node2, relation).toString+"  Relevent weights:\n"+decoder.features.weights.slice(decoder.features.localFeatures(node1, node2, relation)).toString
//              //            })+"\n")
//            } //endif (options.contains('amrOracleData))
//
//            if (!options.contains('stage1Only)) {
//              val decoder = stage2.get
//              //           logger(0, decoder.features.input)
//              //           logger(0, "AMR:\n"+decoderResultGraph.printTriples(detail = 1, extra = (node1, node2, relation) => {
//
//              "" //TODO: put back in "\t"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
//              //"\n"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\nScore = "+decoder.features.localScore(node1, node2, relation).toString+"  Relevent weights:\n"+decoder.features.weights.slice(decoder.features.localFeatures(node1, node2, relation)).toString
//              //         })+"\n")
//            }
//
//            println("# ::snt " + line)
//            println("# ::tok " + tok)
//            val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
//            decoderResultGraph.assignOpN()
//            decoderResultGraph.sortRelations()
//            decoderResultGraph.makeIds()
//            println("# ::alignments " + decoderResultGraph.spans.map(_.format).mkString(" ") + " ::annotator " + VERSION + " ::date " + sdf.format(new Date))
//            if (outputFormat.contains("nodes")) {
//              println(decoderResultGraph.printNodes.map(x => "# ::node\t" + x).mkString("\n"))
//            }
//            if (outputFormat.contains("root")) {
//              println(decoderResultGraph.printRoot)
//            }
//            if (outputFormat.contains("edges") && decoderResultGraph.root.relations.size > 0) {
//              println(decoderResultGraph.printEdges.map(x => "# ::edge\t" + x).mkString("\n"))
//            }
//            if (outputFormat.contains("AMR")) {
//              println(decoderResultGraph.prettyString(detail = 1, pretty = true))
//            }
//            if (outputFormat.contains("triples")) {
//              println(decoderResultGraph.printTriples(detail = 1))
//            }
//            println()
//          } // time
//        } catch {
//          // try
//          case e: Throwable => if (options.contains('ignoreParserErrors)) {
//            println("# ::snt " + input(i))
//            println("# ::tok " + tokenized(i))
//            val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
//            println("# ::alignments 0-1|0 ::annotator " + VERSION + " ::date " + sdf.format(new Date))
//            println("# THERE WAS AN EXCEPTION IN THE PARSER.  Returning an empty graph.")
//            if (options.contains('printStackTraceOnErrors)) {
//              val sw = new StringWriter()
//              e.printStackTrace(new PrintWriter(sw))
//              println(sw.toString.split("\n").map(x => "# " + x).mkString("\n"))
//            }
//            //         logger(-1, " ********** THERE WAS AN EXCEPTION IN THE PARSER. *********")
//            if (verbosity >= -1) { e.printStackTrace }
//            //          logger(-1, "Continuing. To exit on errors, please run without --ignore-parser-errors")
//            println(Graph.empty.prettyString(detail = 1, pretty = true) + '\n')
//          } else {
//            throw e
//          }
//        }
//      } // main loop
//    }
//  }
//}
//
//      //  if (options.contains('stage1Eval)) {
//      //    logger(0, "--- Stage1 evaluation ---\n"+spanF1.toString)
//      //  }
//
//    class AMR {
//
//    }
//
//    class AMRNode {}
//
//    class AMRConceptNode {}
//
//    object AMRTest extends App {
//
//      val modelDir = "/Users/narad/Downloads/jamr-master/models/ACL2014_LDC2013E117/"
//      val stage1Table = modelDir + "conceptTable.train"
//      val stage1Weights = modelDir + "stage1-weights"
//      val stage2Weights = modelDir + "stage2-weights.iter5"
//      val tmpDir = "/Users/narad/Downloads/jamr-tmp/"
//      val depFile = tmpDir + "jamr-88945.snt.deps"
//      val nerFile = tmpDir + "jamr-88945.snt.IllinoisNER"
//      val tokFile = tmpDir + "jamr-88945.snt.tok"
//
//      val stage1Features = "bias,length,fromNERTagger,conceptGivenPhrase"
//      val stage2Decoder = "LR"
//      val stage2Features = "rootConcept,rootDependencyPathv1,bias,typeBias,self,fragHead,edgeCount,distance,logDistance,posPathv3,dependencyPathv4,conceptBigram"
//      val stage2Labelset = "/Users/narad/Downloads/jamr-master/resources/labelset-r3"
//      val outputFormat = "AMR,nodes,edges,root"
//      val extra = "--ignore-parser-errors --print-stack-trace-on-errors"
//
//
//      val opts = Map("--stage1-concept-table" -> stage1Table,
//        "--stage1-weights" -> stage1Weights,
//        "--stage2-weights" -> stage2Weights,
//        "--dependencies" -> depFile,
//        "--ner" -> nerFile,
//        "--tok" -> tokFile,
//        "--stage1-features" -> stage1Features,
//        "--stage2-decoder" -> stage2Decoder,
//        "--stage2-features" -> stage2Features,
//        "--stage2-labelset" -> stage2Labelset,
//        "--output-format" -> outputFormat,
//        "-v" -> "0")
//
//      val sentences = DataToParseable.read(args(0), args(1), args(2))
//
//      val pargs = opts.map(p => "%s %s".format(p._1, p._2)).mkString(" ").split(" ")
//      val proc = new AMRProcessor(pargs)
//      proc.build(sentences)
//
//    }
