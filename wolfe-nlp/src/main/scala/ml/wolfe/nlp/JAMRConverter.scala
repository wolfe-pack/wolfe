//package ml.wolfe.nlp
//
//import java.io.FileWriter
//
///**
// * Created by narad on 11/25/14.
// */
//object JAMRConverter {
//
////  val model1 = ""
////  val stage1Weights = ""
////  val stage2Weights = ""
////  val dependencies = ""
////  val ner = ""
////  val tok = ""
////  val
////
////  ${JAMR_HOME}/run AMRParser \
////  --stage1-concept-table "${MODEL_DIR}/conceptTable.train" \
////  --stage1-weights "${STAGE1_WEIGHTS}" \
////  --stage2-weights "${STAGE2_WEIGHTS}" \
////  --dependencies "${INPUT}.deps" \
////  --ner "${INPUT}.IllinoisNER" \
////  --tok "${INPUT}.tok" \
////  -v 0 \
////  ${PARSER_OPTIONS} \
////  < "${INPUT}"
//
//
////  val options = AMRParser.parseOptions()
//
//}
//
//import edu.illinois.cs.cogcomp.LbjNer.LbjTagger.NerTagger
//
//object UIUCConverter extends App {
//
//  val text = ""
//  val doc = SISTAProcessors.mkParsedDocument(text)
//
//  val tout = new FileWriter("input.toks")
//  tout.write(doc.sentences.map(_.tokens.map(_.word)).mkString("\n"))
//  tout.close()
//
//  val dout = new FileWriter("input.deps")
//  dout.write(doc.sentences.map(_.syntax.dependencies).mkString("\n"))
//  dout.close()
//
//  NerTagger.main(Array("-annotate", "input.toks"))
//
//}
//
//
