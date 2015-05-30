package ml.wolfe.term

import java.util

import scala.collection.mutable

/**
 * @author riedel
 */
case class PTree(parse: Term[VarSeqDom[VarSeqDom[BooleanDom]]], slen: IntTerm) extends ComposedDoubleTerm {

  def copy(args: IndexedSeq[ArgumentType]) =
    PTree(args(0).asInstanceOf[Term[VarSeqDom[VarSeqDom[BooleanDom]]]],args(1).asInstanceOf[IntTerm])

  val arguments = IndexedSeq(parse, slen)

  type ArgumentType = AnyTerm

  val parseDom = parse.domain

  //todo: this accessor should come from the domain of parse

  final def edgeIndex(head: Int, mod: Int) = 1 + mod * parseDom.elementDom.lengths.discOff + head + 1

  final def edge(parse: Array[Int])(head: Int, mod: Int) = parse(edgeIndex(head, mod))

  def maxNumHeads(parse: Array[Int])(mod: Int) = parse(1 + mod * parseDom.elementDom.lengths.discOff)

  override def composer(args: Settings) = new Composer(args) {

    var heads: Array[Int] = null
    var visited: Array[Boolean] = null

    def eval()(implicit execution: Execution): Unit = {
      output.cont(0) = Double.NegativeInfinity
      val parse = input(0).disc.array
      val expectedLength = input(1).disc(0)

      //check whether all lengths are correct
      val actualLength = parse(0)
      if (actualLength != expectedLength) return
      for (mod <- 0 until actualLength) if (maxNumHeads(parse)(mod) != expectedLength) return

      //println("correct length")

      //instantiate head array
      if (heads == null || heads.length < actualLength) {
        heads = Array.ofDim[Int](actualLength)
        visited = Array.ofDim[Boolean](actualLength)
      }
      util.Arrays.fill(visited, false)
      util.Arrays.fill(heads, -1)
      //check whether exactly one token has no head, and all other tokens have exactly one head
      //and set up head pointers
      var tokensWithHeads = 0
      var roots = 0
      var root = -1
      for (mod <- 0 until actualLength) {
        heads(mod) = -1
        for (head <- 0 until actualLength) {
          if (edge(parse)(head, mod) == 1) {
            if (heads(mod) != -1) return //more than one head
            heads(mod) = head
            tokensWithHeads += 1
          }
        }
        if (heads(mod) == 0) roots += 1
        if (heads(mod) == -1) root = mod
      }
      if (roots != 1) return //multiple roots
      if (tokensWithHeads != actualLength - 1) return
      //println("right number of heads")

      if (root != 0) return //0 must be the root node


      //test whether there are loops
      for (mod <- 0 until actualLength; if !visited(mod)) {
        var current = mod
        val path = new mutable.HashSet[Int]
        do {
          if (path(current)) return //cycle
          visited(current) = true
          path += current
          current = heads(current)
        } while (current != -1)
      }
      //println("no loops")

      //test whether there are crossing edges
      //todo: quadratic right now, should be possible in linear fashion
      for (m1 <- 0 until actualLength; if m1 != root) {
        for (m2 <- 0 until actualLength; if m2 != root && m2 != m1) {
          val h1 = heads(m1)
          val h2 = heads(m2)

          val (l1, r1) = if (h1 < m1) (h1, m1) else (m1, h1)
          val (l2, r2) = if (h2 < m2) (h2, m2) else (m2, h2)

          if (l1 < l2 && r1 > l2 && r2 > r1 || l1 > l2 && l1 < r2 && r2 < r1) return //detected crossing edges
        }
      }
      //println(heads.mkString(" "))
      //println("no crossing edges")
      output.cont(0) = 0.0


    }
  }

  override def marginalizerImpl(wrt: Seq[AnyVar], observed: Seq[AnyVar])
                               (in: Settings, inMsgs: Msgs, reverseMsgsAlso: Boolean) = {

    try {
      require(reverseMsgsAlso, "need messages on output variables")

      val target = vars.filterNot(observed.contains)

      require(parse.isInstanceOf[VarSeqConstructor[Dom,VarSeqDom[Dom]]],
        "parse needs to be constructed sequence of bool sequence")

      val parseConstructor = parse.asInstanceOf[VarSeqConstructor[VarSeqDom[BoolDom],VarSeqDom[VarSeqDom[BoolDom]]]]

      val maxLength = parse.domain.maxLength

      require(parseConstructor.length.isInstanceOf[Var[Dom]],
        "component to indicate number of modifiers needs to be a variable")

      val numModsVar = parseConstructor.length.asInstanceOf[Var[IntDom]]

      for (m <- 0 until maxLength) require(parseConstructor.elements(m).isInstanceOf[VarSeqConstructor[Dom,VarSeqDom[Dom]]],
        "the sequence of booleans per modifier needs to be constructed")

      val nodesPerMod = for (m <- 0 until maxLength)
        yield parseConstructor.elements(m).asInstanceOf[VarSeqConstructor[BoolDom,VarSeqDom[BoolDom]]]


      for (m <- 0 until maxLength) require(parseConstructor.length.isInstanceOf[Var[Dom]],
        "the indicator of number of heads per modifier needs to be a variable")

      val numHeadsVarPerMod = for (m <- 0 until maxLength)
        yield parseConstructor.length.asInstanceOf[Var[IntDom]]

      for (m <- 0 until maxLength; h <- 0 until maxLength) require(nodesPerMod(m).elements(h).isInstanceOf[Var[BoolDom]],
        "each component representing an edge needs to be a variable")

      val nodes = for (m <- 0 until maxLength)
        yield for (h <- 0 until maxLength)
          yield nodesPerMod(m).elements(h).asInstanceOf[Var[BoolDom]]

      require(slen.vars forall observed.contains,
        "all variables within sentence length need to be observed")

      val wrtSet = wrt.toSet

      require(nodes.view.flatten.forall(wrtSet),
        "all edge variables need to be in the wrt set.")

      require(wrtSet(numModsVar),
        "variable for number of modifiers needs to be in the wrt set.")

      require(numHeadsVarPerMod.forall(wrtSet),
        "all number of head variables need to be in the wrt set.")


      val nodeIndices = for (m <- 0 until maxLength)
        yield for (h <- 0 until maxLength)
          yield wrt.indexOf(nodes(m)(h))

      val numModsIndex = wrt.indexOf(numModsVar)
      val numHeadsPerModIndices = for (m <- 0 until maxLength) yield wrt.indexOf(numHeadsVarPerMod(m))


      val lengthEval = slen.evaluatorImpl(in.linkedSettings(observed, slen.vars))

      //data structures for message calculation

      new Marginalizer {

        val input = in
        val outputMsgs = Msgs(target map (_.domain.createZeroMsg()))
        val inputMsgs = inMsgs

        //todo: currently we ignore (root,m) msgs

        def inMsgForEdge(h: Int, m: Int) = inputMsgs(nodeIndices(m)(h)).disc(0).msg

        def outMsgForEdge(h: Int, m: Int) = outputMsgs(nodeIndices(m)(h)).disc(0).msg

        def outLengthPerMod(m: Int) = outputMsgs(numHeadsPerModIndices(m)).disc(0).msg

        def outLength() = outputMsgs(numModsIndex).disc(0).msg

        def updateMessages()(implicit execution: Execution) = {
          //get sentence length
          lengthEval.eval()
          val slen = lengthEval.output.disc(0) - 1 // slen in this code is the number of actual tokens w/o ROOT

          //todo: do this only once
          val maxdim = slen + 1
          val worksize = maxdim * maxdim
          val tkmat = Array.ofDim[Double](worksize + maxdim)
          val gradmat = Array.ofDim[Double](worksize + maxdim)
          val heads = Array.ofDim[Int](slen)


          //clear memory from last run
          //util.Arrays.fill(tkmat, 0.0) //todo: only clear the necessary blocks
          util.Arrays.fill(heads, 0, slen, -1)

          //message calculation code from Jason
          for (dep <- 1 to slen) {
            val tkoffset = dep * slen
            tkmat(tkoffset + dep - 1) = 0 //??
            //      println("tk offset = " + tkoffset)
            var trues = 0
            var trueHead = -1
            for (head <- 0 to slen if dep != head) {
              //head == 0 means artificial root token
              //todo: currently we don't have messages for (root,m) nodes, just use zero?
              val mLog = inMsgForEdge(head, dep)
              val m = mLog map Math.exp
              //val m = indexedEdges(head, dep).msgs.asDiscrete.n2f.map(Math.exp(_))
              //        println("m(%d,%d) = [%s]".format(head, dep, m.mkString(", ")))
              //println("  - real space = [%s]".format(m.map(Math.exp(_)).mkString(", ")))
              if (m(0) == 0.0) {
                trues += 1
                trueHead = head
              }
              else {
                val score = m(1) / m(0)
                tkmat(head * slen + dep - 1) = score * -1.0 //negative log odds score for edge head->dep
                tkmat(tkoffset + dep - 1) += score //total score on modifier dep?
              }
            }
            if (trues == 1) {
              heads(dep - 1) = trueHead
              tkmat(tkoffset + dep - 1) = 1
              for (head <- 0 to slen if dep != head) {
                tkmat(head * slen + dep - 1) = if (head == trueHead) -1 else 0
              }
            }
            else if (trues > 1) {
              heads(dep - 1) = -2 //several deterministic heads
            }
            else {
              heads(dep - 1) = -1 //no deterministic heads
            }
          }
          // Calculate the potential's log partition function
          //    println("tkmat = [%s]".format(tkmat.mkString(", ")))
          //    println("gradmat = [%s]".format(gradmat.mkString(", ")))
          val z = sumTree(tkmat, gradmat, slen, multirooted = false)
          //    println(z)
          // Originally had a check here for Z != 0
          // Compute outgoing messages in terms of Z and incoming messages
          for (dep <- 1 to slen) {
            val koff = (dep - 1) * slen
            val tkoff = dep * slen
            for (head <- 0 to slen if dep != head) {
              var m = heads(dep - 1) match {
                case -2 => Array(Double.NaN, Double.NaN) // Incoming beliefs are a conflicting configuration
                case -1 => {
                  // No absolute belief was found pertaining to this head
                  val s = if (head > dep) 1 else 0
                  val n = gradmat(koff + head - s)
                  Array(1 + tkmat(head * slen + dep - 1) * n, n)
                }
                case _ => if (heads(dep - 1) == head) Array(0.0, 1.0) else Array(1.0, 0.0) // Set outgoing message deterministically
              }
              m = m map Math.log

              //indexedEdges(head, dep).msgs.asDiscrete.f2n(0) = m(0)
              //indexedEdges(head, dep).msgs.asDiscrete.f2n(1) = m(1)
              outMsgForEdge(head, dep)(0) = m(0)
              outMsgForEdge(head, dep)(1) = m(1)
            }
            //deterministic edges and length settings
            for (m <- 0 to slen) {
              outMsgForEdge(m, m)(0) = 0.0
              outMsgForEdge(m, m)(1) = Double.NegativeInfinity
              outMsgForEdge(m, 0)(0) = 0.0
              outMsgForEdge(m, 0)(1) = Double.NegativeInfinity
              util.Arrays.fill(outLengthPerMod(m), Double.NegativeInfinity)
              outLengthPerMod(m)(slen + 1) = 0.0
            }
            util.Arrays.fill(outLength, Double.NegativeInfinity)
            outLength(slen + 1) = 0.0

          }
        }


        def sumTree(tkmat: Array[Double], gradmat: Array[Double], slen: Int, multirooted: Boolean = false, verbose: Boolean = false): Double = {
          val sch = Array.ofDim[Double](slen + 1, slen + 1, 2, 2)
          val gch = Array.ofDim[Double](slen + 1, slen + 1, 2, 2)
          var res = 0.0
          val start = if (multirooted) 0 else 1
          //todo: adapt to variable length slen
          //for (i <- 0 until slen * slen) gradmat(i) = Double.NegativeInfinity
          util.Arrays.fill(gradmat, Double.NegativeInfinity) //todo: only clear the necessary blocks (based on slen)

          for (s <- 0 to slen; i <- 0 to 1; j <- 0 to 1) sch(s)(s)(i)(j) = 0.0
          for (width <- 1 to slen; s <- start to slen) {
            val t = s + width
            if (t <= slen) {
              for (i <- 0 to 1; j <- 0 to 1) sch(s)(t)(i)(j) = Double.NegativeInfinity
              if (s > 0) {
                val lkid = math.log(-1.0 * tkmat(t * slen + s - 1))
                for (r <- s until t) {
                  sch(s)(t)(0)(0) = logIncrement(sch(s)(t)(0)(0), sch(s)(r)(1)(1) + sch(r + 1)(t)(0)(1) + lkid)
                }
              }
              val rkid = Math.log(-1.0 * tkmat(s * slen + t - 1))
              for (r <- s until t) sch(s)(t)(1)(0) = logIncrement(sch(s)(t)(1)(0), sch(s)(r)(1)(1) + sch(r + 1)(t)(0)(1) + rkid)
              for (r <- s until t) sch(s)(t)(0)(1) = logIncrement(sch(s)(t)(0)(1), sch(s)(r)(0)(1) + sch(r)(t)(0)(0))
              for (r <- s + 1 to t) sch(s)(t)(1)(1) = logIncrement(sch(s)(t)(1)(1), sch(s)(r)(1)(0) + sch(r)(t)(1)(1))
            }
          }
          if (!multirooted) {
            sch(0)(slen)(1)(1) = Double.NegativeInfinity
            for (r <- 1 to slen) {
              //todo: what is the first row of tkmat?
              sch(0)(slen)(1)(1) = logIncrement(sch(0)(slen)(1)(1), sch(1)(r)(0)(1) + sch(r)(slen)(1)(1) + Math.log(-1.0 * tkmat(r - 1)))
            }
          }
          res = sch(0)(slen)(1)(1)
          for (s <- 0 to slen; t <- s to slen; i <- 0 to 1; j <- 0 to 1) {
            gch(s)(t)(i)(j) = Double.NegativeInfinity
          }
          gch(0)(slen)(1)(1) = -1.0 * res
          if (!multirooted) {
            for (r <- 1 to slen) {
              gch(1)(r)(0)(1) = logIncrement(gch(1)(r)(0)(1),
                -1.0 * res + sch(r)(slen)(1)(1) + math.log(-1.0 * tkmat(r - 1)))
              gch(r)(slen)(1)(1) = logIncrement(gch(r)(slen)(1)(1),
                -1.0 * res + sch(1)(r)(0)(1) + math.log(-1.0 * tkmat(r - 1)))
              gradmat((r - 1) * slen) = logIncrement(gradmat((r - 1) * slen),
                -1.0 * res + sch(1)(r)(0)(1) + sch(r)(slen)(1)(1))
            }
          }
          for (width <- slen to 1 by -1; s <- start to slen) {
            val t = s + width
            if (t <= slen) {
              var gpar = gch(s)(t)(1)(1)
              for (r <- s + 1 to t) {
                gch(s)(r)(1)(0) = logIncrement(gch(s)(r)(1)(0), gpar + sch(r)(t)(1)(1))
                gch(r)(t)(1)(1) = logIncrement(gch(r)(t)(1)(1), gpar + sch(s)(r)(1)(0))
              }
              gpar = gch(s)(t)(0)(1) // this seems to be s,r instead of s,t for some reason
              for (r <- s until t) {
                gch(s)(r)(0)(1) = logIncrement(gch(s)(r)(0)(1), gpar + sch(r)(t)(0)(0))
                gch(r)(t)(0)(0) = logIncrement(gch(r)(t)(0)(0), gpar + sch(s)(r)(0)(1))
              }
              if (s > 0) {
                var lgrad = Double.NegativeInfinity
                val lkid = scala.math.log(-1.0 * tkmat(t * slen + s - 1))
                gpar = gch(s)(t)(0)(0)
                for (r <- s until t) {
                  gch(s)(r)(1)(1) = logIncrement(gch(s)(r)(1)(1), gpar + sch(r + 1)(t)(0)(1) + lkid)
                  gch(r + 1)(t)(0)(1) = logIncrement(gch(r + 1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + lkid)
                  lgrad = logIncrement(lgrad, gpar + sch(s)(r)(1)(1) + sch(r + 1)(t)(0)(1))
                }
                gradmat((s - 1) * slen + t - 1) = logIncrement(gradmat((s - 1) * slen + t - 1), lgrad)
              }
              val rkid = Math.log(-1.0 * tkmat(s * slen + t - 1))
              var rgrad = Double.NegativeInfinity
              gpar = gch(s)(t)(1)(0)
              for (r <- s until t) {
                gch(s)(r)(1)(1) = logIncrement(gch(s)(r)(1)(1), gpar + sch(r + 1)(t)(0)(1) + rkid)
                gch(r + 1)(t)(0)(1) = logIncrement(gch(r + 1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + rkid)
                rgrad = logIncrement(rgrad, gpar + sch(s)(r)(1)(1) + sch(r + 1)(t)(0)(1))
              }
              gradmat((t - 1) * slen + s) = logIncrement(gradmat((t - 1) * slen + s), rgrad)
            }
          }
          //todo: adapt to variable length
          for (i <- 0 until slen * slen) gradmat(i) = Math.exp(gradmat(i))
          Math.abs(res)
        }

        def logIncrement(s: Double, x: Double): Double = {
          if (s == Double.NegativeInfinity) {
            x
          }
          else {
            val d = s - x
            if (d >= 0) {
              if (d <= 745) s + math.log(1.0 + math.exp(-1.0 * d)) else s
            }
            else if (d < -745) {
              x
            }
            else {
              x + math.log(1.0 + math.exp(d))
            }
          }
        }


      }
    } catch {
      case (e: IllegalArgumentException) =>
        logger.info(e.getMessage)
        super.marginalizerImpl(wrt, observed)(in, inMsgs, reverseMsgsAlso)
    }


  }
}

/*

 override def powMarginalF2N() = {           // Projective
    val slen = distinctHeads.length - 1 // indexedEdges.keys.map(_._1).max -- should indexedEdges simply be a Map[(Int,Int)]
                  // Or calculate as n as : n * (n-1) indexedEdges ?
    val maxdim =  slen + 1
    val worksize = maxdim * maxdim
    val tkmat    = Array.ofDim[Double](worksize + maxdim)
    val gradmat  = Array.ofDim[Double](worksize + maxdim)

    val heads = Array.fill(slen)(-1)

    // Collect incoming messages and look for absolute (entirely true or false) variable beliefs
    for (dep <- 1 to slen) {
      val tkoffset = dep * slen
      tkmat(tkoffset + dep - 1) = 0
//      println("tk offset = " + tkoffset)
      var trues = 0
      var trueHead = -1
      for (head <- 0 to slen if dep != head) {
        val m = indexedEdges(head, dep).msgs.asDiscrete.n2f.map(Math.exp(_))
//        println("m(%d,%d) = [%s]".format(head, dep, m.mkString(", ")))
        //println("  - real space = [%s]".format(m.map(Math.exp(_)).mkString(", ")))
        if (m(0) == 0) {
          trues += 1
          trueHead = head
        }
        else {
          val score = m(1) / m(0)
          tkmat(head * slen + dep - 1) = score * -1.0
          tkmat(tkoffset + dep - 1) += score
        }
      }
      if (trues == 1) {
        heads(dep-1) = trueHead
        tkmat(tkoffset + dep - 1) = 1
        for (head <- 0 to slen if dep != head) {
          tkmat(head * slen + dep - 1) = if (head == trueHead) -1 else 0
        }
      }
      else if (trues > 1) {
        heads(dep-1) = -2
      }
      else {
        heads(dep-1) = -1
      }
    }
    // Calculate the potential's log partition function
//    println("tkmat = [%s]".format(tkmat.mkString(", ")))
//    println("gradmat = [%s]".format(gradmat.mkString(", ")))
    val z = sumTree(tkmat, gradmat, slen, multirooted=false)
//    println(z)
    // Originally had a check here for Z != 0
    // Compute outgoing messages in terms of Z and incoming messages
    for (dep <- 1 to slen) {
      val koff = (dep - 1) * slen
      val tkoff = dep * slen
      for (head <- 0 to slen if dep != head) {
        var m = heads(dep-1) match {
          case -2 =>	Array(Double.NaN, Double.NaN) // Incoming beliefs are a conflicting configuration
          case -1 =>  {                             // No absolute belief was found pertaining to this head
            val s = if (head > dep) 1 else 0
            val n = gradmat(koff + head - s)
            Array(1 + tkmat(head * slen + dep - 1) * n, n)
          }
          case _ => if (heads(dep-1) == head) Array(0.0, 1.0) else Array(1.0, 0.0) // Set outgoing message deterministically
        }
        m = m.map(Math.log(_))
        indexedEdges(head, dep).msgs.asDiscrete.f2n(0) = m(0)
        indexedEdges(head, dep).msgs.asDiscrete.f2n(1) = m(1)
      }
    }
  }

  def sumTree(tkmat: Array[Double], gradmat: Array[Double], slen: Int, multirooted: Boolean = false, verbose: Boolean = false): Double = {
    val sch = Array.ofDim[Double](slen+1, slen+1, 2, 2)
    val gch = Array.ofDim[Double](slen+1, slen+1, 2, 2)
    var res = 0.0
    val start = if (multirooted) 0 else 1
    for (i <- 0 until slen*slen) gradmat(i) = Double.NegativeInfinity
    for (s <- 0 to slen; i <- 0 to 1; j <- 0 to 1) sch(s)(s)(i)(j) = 0.0
    for (width <- 1 to slen; s <- start to slen) {
      val t = s + width
      if (t <= slen) {
        for (i <- 0 to 1; j <- 0 to 1) sch(s)(t)(i)(j) = Double.NegativeInfinity
        if (s > 0) {
          val lkid = log(-1.0 * tkmat(t * slen + s-1))
          for (r <- s until t) {
            sch(s)(t)(0)(0) = logIncrement(sch(s)(t)(0)(0), sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1) + lkid)
          }
        }
        val rkid = Math.log(-1.0 * tkmat(s * slen + t-1))
        for (r <- s until t) sch(s)(t)(1)(0) = logIncrement(sch(s)(t)(1)(0), sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1) + rkid)
        for (r <- s until t) sch(s)(t)(0)(1) = logIncrement(sch(s)(t)(0)(1), sch(s)(r)(0)(1) + sch(r)(t)(0)(0))
        for (r <- s+1 to t) sch(s)(t)(1)(1) = logIncrement(sch(s)(t)(1)(1), sch(s)(r)(1)(0) + sch(r)(t)(1)(1))
      }
    }
    if (!multirooted) {
      sch(0)(slen)(1)(1) = Double.NegativeInfinity
      for (r <- 1 to slen) {
        sch(0)(slen)(1)(1) = logIncrement(sch(0)(slen)(1)(1), sch(1)(r)(0)(1) + sch(r)(slen)(1)(1) + Math.log(-1.0 * tkmat(r-1)))
      }
    }
    res = sch(0)(slen)(1)(1)
    for (s <- 0 to slen; t <- s to slen; i <- 0 to 1; j <- 0 to 1) {
      gch(s)(t)(i)(j) = Double.NegativeInfinity
    }
    gch(0)(slen)(1)(1) = -1.0 * res
    if (!multirooted) {
      for (r <- 1 to slen) {
        gch(1)(r)(0)(1) = logIncrement(gch(1)(r)(0)(1),
          -1.0 * res + sch(r)(slen)(1)(1) + log(-1.0 * tkmat(r-1)))
        gch(r)(slen)(1)(1) = logIncrement(gch(r)(slen)(1)(1),
          -1.0 * res + sch(1)(r)(0)(1) + log(-1.0 * tkmat(r-1)))
        gradmat((r-1) * slen) = logIncrement(gradmat((r-1) * slen),
          -1.0 * res + sch(1)(r)(0)(1) + sch(r)(slen)(1)(1))
      }
    }
    for (width <- slen to 1 by -1; s <- start to slen) {
      val t = s + width
      if (t <= slen) {
        var gpar = gch(s)(t)(1)(1)
        for (r <- s+1 to t) {
          gch(s)(r)(1)(0) = logIncrement(gch(s)(r)(1)(0), gpar + sch(r)(t)(1)(1))
          gch(r)(t)(1)(1) = logIncrement(gch(r)(t)(1)(1), gpar + sch(s)(r)(1)(0))
        }
        gpar = gch(s)(t)(0)(1)  // this seems to be s,r instead of s,t for some reason
        for (r <- s until t) {
          gch(s)(r)(0)(1) = logIncrement(gch(s)(r)(0)(1), gpar + sch(r)(t)(0)(0))
          gch(r)(t)(0)(0) = logIncrement(gch(r)(t)(0)(0), gpar + sch(s)(r)(0)(1))
        }
        if (s > 0) {
          var lgrad = Double.NegativeInfinity
          val lkid = scala.math.log(-1.0 * tkmat(t * slen + s-1))
          gpar = gch(s)(t)(0)(0)
          for (r <- s until t) {
            gch(s)(r)(1)(1) 	= logIncrement(gch(s)(r)(1)(1), gpar + sch(r+1)(t)(0)(1) + lkid)
            gch(r+1)(t)(0)(1) = logIncrement(gch(r+1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + lkid)
            lgrad = logIncrement(lgrad, gpar + sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1))
          }
          gradmat((s-1) * slen + t-1) = logIncrement(gradmat((s-1) * slen + t-1), lgrad)
        }
        val rkid = Math.log(-1.0 * tkmat(s * slen + t-1))
        var rgrad = Double.NegativeInfinity
        gpar = gch(s)(t)(1)(0)
        for (r <- s until t) {
          gch(s)(r)(1)(1)   = logIncrement(gch(s)(r)(1)(1), gpar + sch(r+1)(t)(0)(1) + rkid)
          gch(r+1)(t)(0)(1) = logIncrement(gch(r+1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + rkid)
          rgrad = logIncrement(rgrad, gpar + sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1))
        }
        gradmat((t-1) * slen + s) = logIncrement(gradmat((t-1) * slen + s), rgrad)
      }
    }
    for (i <- 0 until slen * slen) gradmat(i) = Math.exp(gradmat(i))
    return Math.abs(res)
  }

  def logIncrement(s: Double, x: Double): Double = {
    if (s == Double.NegativeInfinity) {
      x
    }
    else {
      val d = s - x
      if (d >= 0) {
        if (d <= 745) s + log(1.0 + exp(-1.0 * d)) else s
      }
      else if (d < -745) {
        x
      }
      else {
        x + log(1.0 + exp(d))
      }
    }
  }

 */
