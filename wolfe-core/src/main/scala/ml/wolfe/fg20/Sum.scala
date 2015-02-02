package ml.wolfe.fg20

import ml.wolfe.term.Setting

/**
 * A sum of potential functions forms a potential as well.
 * @tparam P the type of argument potentials.
 */
trait Sum[P <: Potential] extends Potential {
  def args: Seq[P]
  lazy val discVars = args.flatMap(_.discVars).distinct.toArray
  //distinct does not work with iterators
  lazy val contVars = args.flatMap(_.contVars).distinct.toArray
  lazy val vectVars = args.flatMap(_.vectVars).distinct.toArray

  lazy val discVar2Index = discVars.iterator.zipWithIndex.toMap
  lazy val contVar2Index = contVars.iterator.zipWithIndex.toMap
  lazy val vectVar2Index = vectVars.iterator.zipWithIndex.toMap


  lazy val argMaps = args.map(a => new ArgMap(
    a.discVars.map(discVar2Index),
    a.contVars.map(contVar2Index),
    a.vectVars.map(vectVar2Index)))

  def scorer() = new Scorer {

    lazy val scorers = args.map(_.scorer())

    def score(setting: Setting) = {
      val scores = for (((arg, map), scorer) <- (args.iterator zip argMaps.iterator) zip scorers.iterator) yield {
        val local = arg.createSetting()
        for (i <- 0 until arg.discVars.length) local.disc(i) = setting.disc(map.tgtDisc(i))
        for (i <- 0 until arg.contVars.length) local.cont(i) = setting.cont(map.tgtCont(i))
        for (i <- 0 until arg.vectVars.length) local.vect(i) = setting.vect(map.tgtVect(i))
        val localScore = scorer.score(local)
        localScore
      }
      scores.sum
    }
  }

}


class FlatSum[P <: Potential](val args: Seq[P]) extends Sum[P]

trait DifferentiableSum extends Sum[Differentiable] with Differentiable {
  //rather define it as GradientCalculator ?
  sum =>


  override def gradientCalculator(): GradientCalculator = new GradientCalculator {
    class PerTerm(val pot: Differentiable) {
      val calc        = pot.gradientCalculator()
      val localUpdate = new Setting(pot.discVars.length, pot.contVars.length, pot.vectVars.length)
      val local       = pot.createPartialSetting()

    }
    val terms       = args.map(new PerTerm(_))
    val totalUpdate = new Setting(discVars.length, contVars.length, vectVars.length)

    override def gradientAndValue(currentParameters: PartialSetting, gradient: Setting): Double = {
      totalUpdate := 0.0
      var totalSum = 0.0
      for ((arg, map) <- terms.iterator zip argMaps.iterator) {
        //could not get it to work with iterators (?)
        arg.local.copyFrom(currentParameters, map)
        totalSum += arg.calc.gradientAndValue(arg.local, arg.localUpdate)
        for (i <- 0 until arg.pot.discVars.length) if (totalUpdate.disc(map.tgtDisc(i)) != null)
          totalUpdate.disc(map.tgtDisc(i)) += arg.localUpdate.disc(i)
        else totalUpdate.disc(map.tgtDisc(i)) = arg.localUpdate.disc(i) //todo be more efficient
        for (i <- 0 until arg.pot.contVars.length) if (totalUpdate.cont(map.tgtCont(i)) != null)
          totalUpdate.cont(map.tgtCont(i)) += arg.localUpdate.cont(i)
        else totalUpdate.cont(map.tgtCont(i)) = arg.localUpdate.cont(i)
        for (i <- 0 until arg.pot.vectVars.length) if (totalUpdate.vect(map.tgtVect(i)) != null)
          totalUpdate.vect(map.tgtVect(i)) += arg.localUpdate.vect(i)
        else totalUpdate.vect(map.tgtVect(i)) = arg.localUpdate.vect(i)
      }
      //why not directly editing the gradient?
      for (i <- 0 until sum.discVars.length) gradient.disc(i) = totalUpdate.disc(i) //todo, change only the ones that change
      for (i <- 0 until sum.contVars.length) gradient.cont(i) = totalUpdate.cont(i)
      for (i <- 0 until sum.vectVars.length) gradient.vect(i) = totalUpdate.vect(i)
      totalSum
    }
  }
}
