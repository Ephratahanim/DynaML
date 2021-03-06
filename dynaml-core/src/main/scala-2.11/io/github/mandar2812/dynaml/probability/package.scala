package io.github.mandar2812.dynaml

import scalaxy.streams.optimize
import spire.algebra.InnerProductSpace

import scala.util.Random

/**
  * Created by mandar on 11/01/2017.
  */
package object probability {

  var candidates: Int = 10000

  def E[@specialized(Double) I](rv: RandomVariable[I])(implicit f: InnerProductSpace[I, Double]): I = optimize {
    f.divr(
      rv.iid(candidates)
        .sample()
        .reduce(
          (x, y) => f.plus(x,y)
        ),
      candidates.toDouble)
  }


  def E(rv: RandomVariable[Double]): Double = optimize {
    rv.iid(candidates).sample().sum/candidates.toDouble
  }


  /**
    * Calculate the (monte carlo approximation to) mean, median, mode, lower and upper confidence interval.
    *
    *
    *
    * @param r Continuous random variable in question
    * @param alpha Probability of exclusion, i.e. return 100(1-alpha) % confidence interval.
    *              alpha should be between 0 and 1
    * */
  def OrderStats(r: ContinuousDistrRV[Double], alpha: Double): (Double, Double, Double, Double, Double) = {

    val samples = r.iid(candidates).sample()

    val mean = samples.sum/candidates.toDouble

    val median = utils.median(samples)

    val (lowerbar, higherbar) = (
      utils.quickselect(samples, math.ceil(candidates*alpha*0.5).toInt),
      utils.quickselect(samples, math.ceil(candidates*(1.0 - 0.5*alpha)).toInt))

    val uDist = r.underlyingDist

    val mode = samples.map(s => (uDist.logPdf(s), s)).max._2

    (mean, median, mode, lowerbar, higherbar)
  }
}
