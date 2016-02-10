package io.github.mandar2812.dynaml.models.neuralnets

import breeze.linalg.DenseVector
import io.github.mandar2812.dynaml.models.LinearModel
import io.github.mandar2812.dynaml.optimization.{BackPropogation, CommitteeModelSolver, RegularizedOptimizer}
import io.github.mandar2812.dynaml.pipes.DataPipe

/**
  * Created by mandar on 9/2/16.
  */
abstract class CommitteeNetwork[D](data: D,
                                   transform: DataPipe[D, Stream[(DenseVector[Double],
                                     DenseVector[Double])]],
                                   networks: FFNeuralGraph*) extends
LinearModel[D, Int, Int, DenseVector[Double], DenseVector[Double],
  Double, Stream[(DenseVector[Double], Double)]] {

  override protected val g: D = data

  val baseNetworks: List[FFNeuralGraph] = networks.toList

  val baseOptimizer = new BackPropogation()
    .setMomentum(0.01)
    .setMiniBatchFraction(0.5)
    .setNumIterations(20)

  val num_points = dataAsStream(g).length

  def dataAsStream(d: D) = transform.run(d)

  /**
    * Predict the value of the
    * target variable given a
    * point.
    *
    **/
  override def predict(point: DenseVector[Double]): Double =
    params dot featureMap(point)

  override def clearParameters(): Unit =
    DenseVector.fill[Double](baseNetworks.length)(1.0)

  override def initParams(): DenseVector[Double] =
    DenseVector.fill[Double](baseNetworks.length)(1.0)

  /**
    * Learn the parameters
    * of the model which
    * are in a node of the
    * graph.
    *
    **/
  override def learn(): Unit = {
    //First learn the base Networks
    baseNetworks.foreach(network => {
      baseOptimizer.optimize(num_points,dataAsStream(g),network)
    })

    params = optimizer.optimize(
      num_points,
      dataAsStream(g).map(couple =>
        (featureMap(couple._1), couple._2(0))),
      initParams()
    )
  }

  override protected val optimizer: RegularizedOptimizer[Int, DenseVector[Double],
    DenseVector[Double], Double,
    Stream[(DenseVector[Double], Double)]] = new CommitteeModelSolver()

  override protected var params: DenseVector[Double] =
    DenseVector.fill[Double](baseNetworks.length)(1.0)

  featureMap = (pattern) =>
    DenseVector(baseNetworks.map(net => net.forwardPass(pattern)(0)).toArray)

}
