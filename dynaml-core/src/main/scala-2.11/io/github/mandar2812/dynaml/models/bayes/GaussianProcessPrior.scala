/*
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
* */
package io.github.mandar2812.dynaml.models.bayes

import spire.algebra.{Field, InnerProductSpace}
import io.github.mandar2812.dynaml.DynaMLPipe._
import io.github.mandar2812.dynaml.algebra.PartitionedVector
import io.github.mandar2812.dynaml.analysis.PartitionedVectorField
import io.github.mandar2812.dynaml.kernels.LocalScalarKernel
import io.github.mandar2812.dynaml.modelpipe.GPRegressionPipe2
import io.github.mandar2812.dynaml.models.gp.AbstractGPRegressionModel

import scala.reflect.ClassTag
import io.github.mandar2812.dynaml.pipes.{DataPipe, MetaPipe}
import io.github.mandar2812.dynaml.probability.MultGaussianPRV

/**
  * @author mandar2812 date: 21/02/2017.
  *
  * Represents a Gaussian Process Prior over functions.
  * */
abstract class GaussianProcessPrior[I: ClassTag, MeanFuncParams](
  val covariance: LocalScalarKernel[I],
  val noiseCovariance: LocalScalarKernel[I]) extends
  StochasticProcessPrior[
    I, Double, PartitionedVector,
    MultGaussianPRV, MultGaussianPRV,
    AbstractGPRegressionModel[Seq[(I, Double)], I]] {

  type GPModel = AbstractGPRegressionModel[Seq[(I, Double)], I]

  def _meanFuncParams: MeanFuncParams

  def meanFuncParams_(p: MeanFuncParams): Unit

  private val initial_covariance_state = covariance.state ++ noiseCovariance.state

  val meanFunctionPipe: MetaPipe[MeanFuncParams, I, Double]

  private var globalOptConfig = Map(
    "globalOpt" -> "GS",
    "gridSize" -> "3",
    "gridStep" -> "0.2",
    "policy" -> "GS")

  /**
    * Append the global optimization configuration
    * */
  def globalOptConfig_(conf: Map[String, String]) = globalOptConfig ++= conf

  /**
    * Data pipe which takes as input training data and a trend model,
    * outputs a tuned gaussian process regression model.
    * */
  def posteriorModelPipe =
    GPRegressionPipe2[I](covariance, noiseCovariance) >
    gpTuning(
      initial_covariance_state,
      globalOptConfig("globalOpt"),
      globalOptConfig("gridSize").toInt,
      globalOptConfig("gridStep").toDouble,
      policy = globalOptConfig("policy"),
      prior = hyperPrior) >
    DataPipe((modelAndConf: (GPModel, Map[String, Double])) => modelAndConf._1)

  /**
    * Given some data, return a gaussian process regression model
    *
    * @param data A Sequence of input patterns and responses
    * */
  override def posteriorModel(data: Seq[(I, Double)]) =
    posteriorModelPipe(data, meanFunctionPipe(_meanFuncParams))

  /**
    * Returns the distribution of response values,
    * evaluated over a set of domain points of type [[I]].
    * */
  override def priorDistribution[U <: Seq[I]](d: U) = {

    val numPoints: Long = d.length.toLong

    //Declare vector field, required implicit parameter
    implicit val field: Field[PartitionedVector] =
      PartitionedVectorField(numPoints, covariance.rowBlocking)

    //Construct mean Vector
    val meanFunc = meanFunctionPipe(_meanFuncParams)
    val meanVector = PartitionedVector(
      d.toStream.map(meanFunc(_)),
      numPoints,
      covariance.rowBlocking)

    val effectiveCov = covariance + noiseCovariance
    //Construct covariance matrix
    val covMat = effectiveCov.buildBlockedKernelMatrix(d, numPoints)

    MultGaussianPRV(meanVector, covMat)
  }
}

/**
  * @author mandar2812 date 21/02/2017.
  *
  * A gaussian process prior with a linear trend mean function.
  * */
class LinearTrendGaussianPrior[I: ClassTag](
  cov: LocalScalarKernel[I],
  n: LocalScalarKernel[I],
  trendParams: I, intercept: Double)(
  implicit inner: InnerProductSpace[I, Double]) extends
  GaussianProcessPrior[I, (I, Double)](cov, n) with
  LinearTrendStochasticPrior[I, MultGaussianPRV, MultGaussianPRV, AbstractGPRegressionModel[Seq[(I, Double)], I]]{

  override val innerProduct = inner

  override protected var params: (I, Double) = (trendParams, intercept)

  override def _meanFuncParams = params

  override def meanFuncParams_(p: (I, Double)) = params = p

  override val meanFunctionPipe = MetaPipe(
    (parameters: (I, Double)) => (x: I) => inner.dot(parameters._1, x) + parameters._2
  )
}
