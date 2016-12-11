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
package io.github.mandar2812.dynaml.models.neuralnets

import scala.collection.mutable.{MutableList => ML}
import akka.actor.{Actor, ActorPath, ActorRef, ActorSystem, Props}
import akka.event.Logging
import io.github.mandar2812.dynaml.models.Model
import io.github.mandar2812.dynaml.models.neuralnets.utils.{BatchSignal, Signal, UnitSignal}

/**
  * @author mandar2812 date: 2/11/2016
  * Implements the neuron as an akka [[Actor]]
  *
  */
class PerceptronActor(act: String = TransferFunctions.SIGMOID) extends Actor {

  val log = Logging(context.system, this)

  var activation: String = act

  private def actFunc: (Double) => Double = TransferFunctions.getActivation(activation)

  private def diffActFunc: (Double) => Double = TransferFunctions.getDiffActivation(activation)

  protected val incomingSynapses: ML[Synapse.Incoming] = ML()

  protected val outgoingSynapses: ML[Synapse.Outgoing] = ML()

  def ->(ne: ActorRef): Unit = {

  }

  object Synapse {

    case class Incoming(preSynapticNeuron: ActorRef) {
      private var weight: Double = scala.util.Random.nextDouble()

      def w = weight

      def w_(we: Double):Unit = weight = we

      def delw(d: Double): Unit = weight -= d

    }

    case class Outgoing(postSynapticNeuron: ActorRef) {
      private var weight: Double = scala.util.Random.nextDouble()

      def w = weight

      def w_(we: Double):Unit = weight = we

      def delw(d: Double): Unit = weight -= d

      def fire(x: AnyRef): Unit = x match {
        case UnitSignal(Signal.F, data) =>
          postSynapticNeuron ! w*data
        case BatchSignal(Signal.F, data) =>
          postSynapticNeuron ! data.map(_*w)
      }
    }
  }

  def receive = {

    case UnitSignal(Signal.F, data) =>
      log.info("Received value: "+data.hashCode())
      outgoingSynapses.foreach(_.fire(UnitSignal(Signal.F, actFunc(data))))

    case BatchSignal(Signal.F, data) =>
      log.info("Received Batch: "+data.hashCode())
      outgoingSynapses.foreach(_.fire(BatchSignal(Signal.F, data.map(actFunc))))

    case UnitSignal(Signal.B, data) =>
      //Revise presynaptic weights
      //propogate delta backward
      //TODO: Actual Implementation
      log.info("Received Back-propagated Value: "+data.hashCode())

    case BatchSignal(Signal.B, data) =>
      //Revise presynaptic weights
      //propogate delta backward
      //TODO: Actual Implementation
      log.info("Received Back-propagated Batch: "+data.hashCode())

    case _ => log.info("Received unknown message")
  }
}

object PerceptronActor {
  def apply(activation: String)(implicit actorSystem: ActorSystem) =
    actorSystem.actorOf(Props(classOf[PerceptronActor], activation))
}

class InputActor(outerActor: ActorRef) extends PerceptronActor {

  //Input Neurons serve only as sensors from outside world/actors
  activation = TransferFunctions.LIN

  private val incomingConnection = Synapse.Incoming(outerActor)
  incomingConnection.w_(1.0)

  override final val incomingSynapses = ML(incomingConnection)


}

class OutputActor(outerActor: ActorRef) extends PerceptronActor {

  //Output neurons serve only as emitters to outside world/actors
  private val outgoingConnection = Synapse.Outgoing(outerActor)
  outgoingConnection.w_(1.0)

  override final val outgoingSynapses = ML(outgoingConnection)

}


class PerceptronLayer(neurons: ML[ActorRef]) {

  def ->(otherLayer: PerceptronLayer): Unit = {}

}

class InputLayer(nodes: ML[ActorRef]) extends PerceptronLayer(nodes)
class OutputLayer(nodes: ML[ActorRef]) extends PerceptronLayer(nodes)


object NeuralFactory {

  def layer(activations: List[String])(implicit actorSys: ActorSystem) = {
    // Create a set of perceptron actors and enclose them
    // in a layer
    new PerceptronLayer(ML(activations:_*).map(ac => {
      //Create a new Perceptron Actor
      PerceptronActor(ac)
    }))
  }
}

abstract class PerceptronNetwork[T, Q, R] extends Model[T, Q, R] {

  val inputLayer: InputLayer

  val hiddenLayers: ML[PerceptronLayer]

  val outputLayer: OutputLayer

  /**
    * Predict the value of the
    * target variable given a
    * point.
    *
    **/
  //override def predict(point: Q): R = _
}