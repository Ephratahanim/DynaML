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
package io.github.mandar2812.dynaml.pipes

/**
  * @author mandar2812 on 18/11/15.
  *
  * Top level trait representing an
  * abstract pipe that defines a transformation
  * between two data types, i.e. [[Source]] and [[Destination]]
  */
trait DataPipe[-Source, +Destination] {

  def run(data: Source): Destination

  def apply(data: Source): Destination = run(data)

  /**
    * Represents the composition of two
    * pipes, resulting in a third pipe
    * Schematically represented as:
    *
    * [[Source]] -> [[Destination]] :: [[Destination]] -> [[Further]] ==
    * [[Source]] -> [[Further]]
    *
    * */
  def >[Further](that: DataPipe[Destination, Further]):
  DataPipe[Source, Further] = {
    val runFunc = (d: Source) => that.run(this.run(d))
    DataPipe(runFunc)
  }

  def *[OtherSource, OtherDestination](that: DataPipe[OtherSource, OtherDestination])
  :ParallelPipe[Source, Destination, OtherSource, OtherDestination] = ParallelPipe(this.run, that.run)
}

object DataPipe {

  def apply[D](func: () => D): DataPipe[Unit, D] = new DataPipe[Unit, D] {
    def run(x: Unit) = func()
  }

  def apply[S,D](func: (S) => D):
  DataPipe[S, D] = {
    new DataPipe[S,D] {
      def run(data: S) = func(data)
    }
  }

  def apply[S1, D1, S2, D2](pipe1: DataPipe[S1, D1],
                            pipe2: DataPipe[S2, D2]): ParallelPipe[S1, D1, S2, D2] =
    ParallelPipe(pipe1.run, pipe2.run)

  def apply[S, D1, D2](func: (S) => (D1, D2)):
  BifurcationPipe[S, D1, D2] = {
    new BifurcationPipe[S, D1, D2] {
      def run(data: S) = func(data)
    }
  }

  def apply[S](func: (S) => Unit):
  SideEffectPipe[S] = {
    new SideEffectPipe[S] {
      def run(data: S) = func(data)
    }
  }
}

trait ParallelPipe[-Source1, +Result1, -Source2, +Result2]
  extends DataPipe[(Source1, Source2), (Result1, Result2)] {

}

object ParallelPipe {
  def apply[S1, D1, S2, D2](func1: (S1) => D1, func2: (S2) => D2):
  ParallelPipe[S1, D1, S2, D2] = {
    new ParallelPipe[S1, D1, S2, D2] {
      def run(data: (S1, S2)) = (func1(data._1), func2(data._2))
    }
  }
}

trait BifurcationPipe[Source, Result1, Result2]
  extends DataPipe[Source, (Result1, Result2)] {

}

trait SideEffectPipe[I] extends DataPipe[I, Unit] {

}

object BifurcationPipe {

  def apply[Source,
  Destination1,
  Destination2](pipe1: DataPipe[Source, Destination1],
                pipe2: DataPipe[Source, Destination2]):
  BifurcationPipe[Source, Destination1, Destination2] = {

    DataPipe((x: Source) => (pipe1.run(x), pipe2.run(x)))
  }
}
