/*
 * Copyright (C) 2019 Mikhail Vorozhtsov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.mvv.routineer

import scala.language.higherKinds

sealed trait Dispatch[+H[_ <: Args]]

object Dispatch {
  final case class Handler[O <: Args, +H[_ <: Args]](args: O, handler: H[O], next: () => Dispatch[H])
      extends Dispatch[H]
  final case class Failure(context: Context, first: String, rest: String*) extends Dispatch[Route.NoHandler] {
    def toSeq: Seq[String] = first +: rest
  }
  case object NotFound extends Dispatch[Route.NoHandler]

  sealed trait Context
  final case class InSegment(prefix: Seq[String], segment: String) extends Context
  final case class InSegments(prefix: Seq[String], segments: String) extends Context
  final case class InParam(prefix: Seq[String], name: String, value: Option[String]) extends Context
  final case class InParams(prefix: Seq[String], params: Map[String, ParamValues]) extends Context
}
