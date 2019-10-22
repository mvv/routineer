/*
 * Copyright (C) 2010, 2019 Mikhail Vorozhtsov
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

final case class Route[O <: Args, +H[_ <: Args]](pattern: RoutePattern[O], handler: H[O])

object Route {
  type NoHandler[_ <: Args] = Nothing

  type Handler[R, O <: Args] = O#Fn[R]
  object Handler {
    trait Apply[R] {
      type H[O <: Args] = Handler[R, O]
    }
  }
  def handle[R, O <: Args](pattern: RoutePattern[O])(handler: O#Fn[R]): Route[O, Handler.Apply[R]#H] =
    Route[O, Handler.Apply[R]#H](pattern, handler)

  final case class WithEnv[E, R, O <: Args](handler: O#Prepend[E]#Fn[R], growable: O <:< O with Args.Growable)
  object WithEnv {
    trait Apply[E, R] {
      type H[O <: Args] = WithEnv[E, R, O]
    }
  }

  def withEnv[E, R, O <: Args](
      pattern: RoutePattern[O]
  )(handler: O#Prepend[E]#Fn[R])(implicit witness: O <:< O with Args.Growable): Route[O, WithEnv.Apply[E, R]#H] =
    Route[O, WithEnv.Apply[E, R]#H](pattern, WithEnv[E, R, O](handler, witness))
}
