/*
 * Copyright (C) 2011-2013 Mikhail Vorozhtsov
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

import _root_.cats.Monoid
import _root_.cats.arrow.Arrow

import scala.language.higherKinds

package object cats {
  implicit def routineerRoutesMonoid[H[_ <: Args]]: Monoid[Routes[H]] =
    new Monoid[Routes[H]] {
      override def empty: Routes[H] = Routes.empty
      override def combine(rs1: Routes[H], rs2: Routes[H]): Routes[H] = rs1 ++ rs2
    }
  implicit object routineerValuePatternArrow extends Arrow[ValuePattern] {
    override def id[A]: ValuePattern[A, A] = ValuePattern.id[A]
    override def lift[A, B](f: A => B): ValuePattern[A, B] = ValuePattern.map(f)
    override def compose[A, B, C](f: ValuePattern[B, C], g: ValuePattern[A, B]): ValuePattern[A, C] = g >>> f
    override def first[A, B, C](pattern: ValuePattern[A, B]): ValuePattern[(A, C), (B, C)] = pattern *** id[C]
    override def second[A, B, C](pattern: ValuePattern[A, B]): ValuePattern[(C, A), (C, B)] = id[C] *** pattern
  }
}
