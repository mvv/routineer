/*
 * Copyright (C) 2010 Mikhail Vorozhtsov
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

import scala.util.matching.Regex

trait Pattern[-I, O] {
  def matches(in: I): Option[O]
  final def >>>[O1](pattern: Pattern[O, O1]) = Pattern.Chain(this, pattern)
}
object Pattern {
  final case class Chain[-I, O, O1](
                     first: Pattern[I, O], second: Pattern[O, O1])
                   extends Pattern[I, O1] {
    def matches(in: I) = first.matches(in).flatMap(second.matches)
  }
  final case class Product[-I, O1, O2](
                     first: Pattern[I, O1], second: Pattern[I, O2])
                   extends Pattern[I, (O1, O2)] {
    def matches(in: I) =
      first.matches(in).flatMap(out1 => second.matches(in).map((out1, _)))
  }
}

object * extends Pattern[String, String] {
  def matches(in: String) = Some(in)
  override def toString = "*"
}

sealed case class IntP(radix: Int) extends Pattern[String, Int] {
  final def matches(in: String) =
    try {
      Some(java.lang.Integer.parseInt(in, radix))
    } catch {
      case e: Throwable => None
    }
}
object IntP extends IntP(10)

final case class NonNegative[A](implicit num: Numeric[A])
                 extends Pattern[A, A] {
  def matches(in: A) = if (num.gteq(in, num.zero)) Some(in) else None
}

final case class Positive[A](implicit num: Numeric[A])
                 extends Pattern[A, A] {
  def matches(in: A) = if (num.gt(in, num.zero)) Some(in) else None
}

final case class RegexP(regex: Regex) extends Pattern[String, Seq[String]] {
  def matches(in: String) = regex.unapplySeq(in)
}
