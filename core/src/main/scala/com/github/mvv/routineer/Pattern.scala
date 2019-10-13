/*
 * Copyright (C) 2010, 2012 Mikhail Vorozhtsov
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
import scala.language.implicitConversions

/** Kleisli arrow for `Option` with equality. */
trait Pattern[-I, +O] {
  def matches(in: I): Option[O]
  final def >>>[I1 >: O, O1](pat: Pattern[I1, O1]): Pattern[I, O1] =
    Pattern.Chain[I, I1, O1](this, pat)
  final def &&&[I1 <: I, O1](pat: Pattern[I1, O1]): Pattern[I1, (O, O1)] =
    Pattern.Fanout[I1, O, O1](this, pat)
  final def ***[I1, O1](pat: Pattern[I1, O1]) =
    Pattern.Split(this, pat)
}
object Pattern {
  final case class Lifted[-I, +O](f: I => Option[O]) extends Pattern[I, O] {
    def matches(in: I): Option[O] = f(in)
  }
  final case class Conv[-I, +O](f: I => O) extends Pattern[I, O] {
    def matches(in: I): Option[O] = Some(f(in))
  }
  final case class Chain[-I, O, +O1](first: Pattern[I, O], second: Pattern[O, O1]) extends Pattern[I, O1] {
    def matches(in: I): Option[O1] = first.matches(in).flatMap(second.matches)
  }
  final case class Fanout[-I, +O1, +O2](first: Pattern[I, O1], second: Pattern[I, O2]) extends Pattern[I, (O1, O2)] {
    def matches(in: I): Option[(O1, O2)] =
      first.matches(in).flatMap(out1 => second.matches(in).map((out1, _)))
  }
  final case class Split[-I1, -I2, +O1, +O2](first: Pattern[I1, O1], second: Pattern[I2, O2])
      extends Pattern[(I1, I2), (O1, O2)] {
    def matches(in: (I1, I2)): Option[(O1, O2)] =
      first.matches(in._1).flatMap(out1 => second.matches(in._2).map((out1, _)))
  }

  /** Use a matching function as a [[Pattern]] */
  @inline
  implicit def apply[I, O](f: I => Option[O]): Pattern[I, O] = Pattern.Lifted(f)

  /** Use a function as a [[Pattern]] */
  @inline
  def map[I, O](f: I => O): Pattern[I, O] = Pattern.Conv(f)
}

/** Accept-all [[Pattern]] for strings */
object * extends Pattern[String, String] {
  def matches(in: String): Option[String] = Some(in)
  override def toString = "*"
}

/** A [[Pattern]] that matches textual representations of `Int` values
  * (using the provided radix).
  */
sealed class IntP(val radix: Int) extends Pattern[String, Int] {
  final def matches(in: String): Option[Int] =
    try {
      Some(java.lang.Integer.parseInt(in, radix))
    } catch {
      case e: Throwable => None
    }
  override def toString: String = "IntP(" + radix + ")"
  override def hashCode: Int = radix
  override def equals(that: Any): Boolean = that match {
    case that: IntP => radix == that.radix
    case _          => false
  }
}

/** Matches `Int` values written in the decimal numeral system. */
object IntP extends IntP(10) {
  private val MaxRadix = 36
  private val patterns =
    (2 to MaxRadix).map { r =>
      if (r == 10) IntP else new IntP(r)
    }.toArray
  def apply(radix: Int): IntP = {
    require(radix >= 2 && radix <= MaxRadix)
    patterns(radix - 2)
  }
  def unapply(pat: IntP) = Some(pat.radix)
}

/** Matches non-negative numeric values. */
final case class NonNegativeP[A]()(implicit num: Numeric[A]) extends Pattern[A, A] {
  def matches(in: A): Option[A] = if (num.gteq(in, num.zero)) Some(in) else None
}

/** Matches positive numeric values. */
final case class PositiveP[A]()(implicit num: Numeric[A]) extends Pattern[A, A] {
  def matches(in: A): Option[A] = if (num.gt(in, num.zero)) Some(in) else None
}

/** Use a regular expression as a [[Pattern]]. */
final case class RegexP(regex: Regex) extends Pattern[String, Seq[String]] {
  def matches(in: String): Option[List[String]] = regex.unapplySeq(in)
}
