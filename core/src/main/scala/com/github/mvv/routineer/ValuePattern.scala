/*
 * Copyright (C) 2010, 2012, 2019 Mikhail Vorozhtsov
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
import scala.util.{Failure, Success, Try}

trait ValuePattern[-I, +O] {
  def apply(in: I): ValuePattern.Match[O]
  final def contramap[I1](f: I1 => I): ValuePattern[I1, O] =
    ValuePattern.Chain(ValuePattern.Conv(f), this)
  final def map[O1](f: O => O1): ValuePattern[I, O1] =
    ValuePattern.Chain(this, ValuePattern.Conv(f))
  final def >>>[I1 >: O, O1](pat: ValuePattern[I1, O1]): ValuePattern[I, O1] =
    ValuePattern.Chain(this, pat)
  final def &&&[I1 <: I, O1](pat: ValuePattern[I1, O1]): ValuePattern[I1, (O, O1)] =
    ValuePattern.Fanout(this, pat)
  final def ***[I1, O1](pat: ValuePattern[I1, O1]): ValuePattern[(I, I1), (O, O1)] =
    ValuePattern.Split(this, pat)
  final def check: ValueCheck[I] = ValueCheck(this)
  def ! : ValuePattern.Force[I, O] = ValuePattern.Force(this)
  def ~ : ValuePattern.Backtrack[I, O] = ValuePattern.Backtrack(this)
}
object ValuePattern {
  final case class Force[-I, +O](pattern: ValuePattern[I, O])
  final case class Backtrack[-I, +O](pattern: ValuePattern[I, O])

  sealed trait Match[+O] {
    def map[O1](f: O => O1): Match[O1]
    def sum[O1](other: Match[O1]): Match[(O, O1)]
    def toOption: Option[O]
  }
  object Match {
    def fromTry[O](tried: Try[O]): Match[O] = tried match {
      case Success(value) => Matched(value)
      case Failure(e)     => NotMatched(Option(e.getMessage).getOrElse(e.getClass.getCanonicalName))
    }
  }

  final case class Matched[+O](value: O) extends Match[O] {
    override def map[O1](f: O => O1): Match[O1] = Matched(f(value))
    override def sum[O1](other: Match[O1]): Match[(O, O1)] = other match {
      case Matched(otherValue) => Matched((value, otherValue))
      case reasons: NotMatched => reasons
    }
    override def toOption: Option[O] = Some(value)
  }

  final case class NotMatched(first: String, rest: String*) extends Match[Nothing] {
    override def map[O1](f: Nothing => O1): Match[O1] = this
    override def sum[O1](other: Match[O1]): Match[(Nothing, O1)] = other match {
      case Matched(_)        => this
      case other: NotMatched => this ++ other
    }
    override def toOption: Option[Nothing] = None
    def ++(other: NotMatched): NotMatched = NotMatched(first, (rest ++ other.reasons): _*)
    def reasons: Seq[String] = first +: rest
  }

  final private case class Lifted[-I, +O](f: I => Match[O]) extends ValuePattern[I, O] {
    def apply(in: I): Match[O] = f(in)
  }
  final private case class Conv[-I, +O](f: I => O) extends ValuePattern[I, O] {
    def apply(in: I): Match[O] = Matched(f(in))
  }
  final private case class Chain[-I, O, +O1](first: ValuePattern[I, O], second: ValuePattern[O, O1])
      extends ValuePattern[I, O1] {
    def apply(in: I): Match[O1] = first.apply(in) match {
      case Matched(out)        => second(out)
      case reasons: NotMatched => reasons
    }
  }
  final private case class Fanout[-I, +O1, +O2](first: ValuePattern[I, O1], second: ValuePattern[I, O2])
      extends ValuePattern[I, (O1, O2)] {
    def apply(in: I): Match[(O1, O2)] = first.apply(in).sum(second.apply(in))
  }
  final private case class Split[-I1, -I2, +O1, +O2](first: ValuePattern[I1, O1], second: ValuePattern[I2, O2])
      extends ValuePattern[(I1, I2), (O1, O2)] {
    def apply(in: (I1, I2)): Match[(O1, O2)] = first.apply(in._1).sum(second.apply(in._2))
  }

  /** Use a matching function as a [[ValuePattern]] */
  implicit def apply[I, O](f: I => Match[O]): ValuePattern[I, O] = ValuePattern.Lifted(f)

  /** Use a function as a [[ValuePattern]] */
  def map[I, O](f: I => O): ValuePattern[I, O] = ValuePattern.Conv(f)
}

/** Accept-all [[ValuePattern]] for strings */
object * extends ValuePattern[String, String] {
  def apply(in: String): ValuePattern.Match[String] = ValuePattern.Matched(in)
  override def toString = "*"
}

/** A [[ValuePattern]] that accepts only the provided value */
sealed case class EqualsP[A](value: A) extends ValuePattern[A, A] {
  def apply(in: A): ValuePattern.Match[A] =
    if (in == value) ValuePattern.Matched(in) else ValuePattern.NotMatched(s"Value '$in' is not equal to '$value'")
}

/** A [[ValuePattern]] that matches textual representations of `Int` values
  * (using the provided radix).
  */
sealed class IntP(val radix: Int) extends ValuePattern[String, Int] {
  final def apply(in: String): ValuePattern.Match[Int] =
    ValuePattern.Match.fromTry(Try(java.lang.Integer.parseInt(in, radix)))
  override def toString: String = s"IntP($radix)"
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
final case class NonNegativeP[A]()(implicit num: Numeric[A]) extends ValuePattern[A, A] {
  def apply(in: A): ValuePattern.Match[A] =
    if (num.gteq(in, num.zero)) ValuePattern.Matched(in) else ValuePattern.NotMatched(s"Negative value $in")
}

/** Matches positive numeric values. */
final case class PositiveP[A]()(implicit num: Numeric[A]) extends ValuePattern[A, A] {
  def apply(in: A): ValuePattern.Match[A] =
    if (num.gt(in, num.zero)) ValuePattern.Matched(in) else ValuePattern.NotMatched(s"Non-positive value $in")
}

/** Use a regular expression as a [[ValuePattern]]. */
final case class RegexP(regex: Regex) extends ValuePattern[String, Seq[String]] {
  def apply(in: String): ValuePattern.Match[Seq[String]] = regex.unapplySeq(in) match {
    case Some(values) => ValuePattern.Matched(values)
    case None         => ValuePattern.NotMatched(s"Input '$in' does not match regex '${regex.regex}'")
  }
}

object MultiP extends ValuePattern[Seq[String], Seq[String]] {
  def apply(in: Seq[String]): ValuePattern.Match[Seq[String]] = ValuePattern.Matched(in)
}

object SingleP extends ValuePattern[Seq[String], String] {
  def apply(in: Seq[String]): ValuePattern.Match[String] = in.lastOption match {
    case Some(value) => ValuePattern.Matched(value)
    case None        => ValuePattern.NotMatched("No value")
  }
}

object OptionalP extends ValuePattern[Seq[String], Option[String]] {
  override def apply(in: Seq[String]): ValuePattern.Match[Option[String]] = ValuePattern.Matched(in.lastOption)
}
