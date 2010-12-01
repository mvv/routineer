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

object PathSpec {
  final case class Elem[R](prefix: Seq[String], matcher: Pattern[String, R]) {
    val length = prefix.size + 1
    def linearization: Seq[Either[String, Pattern[String, _]]] =
      prefix.map(Left(_)) :+ Right(matcher)
  }

  sealed trait TBool
  sealed trait TTrue extends TBool
  sealed trait TFalse extends TBool

  sealed trait ElemsOps[E0 <: Elems] {
    final type E = E0
    def apply[R](f: E0#Func[R], args: E0#Args): R
    def andThen[R, R1](f: E0#Func[R], g: R => R1): E0#Func[R1]
    def prependElem[H](head: Elem[H], elems: E0): E0#Prepend[H]
    def appendElem[L](elems: E0, last: Elem[L]): E0#Append[L]
    def prependArg[H](head: H, args: E#Args): E0#Prepend[H]#Args
    def appendArg[L](args: E0#Args, last: L): E0#Append[L]#Args
  }
  object ElemsOps {
    implicit val implicitElemsOps0 = PathSpec.ElemsOps0
    implicit def implicitElemsOps1[A] = new PathSpec.ElemsOps1[A]
    implicit def implicitElemsOps2[A, B] = new PathSpec.ElemsOps2[A, B]
    implicit def implicitElemsOps3[A, B, C] = new PathSpec.ElemsOps3[A, B, C]
    implicit def implicitElemsOps4[A, B, C, D] =
      new PathSpec.ElemsOps4[A, B, C, D]
  }

  sealed trait Elems {
    type Args
    type HasNext <: TBool
    type Prepend[_] <: Elems
    type Append[_] <: Elems
    type Func[_]
    def linearization: Seq[Either[String, Pattern[String, _]]]
  }
  sealed class Elems0 extends Elems {
    type Args = Unit
    type HasNext = TTrue
    type Prepend[H] = Elems1[H]
    type Append[L] = Elems1[L]
    type Func[R] = Unit => R
    def linearization = Seq.empty
  }
  val Elems0 = new Elems0
  object ElemsOps0 extends ElemsOps[Elems0] {
    def apply[R](f: Unit => R, args: Unit) = f()
    def andThen[R, R1](f: Unit => R, g: R => R1) = _ => g(f())
    def prependElem[H](head: Elem[H], elems: E) = Elems1(head)
    def appendElem[L](elems: E, last: Elem[L]) = Elems1(last)
    def prependArg[H](head: H, args: Unit) = head
    def appendArg[L](args: Unit, last: L) = last
  }
  final case class Elems1[A](_1: Elem[A]) extends Elems {
    type Args = A
    type HasNext = TTrue
    type Prepend[H] = Elems2[H, A]
    type Append[L] = Elems2[A, L]
    type Func[R] = A => R
    def linearization = _1.linearization
  }
  final class ElemsOps1[A]() extends ElemsOps[Elems1[A]] {
    def apply[R](f: E#Func[R], args: E#Args) = f(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) = x => g(f(x))
    def prependElem[H](head: Elem[H], elems: E) = Elems2(head, elems._1)
    def appendElem[L](elems: E, last: Elem[L]) = Elems2(elems._1, last)
    def prependArg[H](head: H, args: E#Args) = (head, args)
    def appendArg[L](args: E#Args, last: L) = (args, last)
  }
  final case class Elems2[A, B](_1: Elem[A], _2: Elem[B]) extends Elems {
    type Args = (A, B)
    type HasNext = TTrue
    type Prepend[H] = Elems3[H, A, B]
    type Append[L] = Elems3[A, B, L]
    type Func[R] = (A, B) => R
    def linearization = _1.linearization ++ _2.linearization
  }
  final class ElemsOps2[A, B]() extends ElemsOps[Elems2[A, B]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) = (x, y) => g(f(x, y))
    def prependElem[H](head: Elem[H], elems: E) =
      Elems3(head, elems._1, elems._2)
    def appendElem[L](elems: E, last: Elem[L]) =
      Elems3(elems._1, elems._2, last)
    def prependArg[H](head: H, args: E#Args) = (head, args._1, args._2)
    def appendArg[L](args: E#Args, last: L) = (args._1, args._2, last)
  }
  final case class Elems3[A, B, C](_1: Elem[A], _2: Elem[B], _3: Elem[C])
                   extends Elems {
    type Args = (A, B, C)
    type HasNext = TTrue
    type Prepend[H] = Elems4[H, A, B, C]
    type Append[L] = Elems4[A, B, C, L]
    type Func[R] = (A, B, C) => R
    def linearization = _1.linearization ++ _2.linearization ++ _3.linearization
  }
  final class ElemsOps3[A, B, C]() extends ElemsOps[Elems3[A, B, C]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) = (x, y, z) => g(f(x, y ,z))
    def prependElem[H](head: Elem[H], elems: E) =
      Elems4(head, elems._1, elems._2, elems._3)
    def appendElem[L](elems: E, last: Elem[L]) =
      Elems4(elems._1, elems._2, elems._3, last)
    def prependArg[H](head: H, args: E#Args) = (head, args._1, args._2, args._3)
    def appendArg[L](args: E#Args, last: L) = (args._1, args._2, args._3, last)
  }
  final case class Elems4[A, B, C, D](
                     _1: Elem[A], _2: Elem[B], _3: Elem[C], _4: Elem[D])
                   extends Elems {
    type Args = (A, B, C, D)
    type HasNext = TTrue
    type Prepend[H] = Elems5[H, A, B, C, D]
    type Append[L] = Elems5[A, B, C, D, L]
    type Func[R] = (A, B, C, D) => R
    def linearization = _1.linearization ++ _2.linearization ++
                        _3.linearization ++ _4.linearization
  }
  final class ElemsOps4[A, B, C, D]() extends ElemsOps[Elems4[A, B, C, D]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x, y, z, t) => g(f(x, y, z, t))
    def prependElem[H](head: Elem[H], elems: E) =
      Elems5(head, elems._1, elems._2, elems._3, elems._4)
    def appendElem[L](elems: E, last: Elem[L]) =
      Elems5(elems._1, elems._2, elems._3, elems._4, last)
    def prependArg[H](head: H, args: E#Args) =
      (head, args._1, args._2, args._3, args._4)
    def appendArg[L](args: E#Args, last: L) =
      (args._1, args._2, args._3, args._4, last)
  }
  final case class Elems5[A, B, C, D, E](
                     _1: Elem[A], _2: Elem[B], _3: Elem[C], _4: Elem[D],
                     _5: Elem[E])
                   extends Elems {
    type Args = (A, B, C, D, E)
    type HasNext = TFalse
    type Prepend[N] = Elems5[A, B, C, D, E]
    type Append[N] = Elems5[A, B, C, D, E]
    type Func[R] = (A, B, C, D, E) => R
    def linearization = _1.linearization ++ _2.linearization ++
                        _3.linearization ++ _4.linearization ++
                        _5.linearization
  }

  val empty = SimplePathSpec[Elems0](Elems0, Seq.empty)
}

final class PathSpecAndCond[Req, Elems <: PathSpec.Elems](
              path: PathSpec[Elems], cond: Elems#Prepend[Req]#Func[Boolean]) {
  def when[Resp](body: Elems#Prepend[Req]#Func[Resp])(
                 implicit ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]]) =
    CondRoute(path, cond, body)
  def whenDo[Resp](body: Elems#Prepend[Req]#Func[Resp])(
                   implicit ops: PathSpec.ElemsOps[Elems],
                            prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]]) =
    CondRoute(path, cond, body)
}
final class PathSpecAndGuard[Req, Elems <: PathSpec.Elems, E](
              path: PathSpec[Elems],
              guard: Elems#Prepend[Req]#Func[Option[E]]) {
  def when[Resp](body: Elems#Prepend[Req]#Append[E]#Func[Resp])(
                 implicit ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                          prepApOps: PathSpec.ElemsOps[
                                       Elems#Prepend[Req]#Append[E]]) =
    GuardedRoute(path, guard, body)
  def whenDo[Resp](body: Elems#Prepend[Req]#Append[E]#Func[Resp])(
                   implicit ops: PathSpec.ElemsOps[Elems],
                            prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                            prepApOps: PathSpec.ElemsOps[
                                         Elems#Prepend[Req]#Append[E]]) =
    GuardedRoute(path, guard, body)
}

sealed trait PathSpec[E <: PathSpec.Elems] {
  import PathSpec.ElemsOps

  final type Elems = E

  def linearization: Seq[Either[String, Pattern[String, _]]]
  def rest: Option[Pattern[String, _]]

  def when[Req, Resp](body: Elems#Prepend[Req]#Func[Resp])(
                      implicit ops: ElemsOps[Elems],
                               prepOps: ElemsOps[Elems#Prepend[Req]]) =
    SimpleRoute(this, body)
  def whenDo[Req, Resp](body: Elems#Prepend[Req]#Func[Resp])(
                        implicit ops: ElemsOps[Elems],
                                 prepOps: ElemsOps[Elems#Prepend[Req]]) =
    SimpleRoute(this, body)
  def onlyIf[Req](cond: Elems#Prepend[Req]#Func[Boolean]) =
    new PathSpecAndCond(this, cond)
  def guard[Req, G](guard: Elems#Prepend[Req]#Func[Option[G]]) =
    new PathSpecAndGuard(this, guard)
}
final case class SimplePathSpec[Elems <: PathSpec.Elems](
                   elems: Elems, suffix: Seq[String])
                 extends PathSpec[Elems] {
  def />(str: String) = SimplePathSpec(elems, suffix :+ str)
  def />[R](matcher: Pattern[String, R])(
            implicit witness: Elems#HasNext =:= PathSpec.TTrue,
                     ops: PathSpec.ElemsOps[Elems],
                     apOps: PathSpec.ElemsOps[Elems#Append[R]]) =
    SimplePathSpec(ops.appendElem(elems, PathSpec.Elem(suffix, matcher)),
                   Seq.empty)
  def /#[R](matcher: Pattern[String, R])(
            implicit witness: Elems#HasNext =:= PathSpec.TTrue) =
    RestPathSpec(elems, suffix, matcher)

  def linearization = elems.linearization ++ suffix.map(Left(_))
  def rest = None
}
final case class RestPathSpec[E <: PathSpec.Elems, L](
                   prefixElems: E, prefixSuffix: Seq[String],
                   pattern: Pattern[String, L])(
                   implicit witness: E#HasNext =:= PathSpec.TTrue)
                 extends PathSpec[E#Append[L]] {
  def linearization = prefixElems.linearization ++
                      prefixSuffix.map(Left(_))
  def rest = Some(pattern)
}
