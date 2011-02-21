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
    def prependElem[H](head: Elem[H], elems: E0)(
                       implicit hasNext: E0#HasNext =:= TTrue): E0#Prepend[H]
    def appendElem[L](elems: E0, last: Elem[L])(
                      implicit hasNext: E0#HasNext =:= TTrue): E0#Append[L]
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E0#HasNext =:= TTrue):
                        E0#Prepend[H]#Args
    def appendArg[L](args: E0#Args, last: L)(
                     implicit hasNext: E0#HasNext =:= TTrue): E0#Append[L]#Args
  }
  object ElemsOps {
    implicit val elemsOps0 = PathSpec.ElemsOps0
    implicit def elemsOps1[A] = new PathSpec.ElemsOps1[A]
    implicit def elemsOps2[A1, A2] = new PathSpec.ElemsOps2[A1, A2]
    implicit def elemsOps3[A1, A2, A3] = new PathSpec.ElemsOps3[A1, A2, A3]
    implicit def elemsOps4[A1, A2, A3, A4] =
      new PathSpec.ElemsOps4[A1, A2, A3, A4]
    implicit def elemsOps5[A1, A2, A3, A4, A5] =
      new PathSpec.ElemsOps5[A1, A2, A3, A4, A5]
    implicit def elemsOps6[A1, A2, A3, A4, A5, A6] =
      new PathSpec.ElemsOps6[A1, A2, A3, A4, A5, A6]
    implicit def elemsOps7[A1, A2, A3, A4, A5, A6, A7] =
      new PathSpec.ElemsOps7[A1, A2, A3, A4, A5, A6, A7]
    implicit def elemsOps8[A1, A2, A3, A4, A5, A6, A7, A8] =
      new PathSpec.ElemsOps8[A1, A2, A3, A4, A5, A6, A7, A8]
  }

  sealed trait Elems {
    type Args
    type HasNext <: TBool
    type Prepend[_] <: Elems
    type Append[_] <: Elems
    type Func[+_]
    def linearization: Seq[Either[String, Pattern[String, _]]]
  }
  sealed class Elems0 extends Elems {
    type Args = Unit
    type HasNext = TTrue
    type Prepend[H] = Elems1[H]
    type Append[L] = Elems1[L]
    type Func[+R] = Unit => R
    def linearization = Seq.empty
  }
  val Elems0 = new Elems0
  object ElemsOps0 extends ElemsOps[Elems0] {
    def apply[R](f: Unit => R, args: Unit) = f()
    def andThen[R, R1](f: Unit => R, g: R => R1) = _ => g(f())
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) = Elems1(head)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) = Elems1(last)
    def prependArg[H](head: H, args: Unit)(
                      implicit hasNext: E#HasNext =:= TTrue) = head
    def appendArg[L](args: Unit, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) = last
  }
  final case class Elems1[A](_1: Elem[A]) extends Elems {
    type Args = A
    type HasNext = TTrue
    type Prepend[H] = Elems2[H, A]
    type Append[L] = Elems2[A, L]
    type Func[+R] = A => R
    def linearization = _1.linearization
  }
  final class ElemsOps1[A]() extends ElemsOps[Elems1[A]] {
    def apply[R](f: E#Func[R], args: E#Args) = f(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) = x => g(f(x))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems2(head, elems._1)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems2(elems._1, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args, last)
  }
  final case class Elems2[A1, A2](_1: Elem[A1], _2: Elem[A2]) extends Elems {
    type Args = (A1, A2)
    type HasNext = TTrue
    type Prepend[H] = Elems3[H, A1, A2]
    type Append[L] = Elems3[A1, A2, L]
    type Func[+R] = (A1, A2) => R
    def linearization = _1.linearization ++ _2.linearization
  }
  final class ElemsOps2[A1, A2]() extends ElemsOps[Elems2[A1, A2]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) = (x1, x2) => g(f(x1, x2))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems3(head, elems._1, elems._2)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems3(elems._1, elems._2, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args._1, args._2)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args._1, args._2, last)
  }
  final case class Elems3[A1, A2, A3](_1: Elem[A1], _2: Elem[A2], _3: Elem[A3])
                   extends Elems {
    type Args = (A1, A2, A3)
    type HasNext = TTrue
    type Prepend[H] = Elems4[H, A1, A2, A3]
    type Append[L] = Elems4[A1, A2, A3, L]
    type Func[+R] = (A1, A2, A3) => R
    def linearization =
      _1.linearization ++ _2.linearization ++ _3.linearization
  }
  final class ElemsOps3[A1, A2, A3]() extends ElemsOps[Elems3[A1, A2, A3]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x1, x2, x3) => g(f(x1, x2, x3))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems4(head, elems._1, elems._2, elems._3)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems4(elems._1, elems._2, elems._3, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args._1, args._2, args._3)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args._1, args._2, args._3, last)
  }
  final case class Elems4[A1, A2, A3, A4](
                     _1: Elem[A1], _2: Elem[A2], _3: Elem[A3], _4: Elem[A4])
                   extends Elems {
    type Args = (A1, A2, A3, A4)
    type HasNext = TTrue
    type Prepend[H] = Elems5[H, A1, A2, A3, A4]
    type Append[L] = Elems5[A1, A2, A3, A4, L]
    type Func[+R] = (A1, A2, A3, A4) => R
    def linearization =
      _1.linearization ++ _2.linearization ++ _3.linearization ++
      _4.linearization
  }
  final class ElemsOps4[A1, A2, A3, A4]
              extends ElemsOps[Elems4[A1, A2, A3, A4]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x1, x2, x3, x4) => g(f(x1, x2, x3, x4))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems5(head, elems._1, elems._2, elems._3, elems._4)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems5(elems._1, elems._2, elems._3, elems._4, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args._1, args._2, args._3, args._4)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args._1, args._2, args._3, args._4, last)
  }
  final case class Elems5[A1, A2, A3, A4, A5](
                     _1: Elem[A1], _2: Elem[A2], _3: Elem[A3], _4: Elem[A4],
                     _5: Elem[A5])
                   extends Elems {
    type Args = (A1, A2, A3, A4, A5)
    type HasNext = TTrue
    type Prepend[H] = Elems6[H, A1, A2, A3, A4, A5]
    type Append[L] = Elems6[A1, A2, A3, A4, A5, L]
    type Func[+R] = (A1, A2, A3, A4, A5) => R
    def linearization =
      _1.linearization ++ _2.linearization ++ _3.linearization ++
      _4.linearization ++ _5.linearization
  }
  final class ElemsOps5[A1, A2, A3, A4, A5]
              extends ElemsOps[Elems5[A1, A2, A3, A4, A5]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x1, x2, x3, x4, x5) => g(f(x1, x2, x3, x4, x5))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems6(head, elems._1, elems._2, elems._3, elems._4, elems._5)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems6(elems._1, elems._2, elems._3, elems._4, elems._5, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args._1, args._2, args._3, args._4, args._5)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args._1, args._2, args._3, args._4, args._5, last)
  }
  final case class Elems6[A1, A2, A3, A4, A5, A6](
                     _1: Elem[A1], _2: Elem[A2], _3: Elem[A3], _4: Elem[A4],
                     _5: Elem[A5], _6: Elem[A6])
                   extends Elems {
    type Args = (A1, A2, A3, A4, A5, A6)
    type HasNext = TTrue
    type Prepend[H] = Elems7[H, A1, A2, A3, A4, A5, A6]
    type Append[L] = Elems7[A1, A2, A3, A4, A5, A6, L]
    type Func[+R] = (A1, A2, A3, A4, A5, A6) => R
    def linearization =
      _1.linearization ++ _2.linearization ++ _3.linearization ++
      _4.linearization ++ _5.linearization ++ _6.linearization
  }
  final class ElemsOps6[A1, A2, A3, A4, A5, A6]
              extends ElemsOps[Elems6[A1, A2, A3, A4, A5, A6]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x1, x2, x3, x4, x5, x6) => g(f(x1, x2, x3, x4, x5, x6))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems7(head, elems._1, elems._2, elems._3, elems._4, elems._5, elems._6)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems7(elems._1, elems._2, elems._3, elems._4, elems._5, elems._6, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args._1, args._2, args._3, args._4, args._5, args._6)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args._1, args._2, args._3, args._4, args._5, args._6, last)
  }
  final case class Elems7[A1, A2, A3, A4, A5, A6, A7](
                     _1: Elem[A1], _2: Elem[A2], _3: Elem[A3], _4: Elem[A4],
                     _5: Elem[A5], _6: Elem[A6], _7: Elem[A7])
                   extends Elems {
    type Args = (A1, A2, A3, A4, A5, A6, A7)
    type HasNext = TTrue
    type Prepend[H] = Elems8[H, A1, A2, A3, A4, A5, A6, A7]
    type Append[L] = Elems8[A1, A2, A3, A4, A5, A6, A7, L]
    type Func[+R] = (A1, A2, A3, A4, A5, A6, A7) => R
    def linearization =
      _1.linearization ++ _2.linearization ++ _3.linearization ++
      _4.linearization ++ _5.linearization ++ _6.linearization ++
      _7.linearization
  }
  final class ElemsOps7[A1, A2, A3, A4, A5, A6, A7]
              extends ElemsOps[Elems7[A1, A2, A3, A4, A5, A6, A7]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x1, x2, x3, x4, x5, x6, x7) => g(f(x1, x2, x3, x4, x5, x6, x7))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      Elems8(head, elems._1, elems._2, elems._3, elems._4, elems._5, elems._6,
             elems._7)
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      Elems8(elems._1, elems._2, elems._3, elems._4, elems._5, elems._6,
             elems._7, last)
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      (head, args._1, args._2, args._3, args._4, args._5, args._6, args._7)
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      (args._1, args._2, args._3, args._4, args._5, args._6, args._7, last)
  }
  final case class Elems8[A1, A2, A3, A4, A5, A6, A7, A8](
                     _1: Elem[A1], _2: Elem[A2], _3: Elem[A3], _4: Elem[A4],
                     _5: Elem[A5], _6: Elem[A6], _7: Elem[A7], _8: Elem[A8])
                   extends Elems {
    type Args = (A1, A2, A3, A4, A5, A6, A7, A8)
    type HasNext = TFalse
    type Prepend[H] = Elems8[A1, A2, A3, A4, A5, A6, A7, A8]
    type Append[L] = Elems8[A1, A2, A3, A4, A5, A6, A7, A8]
    type Func[+R] = (A1, A2, A3, A4, A5, A6, A7, A8) => R
    def linearization =
      _1.linearization ++ _2.linearization ++ _3.linearization ++
      _4.linearization ++ _5.linearization ++ _6.linearization ++
      _7.linearization ++ _8.linearization
  }
  final class ElemsOps8[A1, A2, A3, A4, A5, A6, A7, A8]
              extends ElemsOps[Elems8[A1, A2, A3, A4, A5, A6, A7, A8]] {
    def apply[R](f: E#Func[R], args: E#Args) = f.tupled(args)
    def andThen[R, R1](f: E#Func[R], g: R => R1) =
      (x1, x2, x3, x4, x5, x6, x7, x8) => g(f(x1, x2, x3, x4, x5, x6, x7, x8))
    def prependElem[H](head: Elem[H], elems: E)(
                       implicit hasNext: E#HasNext =:= TTrue) =
      throw new UnsupportedOperationException
    def appendElem[L](elems: E, last: Elem[L])(
                      implicit hasNext: E#HasNext =:= TTrue) =
      throw new UnsupportedOperationException
    def prependArg[H](head: H, args: E#Args)(
                      implicit hasNext: E#HasNext =:= TTrue) =
      throw new UnsupportedOperationException
    def appendArg[L](args: E#Args, last: L)(
                     implicit hasNext: E#HasNext =:= TTrue) =
      throw new UnsupportedOperationException
  }

  val empty = SimplePathSpec[Elems0](Elems0, Seq.empty)
}

final class PathSpecAndCond[Req, Elems <: PathSpec.Elems](
              path: PathSpec[Elems], cond: Elems#Prepend[Req]#Func[Boolean]) {
  def when[Resp](body: Elems#Prepend[Req]#Func[Resp])(
                 implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                          ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]]) =
    CondRoute(path, cond, body)
  def whenDo[Resp](body: Elems#Prepend[Req]#Func[Resp])(
                   implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                            ops: PathSpec.ElemsOps[Elems],
                            prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]]) =
    CondRoute(path, cond, body)
}
final class PathSpecAndGuard[Req, Elems <: PathSpec.Elems, E](
              path: PathSpec[Elems],
              guard: Elems#Prepend[Req]#Func[Option[E]]) {
  def when[Resp](body: Elems#Prepend[Req]#Append[E]#Func[Resp])(
                 implicit hasNext: Elems#Prepend[Req]#HasNext =:=
                                   PathSpec.TTrue,
                          ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                          prepApOps: PathSpec.ElemsOps[
                                       Elems#Prepend[Req]#Append[E]]) =
    GuardedRoute(path, guard, body)
  def whenDo[Resp](body: Elems#Prepend[Req]#Append[E]#Func[Resp])(
                   implicit hasNext: Elems#Prepend[Req]#HasNext =:=
                                     PathSpec.TTrue,
                            ops: PathSpec.ElemsOps[Elems],
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
                      implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                               ops: ElemsOps[Elems],
                               prepOps: ElemsOps[Elems#Prepend[Req]]) =
    SimpleRoute(this, body)
  def whenDo[Req, Resp](body: Elems#Prepend[Req]#Func[Resp])(
                        implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                                 ops: ElemsOps[Elems],
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
            implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                     ops: PathSpec.ElemsOps[Elems],
                     apOps: PathSpec.ElemsOps[Elems#Append[R]]) =
    SimplePathSpec(ops.appendElem(elems, PathSpec.Elem(suffix, matcher)),
                   Seq.empty)
  def /#[R](matcher: Pattern[String, R])(
            implicit hasNext: Elems#HasNext =:= PathSpec.TTrue) =
    RestPathSpec(elems, suffix, matcher)

  def linearization = elems.linearization ++ suffix.map(Left(_))
  def rest = None
}
final case class RestPathSpec[E <: PathSpec.Elems, L](
                   prefixElems: E, prefixSuffix: Seq[String],
                   pattern: Pattern[String, L])(
                   implicit hasNext: E#HasNext =:= PathSpec.TTrue)
                 extends PathSpec[E#Append[L]] {
  def linearization = prefixElems.linearization ++
                      prefixSuffix.map(Left(_))
  def rest = Some(pattern)
}
