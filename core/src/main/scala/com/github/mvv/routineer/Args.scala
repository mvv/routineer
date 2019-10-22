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

sealed trait Args {
  type Fn[+ _]
  def through[R](f: Fn[R]): R

  type Prepend[H] <: Args
  type Append[L] <: Args
}

object Args {
  sealed trait Growable extends Args {
    def prepend[H](head: H): Prepend[H]
    def append[L](last: L): Append[L]
  }

  final class _0 extends Growable {
    override type Fn[+R] = () => R
    override def through[R](f: Fn[R]): R = f()

    override type Prepend[H] = _1[H]
    override type Append[L] = _1[L]
    override def prepend[H](head: H): Prepend[H] = new _1(head)
    override def append[H](last: H): Append[H] = new _1(last)
  }
  val _0: _0 = new _0
  final class _1[A] private[Args] (_1: A) extends Growable {
    override type Fn[+R] = A => R
    override def through[R](f: Fn[R]): R = f(_1)

    override type Prepend[H] = _2[H, A]
    override type Append[L] = _2[A, L]
    override def prepend[H](head: H): Prepend[H] = new _2(head, _1)
    override def append[L](last: L): Append[L] = new _2(_1, last)
  }
  final class _2[A1, A2] private[Args] (_1: A1, _2: A2) extends Growable {
    override type Fn[+R] = (A1, A2) => R
    override def through[R](f: Fn[R]): R = f(_1, _2)

    override type Prepend[H] = _3[H, A1, A2]
    override type Append[L] = _3[A1, A2, L]
    override def prepend[H](head: H): Prepend[H] = new _3(head, _1, _2)
    override def append[L](last: L): Append[L] = new _3(_1, _2, last)
  }
  final class _3[A1, A2, A3] private[Args] (_1: A1, _2: A2, _3: A3) extends Growable {
    override type Fn[+R] = (A1, A2, A3) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3)

    override type Prepend[H] = _4[H, A1, A2, A3]
    override type Append[L] = _4[A1, A2, A3, L]
    override def prepend[H](head: H): Prepend[H] = new _4(head, _1, _2, _3)
    override def append[L](last: L): Append[L] = new _4(_1, _2, _3, last)
  }
  final class _4[A1, A2, A3, A4] private[Args] (_1: A1, _2: A2, _3: A3, _4: A4) extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4)

    override type Prepend[H] = _5[H, A1, A2, A3, A4]
    override type Append[L] = _5[A1, A2, A3, A4, L]
    override def prepend[H](head: H): Prepend[H] = new _5(head, _1, _2, _3, _4)
    override def append[L](last: L): Append[L] = new _5(_1, _2, _3, _4, last)
  }
  final class _5[A1, A2, A3, A4, A5] private[Args] (_1: A1, _2: A2, _3: A3, _4: A4, _5: A5) extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5)

    override type Prepend[H] = _6[H, A1, A2, A3, A4, A5]
    override type Append[L] = _6[A1, A2, A3, A4, A5, L]
    override def prepend[H](head: H): Prepend[H] = new _6(head, _1, _2, _3, _4, _5)
    override def append[L](last: L): Append[L] = new _6(_1, _2, _3, _4, _5, last)
  }
  final class _6[A1, A2, A3, A4, A5, A6] private[Args] (_1: A1, _2: A2, _3: A3, _4: A4, _5: A5, _6: A6)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6)

    override type Prepend[H] = _7[H, A1, A2, A3, A4, A5, A6]
    override type Append[L] = _7[A1, A2, A3, A4, A5, A6, L]
    override def prepend[H](head: H): Prepend[H] = new _7(head, _1, _2, _3, _4, _5, _6)
    override def append[L](last: L): Append[L] = new _7(_1, _2, _3, _4, _5, _6, last)
  }
  final class _7[A1, A2, A3, A4, A5, A6, A7] private[Args] (_1: A1, _2: A2, _3: A3, _4: A4, _5: A5, _6: A6, _7: A7)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7)

    override type Prepend[H] = _8[H, A1, A2, A3, A4, A5, A6, A7]
    override type Append[L] = _8[A1, A2, A3, A4, A5, A6, A7, L]
    override def prepend[H](head: H): Prepend[H] = new _8(head, _1, _2, _3, _4, _5, _6, _7)
    override def append[L](last: L): Append[L] = new _8(_1, _2, _3, _4, _5, _6, _7, last)
  }
  final class _8[A1, A2, A3, A4, A5, A6, A7, A8] private[Args] (_1: A1,
                                                                _2: A2,
                                                                _3: A3,
                                                                _4: A4,
                                                                _5: A5,
                                                                _6: A6,
                                                                _7: A7,
                                                                _8: A8)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8)

    override type Prepend[H] = _9[H, A1, A2, A3, A4, A5, A6, A7, A8]
    override type Append[L] = _9[A1, A2, A3, A4, A5, A6, A7, A8, L]
    override def prepend[H](head: H): Prepend[H] = new _9(head, _1, _2, _3, _4, _5, _6, _7, _8)
    override def append[L](last: L): Append[L] = new _9(_1, _2, _3, _4, _5, _6, _7, _8, last)
  }
  final class _9[A1, A2, A3, A4, A5, A6, A7, A8, A9] private[Args] (_1: A1,
                                                                    _2: A2,
                                                                    _3: A3,
                                                                    _4: A4,
                                                                    _5: A5,
                                                                    _6: A6,
                                                                    _7: A7,
                                                                    _8: A8,
                                                                    _9: A9)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9)

    override type Prepend[H] = _10[H, A1, A2, A3, A4, A5, A6, A7, A8, A9]
    override type Append[L] = _10[A1, A2, A3, A4, A5, A6, A7, A8, A9, L]
    override def prepend[H](head: H): Prepend[H] = new _10(head, _1, _2, _3, _4, _5, _6, _7, _8, _9)
    override def append[L](last: L): Append[L] = new _10(_1, _2, _3, _4, _5, _6, _7, _8, _9, last)
  }
  final class _10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] private[Args] (_1: A1,
                                                                          _2: A2,
                                                                          _3: A3,
                                                                          _4: A4,
                                                                          _5: A5,
                                                                          _6: A6,
                                                                          _7: A7,
                                                                          _8: A8,
                                                                          _9: A9,
                                                                          _10: A10)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    override type Prepend[H] = _11[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
    override type Append[L] = _11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, L]
    override def prepend[H](head: H): Prepend[H] = new _11(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10)
    override def append[L](last: L): Append[L] = new _11(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, last)
  }
  final class _11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] private[Args] (_1: A1,
                                                                               _2: A2,
                                                                               _3: A3,
                                                                               _4: A4,
                                                                               _5: A5,
                                                                               _6: A6,
                                                                               _7: A7,
                                                                               _8: A8,
                                                                               _9: A9,
                                                                               _10: A10,
                                                                               _11: A11)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)

    override type Prepend[H] = _12[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
    override type Append[L] = _12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, L]
    override def prepend[H](head: H): Prepend[H] = new _12(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)
    override def append[L](last: L): Append[L] = new _12(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, last)
  }
  final class _12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] private[Args] (_1: A1,
                                                                                    _2: A2,
                                                                                    _3: A3,
                                                                                    _4: A4,
                                                                                    _5: A5,
                                                                                    _6: A6,
                                                                                    _7: A7,
                                                                                    _8: A8,
                                                                                    _9: A9,
                                                                                    _10: A10,
                                                                                    _11: A11,
                                                                                    _12: A12)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)

    override type Prepend[H] = _13[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
    override type Append[L] = _13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, L]
    override def prepend[H](head: H): Prepend[H] = new _13(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)
    override def append[L](last: L): Append[L] = new _13(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, last)
  }
  final class _13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] private[Args] (_1: A1,
                                                                                         _2: A2,
                                                                                         _3: A3,
                                                                                         _4: A4,
                                                                                         _5: A5,
                                                                                         _6: A6,
                                                                                         _7: A7,
                                                                                         _8: A8,
                                                                                         _9: A9,
                                                                                         _10: A10,
                                                                                         _11: A11,
                                                                                         _12: A12,
                                                                                         _13: A13)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13)

    override type Prepend[H] = _14[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
    override type Append[L] = _14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, L]
    override def prepend[H](head: H): Prepend[H] = new _14(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13)
    override def append[L](last: L): Append[L] = new _14(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, last)
  }
  final class _14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] private[Args] (_1: A1,
                                                                                              _2: A2,
                                                                                              _3: A3,
                                                                                              _4: A4,
                                                                                              _5: A5,
                                                                                              _6: A6,
                                                                                              _7: A7,
                                                                                              _8: A8,
                                                                                              _9: A9,
                                                                                              _10: A10,
                                                                                              _11: A11,
                                                                                              _12: A12,
                                                                                              _13: A13,
                                                                                              _14: A14)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14)

    override type Prepend[H] = _15[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
    override type Append[L] = _15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, L]
    override def prepend[H](head: H): Prepend[H] =
      new _15(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14)
    override def append[L](last: L): Append[L] =
      new _15(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, last)
  }
  final class _15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] private[Args] (_1: A1,
                                                                                                   _2: A2,
                                                                                                   _3: A3,
                                                                                                   _4: A4,
                                                                                                   _5: A5,
                                                                                                   _6: A6,
                                                                                                   _7: A7,
                                                                                                   _8: A8,
                                                                                                   _9: A9,
                                                                                                   _10: A10,
                                                                                                   _11: A11,
                                                                                                   _12: A12,
                                                                                                   _13: A13,
                                                                                                   _14: A14,
                                                                                                   _15: A15)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)

    override type Prepend[H] = _16[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
    override type Append[L] = _16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, L]
    override def prepend[H](head: H): Prepend[H] =
      new _16(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)
    override def append[L](last: L): Append[L] =
      new _16(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, last)
  }
  final class _16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] private[Args] (_1: A1,
                                                                                                        _2: A2,
                                                                                                        _3: A3,
                                                                                                        _4: A4,
                                                                                                        _5: A5,
                                                                                                        _6: A6,
                                                                                                        _7: A7,
                                                                                                        _8: A8,
                                                                                                        _9: A9,
                                                                                                        _10: A10,
                                                                                                        _11: A11,
                                                                                                        _12: A12,
                                                                                                        _13: A13,
                                                                                                        _14: A14,
                                                                                                        _15: A15,
                                                                                                        _16: A16)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16)

    override type Prepend[H] = _17[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
    override type Append[L] = _17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, L]
    override def prepend[H](head: H): Prepend[H] =
      new _17(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16)
    override def append[L](last: L): Append[L] =
      new _17(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, last)
  }
  final class _17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] private[Args] (_1: A1,
                                                                                                             _2: A2,
                                                                                                             _3: A3,
                                                                                                             _4: A4,
                                                                                                             _5: A5,
                                                                                                             _6: A6,
                                                                                                             _7: A7,
                                                                                                             _8: A8,
                                                                                                             _9: A9,
                                                                                                             _10: A10,
                                                                                                             _11: A11,
                                                                                                             _12: A12,
                                                                                                             _13: A13,
                                                                                                             _14: A14,
                                                                                                             _15: A15,
                                                                                                             _16: A16,
                                                                                                             _17: A17)
      extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => R
    override def through[R](f: Fn[R]): R = f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17)

    override type Prepend[H] = _18[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
    override type Append[L] = _18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, L]
    override def prepend[H](head: H): Prepend[H] =
      new _18(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17)
    override def append[L](last: L): Append[L] =
      new _18(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, last)
  }
  final class _18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] private[Args] (
      _1: A1,
      _2: A2,
      _3: A3,
      _4: A4,
      _5: A5,
      _6: A6,
      _7: A7,
      _8: A8,
      _9: A9,
      _10: A10,
      _11: A11,
      _12: A12,
      _13: A13,
      _14: A14,
      _15: A15,
      _16: A16,
      _17: A17,
      _18: A18
  ) extends Growable {
    override type Fn[+R] = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => R
    override def through[R](f: Fn[R]): R =
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18)

    override type Prepend[H] = _19[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
    override type Append[L] = _19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, L]
    override def prepend[H](head: H): Prepend[H] =
      new _19(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18)
    override def append[L](last: L): Append[L] =
      new _19(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, last)
  }
  final class _19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] private[Args] (
      _1: A1,
      _2: A2,
      _3: A3,
      _4: A4,
      _5: A5,
      _6: A6,
      _7: A7,
      _8: A8,
      _9: A9,
      _10: A10,
      _11: A11,
      _12: A12,
      _13: A13,
      _14: A14,
      _15: A15,
      _16: A16,
      _17: A17,
      _18: A18,
      _19: A19
  ) extends Growable {
    override type Fn[+R] =
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => R
    override def through[R](f: Fn[R]): R =
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19)

    override type Prepend[H] =
      _20[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
    override type Append[L] =
      _20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, L]
    override def prepend[H](head: H): Prepend[H] =
      new _20(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19)
    override def append[L](last: L): Append[L] =
      new _20(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, last)
  }
  final class _20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] private[Args] (
      _1: A1,
      _2: A2,
      _3: A3,
      _4: A4,
      _5: A5,
      _6: A6,
      _7: A7,
      _8: A8,
      _9: A9,
      _10: A10,
      _11: A11,
      _12: A12,
      _13: A13,
      _14: A14,
      _15: A15,
      _16: A16,
      _17: A17,
      _18: A18,
      _19: A19,
      _20: A20
  ) extends Growable {
    override type Fn[+R] =
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => R
    override def through[R](f: Fn[R]): R =
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20)

    override type Prepend[H] =
      _21[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
    override type Append[L] =
      _21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, L]
    override def prepend[H](head: H): Prepend[H] =
      new _21(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20)
    override def append[L](last: L): Append[L] =
      new _21(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, last)
  }
  final class _21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] private[Args] (
      _1: A1,
      _2: A2,
      _3: A3,
      _4: A4,
      _5: A5,
      _6: A6,
      _7: A7,
      _8: A8,
      _9: A9,
      _10: A10,
      _11: A11,
      _12: A12,
      _13: A13,
      _14: A14,
      _15: A15,
      _16: A16,
      _17: A17,
      _18: A18,
      _19: A19,
      _20: A20,
      _21: A21
  ) extends Growable {
    override type Fn[+R] =
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => R
    override def through[R](f: Fn[R]): R =
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21)

    override type Prepend[H] =
      _22[H, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
    override type Append[L] =
      _22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, L]
    override def prepend[H](head: H): Prepend[H] =
      new _22(head, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21)
    override def append[L](last: L): Append[L] =
      new _22(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, last)
  }
  final class _22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] private[Args] (
      _1: A1,
      _2: A2,
      _3: A3,
      _4: A4,
      _5: A5,
      _6: A6,
      _7: A7,
      _8: A8,
      _9: A9,
      _10: A10,
      _11: A11,
      _12: A12,
      _13: A13,
      _14: A14,
      _15: A15,
      _16: A16,
      _17: A17,
      _18: A18,
      _19: A19,
      _20: A20,
      _21: A21,
      _22: A22
  ) extends Args {
    override type Fn[+R] =
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => R
    override def through[R](f: Fn[R]): R =
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22)

    override type Prepend[H] =
      _22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
    override type Append[L] =
      _22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  }
}
