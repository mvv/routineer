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

package com.github.mvv.routineer.scalaz

import com.github.mvv.routineer._
import _root_.scalaz._

package object `package` {
  implicit def routineerRoutesMonoid[Req, Resp] =
    new Monoid[Routes[Req, Resp]] {
      @inline
      def zero = Routes.empty[Req, Resp]
      @inline
      def append(rs1: Routes[Req, Resp], rs2: => Routes[Req, Resp]) =
        rs1 ++ rs2
    }
  implicit def routineerRouteMapMonoid[K, Req, Resp] =
    new Monoid[RouteMap[K, Req, Resp]] {
      @inline
      def zero = RouteMap.empty[K, Req, Resp]
      @inline
      def append(rs1: RouteMap[K, Req, Resp],
                 rs2: => RouteMap[K, Req, Resp]) =
        rs1 ++ rs2
    }
  implicit object routineerPatternArrow extends Arrow[Pattern] {
    @inline
    def id[A] = Pattern.map(identity)
    @inline
    def compose[A, B, C](f: Pattern[B, C], g: Pattern[A, B]) = g >>> f
    @inline
    def arr[A, B](f: A => B) = Pattern.map(f)
    @inline
    def first[A, B, C](pat: Pattern[A, B]) = pat *** Pattern.map(identity[C])
    @inline
    override def second[A, B, C](pat: Pattern[A, B]) =
      Pattern.map(identity[C]) *** pat
  }
}
