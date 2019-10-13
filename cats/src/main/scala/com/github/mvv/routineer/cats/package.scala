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

import com.github.mvv.routineer._
import _root_.cats.Monoid
import _root_.cats.arrow.Arrow

package object cats {
  implicit def routineerRoutesMonoid[Req, Resp]: Monoid[Routes[Req, Resp]] =
    new Monoid[Routes[Req, Resp]] {
      override def empty: Routes[Req, Resp] = Routes.empty[Req, Resp]
      override def combine(rs1: Routes[Req, Resp], rs2: Routes[Req, Resp]): Routes[Req, Resp] =
        rs1 ++ rs2
    }
  implicit def routineerRouteMapMonoid[K, Req, Resp]: Monoid[RouteMap[K, Req, Resp]] =
    new Monoid[RouteMap[K, Req, Resp]] {
      override def empty: RouteMap[K, Req, Resp] = RouteMap.empty[K, Req, Resp]
      override def combine(rs1: RouteMap[K, Req, Resp], rs2: RouteMap[K, Req, Resp]): RouteMap[K, Req, Resp] =
        rs1 ++ rs2
    }
  implicit object routineerPatternArrow extends Arrow[Pattern] {
    override def lift[A, B](f: A => B): Pattern[A, B] = Pattern.map(f)
    override def compose[A, B, C](f: Pattern[B, C], g: Pattern[A, B]): Pattern[A, C] = g >>> f
    override def first[A, B, C](pat: Pattern[A, B]): Pattern[(A, C), (B, C)] = pat *** id[C]
    override def second[A, B, C](pat: Pattern[A, B]): Pattern[(C, A), (C, B)] = id[C] *** pat
  }
}
