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

package com.github.mvv.routineer.syntax

import com.github.mvv.routineer.{Args, EqualsP, RoutePattern, ValueCheck, ValuePattern}

import scala.language.implicitConversions

/** Essential implicit convertions. */
trait RoutineerImplicits {
  implicit def stringToValueCheck(str: String): ValueCheck[String] =
    EqualsP(str).check
  implicit def stringToRoutePattern(str: String): RoutePattern.PartialPath[Args._0] =
    RoutePattern.Root /> EqualsP(str).check
  implicit def checkToRoutePattern[R](check: ValueCheck[String]): RoutePattern.PartialPath[Args._0] =
    RoutePattern.Root /> check
  implicit def forceCheckToRoutePattern[R](check: ValueCheck.Force[String]): RoutePattern.PartialPath[Args._0] =
    RoutePattern.Root /> check
  implicit def patternToRoutePattern[R](pattern: ValuePattern[String, R]): RoutePattern.PartialPath[Args._1[R]] =
    RoutePattern.Root /> pattern
  implicit def forcePatternToRoutePattern[R](
      pattern: ValuePattern.Force[String, R]
  ): RoutePattern.PartialPath[Args._1[R]] =
    RoutePattern.Root /> pattern
}

object `package` extends RoutineerImplicits {
  val Root: RoutePattern.PartialPath[Args._0] = RoutePattern.Root
}
