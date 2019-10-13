/*
 * Copyright (C) 2010-2011 Mikhail Vorozhtsov
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

package com.github.mvv.routineer.tests

import com.github.mvv.routineer._
import org.specs2.mutable._

object AppendSpec extends Specification {
  "Appending a route to itself must raise an error" in {
    val rs = Routes[Any, Any](PathSpec.empty when (r => r))
    (rs ++ rs) must throwAn[RouteOvershadowedException[_, _]]
  }
}
