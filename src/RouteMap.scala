/*
 * Copyright (C) 2011 Mikhail Vorozhtsov
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

trait RouteMap[K, Req, +Resp] {
  import RouteMap._

  protected def routesByKey: Map[K, Routes[Req, Resp]]

  def apply(req: Req, key: K, path: String): Option[() => Resp] =
    routesByKey.get(key).flatMap(rs => rs(req, path))
  def /:(str: String): RouteMap[K, Req, Resp] =
    MapBacked { routesByKey.map { case (key, rs) => key -> (str /: rs) } }
  def ++[Resp1 >: Resp](
        map: RouteMap[K, Req, Resp1]): RouteMap[K, Req, Resp1] = {
    val routes = this.routesByKey
    val mapRoutes = map.routesByKey
    MapBacked {
      (routes.keySet union mapRoutes.keySet).view.map { key =>
        key -> {
          routes.get(key) match {
            case Some(rs) =>
              mapRoutes.get(key).map(rs ++ _).getOrElse(rs)
            case None =>
              mapRoutes(key)
          }
        }
      } .toMap
    }
  }

  override def toString = {
    routesByKey.foldLeft("(route-map") { case (str, (key, rs)) =>
      str + "\n  (:" + key + "\n" +
              "     (" + rs + ")"
    } + ")"
  }
}

object RouteMap {
  private final class MapBacked[K, Req, +Resp](
                        protected val routesByKey: Map[K, Routes[Req, Resp]])
                      extends RouteMap[K, Req, Resp]
  private object MapBacked {
    def apply[K, Req, Resp](routes: Map[K, Routes[Req, Resp]]) =
      new MapBacked(routes)
  }
  def empty[K, Req, Resp]: RouteMap[K, Req, Resp] = MapBacked(Map.empty)
}

trait DeclRouteMap[K, Req, Resp] extends RouteMap[K, Req, Resp] {
  import DeclRouteMap._

  private var routesVar = Map[K, Routes[Req, Resp]]()
  protected final def routesByKey = routesVar

  protected final def routeDecl[Elems <: PathSpec.Elems](
                        key: K, spec: PathSpec[Elems]) =
    new SimpleRouteDecl(this, key, spec)
}

object DeclRouteMap {
  private def addRoute[K, Req, Resp](map: DeclRouteMap[K, Req, Resp],
                                     key: K, route: Route[Req, Resp]) {
    map.routesVar.get(key) match {
      case Some(rs) =>
        map.routesVar += (key -> (rs ++ route))
      case None =>
        map.routesVar += (key -> Routes(route))
    }
  }

  protected final class SimpleRouteDecl[K, Req, Resp, Elems <: PathSpec.Elems] private[DeclRouteMap] (
                          map: DeclRouteMap[K, Req, Resp],
                          key: K, spec: PathSpec[Elems]) {
    def apply[R](body: Elems#Prepend[Req]#Func[R])(
                 implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                          ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                          conv: R => Resp) =
      addRoute(map, key, spec when prepOps.andThen(body, conv))
    def onlyIf(cond: Elems#Prepend[Req]#Func[Boolean]) =
      new CondRouteDecl(map, key, spec, cond)
    def guard[G](guard: Elems#Prepend[Req]#Func[Option[G]]) =
      new GuardedRouteDecl(map, key, spec, guard)
  }
  protected final class CondRouteDecl[K, Req, Resp, Elems <: PathSpec.Elems] private[DeclRouteMap] (
                          map: DeclRouteMap[K, Req, Resp], key: K,
                          spec: PathSpec[Elems],
                          cond: Elems#Prepend[Req]#Func[Boolean]) {
    def apply[R](body: Elems#Prepend[Req]#Func[R])(
                 implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                          ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                          conv: R => Resp) =
      addRoute(map, key, spec onlyIf cond when prepOps.andThen(body, conv))
  }
  protected final class GuardedRouteDecl[K, Req, Resp, Elems <: PathSpec.Elems, G] private[DeclRouteMap] (
                          map: DeclRouteMap[K, Req, Resp], key: K,
                          spec: PathSpec[Elems],
                          guard: Elems#Prepend[Req]#Func[Option[G]]) {
    def apply[R](body: Elems#Prepend[Req]#Append[G]#Func[R])(
                 implicit hasNext: Elems#Prepend[Req]#HasNext =:=
                                   PathSpec.TTrue,
                          ops: PathSpec.ElemsOps[Elems],
                          prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                          prepApOps: PathSpec.ElemsOps[
                                       Elems#Prepend[Req]#Append[G]],
                          conv: R => Resp) =
      addRoute(map, key, spec guard guard when prepApOps.andThen(body, conv))
  }
}

