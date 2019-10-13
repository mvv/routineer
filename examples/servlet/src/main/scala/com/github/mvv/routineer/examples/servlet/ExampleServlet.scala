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

package com.github.mvv.routineer.examples.servlet

import javax.servlet.http._
import com.github.mvv.routineer._

trait Handler extends DeclRouteMap[String, HttpServletRequest, String] {
  final protected def get() = routeDecl("GET", PathSpec.empty)
  final protected def get[E <: PathSpec.Elems](spec: PathSpec[E]) =
    routeDecl("GET", spec)
  final protected def post() = routeDecl("POST", PathSpec.empty)
  final protected def post[E <: PathSpec.Elems](spec: PathSpec[E]) =
    routeDecl("POST", spec)
}

object EchoHandler extends Handler {
  get("echo" /> *) { (_, str) =>
    str
  }
  get("echo-rest" /# *) { (_, str) =>
    str
  }
  get("echo-int" /> IntP) { (_, i) =>
    i.toString
  }
}

object CondHandler extends Handler {
  get(* /> (IntP >>> PositiveP[Int])).onlyIf { (_, str, len) =>
    str.length <= len
  } { (req, str, len) =>
    "%s: string \"%s\" has length <= %d" format (req.getRequestURI, str, len)
  }
}

object GuardHandler extends Handler {
  get(*).guard { (_, str) =>
    Some(str.count(_ == 'a')).filter(_ > 0)
  } { (req, str, as) =>
    "%s: string \"%s\" has %d 'a'(s)" format (req.getRequestURI, str, as)
  }
}

class ExampleServlet extends HttpServlet {
  lazy val routes = "cond" /: {
    CondHandler ++
      GuardHandler
  } ++
    EchoHandler

  override protected def service(req: HttpServletRequest, resp: HttpServletResponse): Unit =
    routes(req, req.getMethod, req.getRequestURI) match {
      case Some(code) =>
        resp.setContentType("text/plain")
        resp.setCharacterEncoding("UTF-8")
        resp.getWriter.append(code()).close()
      case None =>
        resp.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
}
