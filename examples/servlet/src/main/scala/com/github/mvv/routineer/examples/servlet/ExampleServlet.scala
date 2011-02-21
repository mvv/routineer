package com.github.mvv.routineer.examples.servlet

import javax.servlet.http._
import com.github.mvv.routineer._

trait Handler
      extends DeclRouteMap[String, (HttpServletRequest, HttpServletResponse),
                           Unit] {
  protected final def get() = routeDecl("GET", PathSpec.empty)
  protected final def get[E <: PathSpec.Elems](spec: PathSpec[E]) =
    routeDecl("GET", spec)
  protected final def post() = routeDecl("POST", PathSpec.empty)
  protected final def post[E <: PathSpec.Elems](spec: PathSpec[E]) =
    routeDecl("POST", spec)

  protected final def output(resp: HttpServletResponse, content: String) {
    resp.getWriter.append(content).close
  }
}

object EchoHandler extends Handler {
  get("echo" /> *) { case ((req, resp), str) =>
    output(resp, str)
  }

  get("echo-rest" /# *) { (rr, str) =>
    output(rr._2, str)
  }

  get("echo-int" /> IntP) { (rr, i) =>
    output(rr._2, i.toString)
  }
}

object CondHandler extends Handler {
  get(* /> (IntP >>> Positive[Int]))
      .onlyIf { (_, str, len) => str.length <= len } { (rr, str, len) =>
    output(rr._2, "string \"%s\" has length <= %d" format(str, len))
  }
}

object GuardHandler extends Handler {
  get(*) .guard { (_, str) => Some(str.count(_ == 'a')).filter(_ > 0) } {
    (rr, str, as) =>
      output(rr._2, "string \"%s\" has %d 'a'(s)" format (str, as))
  }
}

class ExampleServlet extends HttpServlet {
  lazy val routes = "cond" /: {
                      CondHandler ++
                      GuardHandler
                    } ++
                    EchoHandler

  override protected def service(req: HttpServletRequest,
                                 resp: HttpServletResponse) {
    routes((req, resp), req.getMethod, req.getRequestURI) match {
      case Some(code) =>
        resp.setContentType("text/plain")
        resp.setCharacterEncoding("UTF-8")
        code()
      case None =>
        resp.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
  }
}
