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

case class RouteOvershadowedException[Req, Resp](
             route: Route[Req, Resp], by: Route[Req, Resp])
           extends Exception {
  override def getMessage =
    "Route [ " + route + " ] is overshadowed by route [ " + by + " ]"
}

sealed trait Route[Req, +Resp] extends Routes.NonEmpty[Req, Resp] {
  type RouteElems <: PathSpec.Elems
  final type RouteArgs = RouteElems#Prepend[Req]#Args

  val spec: PathSpec[RouteElems]
  final val specLinearization = spec.linearization
  def isGuarded: Boolean
  def checkGuard(args: RouteArgs): Option[() => Resp]

  private[routineer] val straight: Routes[Req, Resp] =
    Routes.Straight(specLinearization, this)

  final def ++[Resp1 >: Resp](routes: Routes[Req, Resp1]) = straight ++ routes
  protected[routineer] def apply(evaluator: Routes.PatternEvaluator,
                                 path: Routes.Path, args: Seq[Any]) =
    straight(evaluator, path, args)

  override def toString = {
    val specStr =
      if (specLinearization.isEmpty) {
        spec.rest match {
          case Some(pat) => "/# " + pat.toString
          case None => "<empty>"
        }
      } else {
        val linearStr = specLinearization.map {
          case Left(str) => str
          case Right(pat) => pat.toString
        } .mkString(" /> ")
        spec.rest match {
          case Some(pat) => linearStr + " /# " + pat.toString
          case None => linearStr
        }
      }
    if (isGuarded)
      specStr + " (guarded)"
    else
      specStr
  }
}
final case class SimpleRoute[Req, +Resp, Elems <: PathSpec.Elems](
                   spec: PathSpec[Elems],
                   body: Elems#Prepend[Req]#Func[Resp])(
                   implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                            ops: PathSpec.ElemsOps[Elems#Prepend[Req]])
                 extends Route[Req, Resp] {
  type RouteElems = Elems
  def isGuarded = false
  def checkGuard(args: RouteArgs) =
    Some(() => ops.apply(body, args))
}
final case class CondRoute[Req, +Resp, Elems <: PathSpec.Elems](
                   spec: PathSpec[Elems],
                   cond: Elems#Prepend[Req]#Func[Boolean],
                   body: Elems#Prepend[Req]#Func[Resp])(
                   implicit hasNext: Elems#HasNext =:= PathSpec.TTrue,
                            ops: PathSpec.ElemsOps[Elems#Prepend[Req]])
                 extends Route[Req, Resp] {
  type RouteElems = Elems
  def isGuarded = true
  def checkGuard(args: RouteArgs) = {
    if (ops.apply(cond, args))
      Some(() => ops.apply(body, args))
    else
      None
  }
}
final case class GuardedRoute[Req, +Resp, Elems <: PathSpec.Elems, G](
                   spec: PathSpec[Elems],
                   guard: Elems#Prepend[Req]#Func[Option[G]],
                   body: Elems#Prepend[Req]#Append[G]#Func[Resp])(
                   implicit hasNext: Elems#Prepend[Req]#HasNext =:=
                                     PathSpec.TTrue,
                            prepOps: PathSpec.ElemsOps[Elems#Prepend[Req]],
                            prepApOps: PathSpec.ElemsOps[
                                         Elems#Prepend[Req]#Append[G]])
                 extends Route[Req, Resp] {
  type RouteElems = Elems
  def isGuarded = true
  def checkGuard(args: RouteArgs) = {
    prepOps.apply(guard, args).map { extra =>
      () => prepApOps.apply(body, prepOps.appendArg(args, extra))
    }
  }
}

sealed trait Routes[Req, +Resp] {
  def isEmpty: Boolean
  def /:(str: String): Routes[Req, Resp]
  def ++[Resp1 >: Resp](routes: Routes[Req, Resp1]): Routes[Req, Resp1]
  protected[routineer] def apply(evaluator: Routes.PatternEvaluator,
                                 path: Routes.Path,
                                 args: Seq[Any]): Option[() => Resp]
  final def apply(req: Req, path: String): Option[() => Resp] =
    apply(new Routes.PatternEvaluator, Routes.Path.parse(path), List(req))
}
object Routes {
  private def seqToTuple(seq: Seq[Any]) = {
    val size = seq.size
    if (size == 0)
      ()
    else if (size == 1)
      seq.head
    else if (size == 2) {
      var tail = seq
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2)
    } else if (size == 3) {
      var tail = seq
      val _3 = tail.head
      tail = tail.tail
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2, _3)
    } else if (size == 4) {
      var tail = seq
      val _4 = tail.head
      tail = tail.tail
      val _3 = tail.head
      tail = tail.tail
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2, _3, _4)
    } else if (size == 5) {
      var tail = seq
      val _5 = tail.head
      tail = tail.tail
      val _4 = tail.head
      tail = tail.tail
      val _3 = tail.head
      tail = tail.tail
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2, _3, _4, _5)
    } else if (size == 6) {
      var tail = seq
      val _6 = tail.head
      tail = tail.tail
      val _5 = tail.head
      tail = tail.tail
      val _4 = tail.head
      tail = tail.tail
      val _3 = tail.head
      tail = tail.tail
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2, _3, _4, _5, _6)
    } else if (size == 7) {
      var tail = seq
      val _7 = tail.head
      tail = tail.tail
      val _6 = tail.head
      tail = tail.tail
      val _5 = tail.head
      tail = tail.tail
      val _4 = tail.head
      tail = tail.tail
      val _3 = tail.head
      tail = tail.tail
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2, _3, _4, _5, _6, _7)
    } else if (size == 8) {
      var tail = seq
      val _8 = tail.head
      tail = tail.tail
      val _7 = tail.head
      tail = tail.tail
      val _6 = tail.head
      tail = tail.tail
      val _5 = tail.head
      tail = tail.tail
      val _4 = tail.head
      tail = tail.tail
      val _3 = tail.head
      tail = tail.tail
      val _2 = tail.head
      tail = tail.tail
      val _1 = tail.head
      (_1, _2, _3, _4, _5, _6, _7, _8)
    }
  }

  sealed trait Path {
    def isEmpty: Boolean
  }
  object Path {
    object Empty extends Path {
      def isEmpty = true
    }
    sealed trait NonEmpty extends Path {
      final def isEmpty = false
      def head: String
      def string: String
      def tail: Path
    }
    final case class End(head: String) extends NonEmpty {
      def string = head
      def tail = Empty
    }
    final class Cons(val head: String, lazyTail: => Path, val string: String)
                extends NonEmpty {
      private var evaluatedTail: Path = null
      def tail = {
        if (evaluatedTail == null)
          evaluatedTail = lazyTail
        evaluatedTail
      }
    }

    def parse(str: String): Path = {
      val seps = str.prefixLength(_ == '/')
      if (str.length == seps)
        Empty
      else {
        val i = str.indexOf('/', seps)
        if (i < 0)
          End(str.substring(seps))
        else
          new Cons(str.substring(seps, i),
                   parse(str.substring(i + 1)),
                   str.substring(seps))
      }
    }
  }

  final class PatternEvaluator() {
    private var cache: Map[(String, Pattern[String, _]),
                           Option[Any]] = Map.empty
    def eval[R](pattern: Pattern[String, R], str: String): Option[R] =
      cache.get((str, pattern)) match {
        case Some(result) => result.asInstanceOf[Option[R]]
        case None =>
          val result = pattern.matches(str)
          cache += ((str, pattern) -> result)
          result
      }
  }

  final class Empty[Req, +Resp]() extends Routes[Req, Resp] {
    def isEmpty = true
    def /:(str: String) = this
    def ++[Resp1 >: Resp](routes: Routes[Req, Resp1]) = routes
    protected[routineer] def apply(evaluator: PatternEvaluator,
                                   path: Path, args: Seq[Any]) = None
  }

  def empty[Req, Resp](): Routes[Req, Resp] = new Empty[Req, Resp]
  def apply[Req, Resp](routes: Route[Req, Resp]*): Routes[Req, Resp] =
    routes.foldLeft(empty[Req, Resp])(_ ++ _)

  sealed trait NonEmpty[Req, +Resp] extends Routes[Req, Resp] {
    final def isEmpty = true
    final def /:(str: String) = new Prefixed(str, this)
  }

  final case class Prefixed[Req, +Resp](
                     prefix: String, routes: Routes[Req, Resp])
                   extends NonEmpty[Req, Resp] {
    def ++[Resp1 >: Resp](routes1: Routes[Req, Resp1]) = routes1 match {
      case _: Empty[_, _] => this
      case route: Route[_, _] =>
        this ++ route.asInstanceOf[Route[Req, Resp1]].straight
      case Prefixed(prefix1, _routes1: Routes[_, _]) =>
        val routes1 = _routes1.asInstanceOf[Routes[Req, Resp1]]
        if (prefix1 == prefix)
          Prefixed(prefix, routes ++ routes1)
        else
          Choice(stringMap = Map(prefix -> routes, prefix1 -> routes1))
      case routes1 @ Straight(prefix1, end1) => prefix1.headOption match {
        case Some(Left(str)) =>
          if (str == prefix)
            Prefixed(prefix, routes ++ routes1.step)
          else
            Choice(stringMap = Map(prefix -> routes,
                                   str -> routes1.step))
        case Some(Right(pat)) =>
          val pair = pat -> routes1.step
          Choice(stringMap = Map(prefix -> routes),
                 patternMap = Map(pair), patternSeq = Vector(pair))
        case None => end1.spec.rest match {
          case Some(pat1) =>
            val pair1 = pat1 -> end1
            Choice(stringMap = Map(prefix -> routes),
                   restMap = Map(pair1), restSeq = Vector(pair1))
          case None =>
            Choice(stringMap = Map(prefix -> routes),
                   ends = Vector(end1))
        }
      }
      case _choice: Choice[_, _] =>
        val choice = _choice.asInstanceOf[Choice[Req, Resp1]]
        choice.stringMap.get(prefix) match {
          case Some(routes1) =>
            choice.copy(stringMap = choice.stringMap +
                                    (prefix -> (routes ++ routes1)))
          case None =>
            choice.copy(stringMap = choice.stringMap + (prefix -> routes))
        }
    }
    protected[routineer] def apply(evaluator: PatternEvaluator,
                                   path: Path, args: Seq[Any]) =
      path match {
        case path: Path.NonEmpty if path.head == prefix =>
          routes(evaluator, path.tail, args)
        case _ =>
          None
      }
  }

  final case class Straight[Req, +Resp](
                     prefix: Seq[Either[String, Pattern[String, _]]],
                     end: Route[Req, Resp])
                   extends NonEmpty[Req, Resp] {
    def ++[Resp1 >: Resp](routes1: Routes[Req, Resp1]) = routes1 match {
      case _: Empty[_, _] => this
      case route: Route[_, _] =>
        this ++ route.asInstanceOf[Route[Req, Resp1]].straight
      case Prefixed(prefix1, routes1) => prefix.headOption match {
        case Some(Left(str)) =>
          if (str == prefix1)
            Prefixed(prefix1, step ++ routes1)
          else
            Choice(stringMap = Map(str -> step, prefix1 -> routes1))
        case Some(Right(pat)) =>
          val pair = pat -> step
          Choice(stringMap = Map(prefix1 -> routes1),
                 patternMap = Map(pair), patternSeq = Vector(pair))
        case None => end.spec.rest match {
          case Some(pat) =>
            val pair = pat -> end
            Choice(stringMap = Map(prefix1 -> routes1),
                   restMap = Map(pair), restSeq = Vector(pair))
          case None =>
            Choice(stringMap = Map(prefix1 -> routes1),
                   ends = Vector(end))
        }
      }
      case routes1 @ Straight(prefix1, end1) =>
        (prefix.headOption, prefix1.headOption) match {
          case (None, None) => (end.spec.rest, end1.spec.rest) match {
            case (None, None) =>
              if (!end.isGuarded)
                throw new RouteOvershadowedException(end1, end)
              Choice(ends = Vector(end, end1))
            case (None, Some(pat1)) =>
              val pair1 = pat1 -> end1
              Choice(restMap = Map(pair1), restSeq = Vector(pair1),
                     ends = Vector(end))
            case (Some(pat), None) =>
              val pair = pat -> end
              Choice(restMap = Map(pair), restSeq = Vector(pair),
                     ends = Vector(end1))
            case (Some(pat), Some(pat1)) =>
              val pair = pat -> end
              val pair1 = pat1 -> end1
              if (pat == pat1) {
                if (!end.isGuarded)
                  throw new RouteOvershadowedException(end1, end)
                Choice(restMap = Map(pair1), restSeq = Vector(pair, pair1))
              } else
                Choice(restMap = Map(pair, pair1),
                       restSeq = Vector(pair, pair1))
          }
          case (None, Some(Left(str1))) =>
            val map1 = Map(str1 -> routes1.step)
            end.spec.rest match {
              case Some(pat) =>
                val pair = pat -> end
                Choice(stringMap = map1,
                       restMap = Map(pair), restSeq = Vector(pair))
              case None =>
                Choice(stringMap = map1, ends = Vector(end))
            }
          case (None, Some(Right(pat1))) =>
            val pair1 = pat1 -> routes1.step
            end.spec.rest match {
              case Some(pat) =>
                val pair = pat -> end
                Choice(patternMap = Map(pair1), patternSeq = Vector(pair1),
                       restMap = Map(pair), restSeq = Vector(pair))
              case None =>
                Choice(patternMap = Map(pair1), patternSeq = Vector(pair1),
                       ends = Vector(end))
            }
          case (Some(Left(str)), None) =>
            val map = Map(str -> step)
            end1.spec.rest match {
              case Some(pat1) =>
                val pair1 = pat1 -> end1
                Choice(stringMap = map,
                       restMap = Map(pair1), restSeq = Vector(pair1))
              case None =>
                Choice(stringMap = map, ends = Vector(end1))
            }
          case (Some(Left(str)), Some(Left(str1))) =>
            if (str == str1)
              Prefixed(str, step ++ routes1.step)
            else
              Choice(stringMap = Map(str -> step, str1 -> routes1.step))
          case (Some(Left(str)), Some(Right(pat1))) =>
            val pair1 = pat1 -> routes1.step
            Choice(stringMap = Map(str -> step),
                   patternMap = Map(pair1), patternSeq = Vector(pair1))
          case (Some(Right(pat)), None) =>
            val pair = pat -> step
            end1.spec.rest match {
              case Some(pat1) =>
                val pair1 = pat1 -> end1
                Choice(patternMap = Map(pair), patternSeq = Vector(pair),
                       restMap = Map(pair1), restSeq = Vector(pair1))
              case None =>
                Choice(patternMap = Map(pair), patternSeq = Vector(pair),
                       ends = Vector(end1))
            }
          case (Some(Right(pat)), Some(Left(str1))) =>
            val pair = pat -> step
            Choice(stringMap = Map(str1 -> routes1.step),
                   patternMap = Map(pair), patternSeq = Vector(pair))
          case (Some(Right(pat)), Some(Right(pat1))) =>
            val pair = pat -> step
            val pair1 = pat1 -> routes1.step
            if (pat == pat1)
              Choice(patternMap = Map(pat -> (step ++ routes1.step)),
                     patternSeq = Vector(pair, pair1))
            else
              Choice(patternMap = Map(pair, pair1),
                     patternSeq = Vector(pair, pair1))
        }
      case _choice: Choice[_, _] =>
        val choice = _choice.asInstanceOf[Choice[Req, Resp1]]
        prefix.headOption match {
          case Some(Left(str)) => choice.stringMap.get(str) match {
            case Some(routes1) =>
              choice.copy(stringMap = choice.stringMap +
                                      (str -> (step ++ routes1)))
            case None =>
              choice.copy(stringMap = choice.stringMap + (str -> step))
          }
          case Some(Right(pat)) => choice.patternMap.get(pat) match {
            case Some(routes1) =>
              choice.copy(
                patternMap = choice.patternMap + (pat -> (step ++ routes1)),
                patternSeq = choice.patternSeq :+ (pat -> step))
            case None =>
              val pair = pat -> step
              choice.copy(patternMap = choice.patternMap + pair,
                          patternSeq = choice.patternSeq :+ pair)
          }
          case None => end.spec.rest match {
            case Some(pat) =>
              choice.restMap.get(pat).foreach { end1 =>
                if (!end.isGuarded)
                  throw new RouteOvershadowedException(end1, end)
              }
              val pair = pat -> end
              choice.copy(restMap = choice.restMap + pair,
                          restSeq = choice.restSeq :+ pair)
            case None =>
              choice.ends.lastOption.foreach { end1 =>
                if (!end.isGuarded)
                  throw new RouteOvershadowedException(end1, end)
              }
              choice.copy(ends = choice.ends :+ end)
          }
        }
    }
    protected[routineer] def apply(evaluator: PatternEvaluator,
                                   path: Path, args: Seq[Any]) =
      prefix.headOption match {
        case Some(prefixHead) => path match {
          case Path.Empty => None
          case path: Path.NonEmpty =>
            val pathHead = path.head
            prefixHead match {
              case Left(str) =>
                if (str == pathHead)
                  (new Straight(prefix.tail, end))(
                    evaluator, path.tail, args)
                else
                  None
              case Right(pat) => evaluator.eval(pat, pathHead) match {
                case Some(arg) =>
                  (new Straight(prefix.tail, end))(
                    evaluator, path.tail, arg +: args)
                case None =>
                  None
              }
            }
        }
        case None => end.spec.rest match {
          case Some(pat) => path match {
            case Path.Empty => None
            case path: Path.NonEmpty => pat.matches(path.string) match {
              case Some(arg) =>
                end.checkGuard(
                  seqToTuple(arg +: args).asInstanceOf[end.RouteArgs])
              case None => None
            }
          }
          case None => path match {
            case Path.Empty =>
              end.checkGuard(seqToTuple(args).asInstanceOf[end.RouteArgs])
            case _ => None
          }
        }
      }
    def step = Straight(prefix.tail, end)
  }

  final case class Choice[Req, +Resp](
                     stringMap: Map[String, Routes[Req, Resp]] =
                       Map.empty[String, Routes[Req, Resp]],
                     patternMap: Map[Pattern[String, _], Routes[Req, Resp]] =
                       Map.empty[Pattern[String, _], Routes[Req, Resp]],
                     patternSeq: Seq[(Pattern[String, _], Routes[Req, Resp])] =
                       Vector.empty[(Pattern[String, _], Routes[Req, Resp])],
                     restMap: Map[Pattern[String, _], Route[Req, Resp]] =
                       Map.empty[Pattern[String, _], Route[Req, Resp]],
                     restSeq: Seq[(Pattern[String, _], Route[Req, Resp])] =
                       Vector.empty[(Pattern[String, _], Route[Req, Resp])],
                     ends: Seq[Route[Req, Resp]] = Vector.empty)
                   extends NonEmpty[Req, Resp] {
    def ++[Resp1 >: Resp](routes1: Routes[Req, Resp1]) = routes1 match {
      case _: Empty[_, _] => this
      case route: Route[_, _] =>
        this ++ route.asInstanceOf[Route[Req, Resp1]].straight
      case Prefixed(prefix1, routes1) => stringMap.get(prefix1) match {
        case Some(routes) =>
          copy(stringMap = stringMap + (prefix1 -> (routes ++ routes1)))
        case None =>
          copy(stringMap = stringMap + (prefix1 -> routes1))
      }
      case routes1 @ Straight(prefix1, end1) => prefix1.headOption match {
        case Some(Left(str1)) => stringMap.get(str1) match {
          case Some(routes) =>
            copy(stringMap = stringMap +
                             (str1 -> (routes ++ routes1.step)))
          case None =>
            copy(stringMap = stringMap + (str1 -> routes1.step))
        }
        case Some(Right(pat1)) => patternMap.get(pat1) match {
          case Some(routes) =>
            val step1 = routes1.step
            copy(patternMap = patternMap + (pat1 -> (routes ++ step1)),
                 patternSeq = patternSeq :+ (pat1 -> step1))
          case None =>
            val pair1 = pat1 -> routes1.step
            copy(patternMap = patternMap + pair1,
                 patternSeq = patternSeq :+ pair1)
        }
        case None => end1.spec.rest match {
          case Some(pat1) =>
            restMap.get(pat1).foreach { end =>
              if (!end.isGuarded)
                throw new RouteOvershadowedException(end1, end)
            }
            val pair1 = pat1 -> end1
            copy(restMap = restMap + pair1, restSeq = restSeq :+ pair1)
          case None =>
            ends.lastOption.foreach { end =>
              if (!end.isGuarded)
                throw new RouteOvershadowedException(end1, end)
            }
            copy(ends = ends :+ end1)
        }
      }
      case _choice: Choice[_, _] =>
        val choice = _choice.asInstanceOf[Choice[Req, Resp1]]
        val newStringMap =
          choice.stringMap.
              foldLeft(stringMap: Map[String, Routes[Req, Resp1]]) {
            case (m, (str1, routes1)) => m.get(str1) match {
              case Some(routes) => m + (str1 -> (routes ++ routes1))
              case None => m + (str1 -> routes1)
            }
          }
        val (newPatternMap, newPatternSeq) =
          choice.patternSeq.
              foldLeft((patternMap: Map[Pattern[String, _], Routes[Req, Resp1]],
                        patternSeq: Seq[(Pattern[String, _],
                                         Routes[Req, Resp1])])) {
            case ((m, s), (pat1, routes1)) => m.get(pat1) match {
              case Some(routes) =>
                (m + (pat1 -> (routes ++ routes1)), s :+ (pat1 -> routes1))
              case None =>
                (m + (pat1 -> routes1), s :+ (pat1 -> routes1))
            }
          }
        val (newRestMap, newRestSeq) =
          choice.restSeq.
              foldLeft((restMap: Map[Pattern[String, _], Route[Req, Resp1]],
                        restSeq: Seq[(Pattern[String, _],
                                      Route[Req, Resp1])])) {
            case ((m, s), (pat1, end1)) =>
              m.get(pat1).foreach { end =>
                if (!end.isGuarded)
                  throw new RouteOvershadowedException(end1, end)
              }
              val pair1 = pat1 -> end1
              (m + pair1, s :+ pair1)
          }
        val newEnds =
          if (choice.ends.isEmpty)
            ends
          else {
            ends.lastOption.foreach { end =>
              if (!end.isGuarded)
                throw new RouteOvershadowedException(choice.ends.head, end)
            }
            ends ++ choice.ends
          }
        Choice(stringMap = newStringMap,
               patternMap = newPatternMap, patternSeq = newPatternSeq,
               restMap = newRestMap, restSeq = newRestSeq,
               ends = newEnds)
    }
    protected[routineer] def apply(
        evaluator: PatternEvaluator, path: Path,
        args: Seq[Any]): Option[() => Resp] = path match {
      case Path.Empty =>    
        ends.foreach { end =>
          val result = end.checkGuard(
                         seqToTuple(args).asInstanceOf[end.RouteArgs])
          if (result.isDefined)
            return result
        }
        return None
      case path: Path.NonEmpty =>
        val pathHead = path.head
        val pathTail = path.tail
        stringMap.get(pathHead).foreach { routes =>
          val result = routes(evaluator, pathTail, args)
          if (result.isDefined)
            return result
        }
        patternSeq.foreach { case (pat, routes) =>
          evaluator.eval(pat, pathHead).foreach { arg =>
            val result = routes(evaluator, pathTail, arg +: args)
            if (result.isDefined)
              return result
          }
        }
        restSeq.foreach { case (pat, end) =>
          evaluator.eval(pat, path.string).foreach { arg =>
            val result = end.checkGuard(
                           seqToTuple(args).asInstanceOf[end.RouteArgs])
            if (result.isDefined)
              return result
          } 
        }
        None
    }
  }
}
