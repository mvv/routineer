package com.github.mvv.routineer

import com.github.mvv.routineer.Route.NoHandler

import scala.language.higherKinds

sealed trait Routes[+H[_ <: Args]] {
  final def :+[O <: Args, H1[O1 <: Args] >: H[O1]](route: Route[O, H1]): Routes[H1] =
    this ++ Routes(route)
  final def +:[O <: Args, H1[O1 <: Args] >: H[O1]](route: Route[O, H1]): Routes[H1] =
    Routes(route) ++ this
  def ++[H1[O <: Args] >: H[O]](routes: Routes[H1]): Routes[H1]
  def /:(segment: String): Routes[H]
  def dispatch(parser: PathParser): Dispatch[H]
}

object Routes {
  val empty: Routes[Route.NoHandler] = Empty

  def apply[O <: Args, H[_ <: Args]](route: Route[O, H]): Routes[H] =
    NonEmpty(tracePath(route.pattern.trace, route.handler))

  private def tracePath[I <: Args, O <: Args, H[_ <: Args]](trace: RoutePattern.PathTrace[I, O, RoutePattern.PathEnd],
                                                            handler: H[O]): PathTrace[I, H] =
    trace match {
      case RoutePattern.SegmentCheck(ValueCheck(EqualsP(segment: String)), false, next) =>
        SegmentConst(segment, tracePath(next, handler))
      case RoutePattern.SegmentCheck(check, force, next) =>
        SegmentCheck(check, force, tracePath(next, handler))
      case RoutePattern.SegmentPattern(pattern, force, next, growable) =>
        SegmentPattern(pattern, force, tracePath(next, handler), growable)
      case RoutePattern.SegmentsCheck(check, force, end) =>
        val next = end match {
          case RoutePattern.PathEnd.WithQuery(trace1) =>
            traceQuery(trace1, handler)
          case RoutePattern.PathEnd.WithoutQuery(same) =>
            same.symm.subst[({ type L[A <: Args] = QueryMatched[A, H] })#L](QueryMatched[O, H](handler))
        }
        SegmentsCheck(check, force, next)
      case RoutePattern.SegmentsPattern(pattern, force, end, growable) =>
        val next = end match {
          case RoutePattern.PathEnd.WithQuery(trace1) =>
            traceQuery(trace1, handler)
          case RoutePattern.PathEnd.WithoutQuery(same) =>
            same.symm.subst[({ type L[A <: Args] = QueryMatched[A, H] })#L](QueryMatched[O, H](handler))
        }
        SegmentsPattern(pattern, force, next, growable)
      case RoutePattern.PathMatched(end) =>
        val next = end match {
          case RoutePattern.PathEnd.WithQuery(trace1) =>
            traceQuery(trace1, handler)
          case RoutePattern.PathEnd.WithoutQuery(same) =>
            same.symm.subst[({ type L[A <: Args] = QueryMatched[A, H] })#L](QueryMatched[O, H](handler))
        }
        PathMatched(next)
    }

  private def traceQuery[I <: Args, O <: Args, H[_ <: Args]](trace: RoutePattern.QueryTrace[I, O],
                                                             handler: H[O]): QueryTrace[I, H] =
    trace match {
      case RoutePattern.ParamCheck(name, check, force, next) =>
        ParamCheck(name, check, force, traceQuery(next, handler))
      case RoutePattern.ParamPattern(name, pattern, force, next, growable) =>
        ParamPattern(name, pattern, force, traceQuery(next, handler), growable)
      case RoutePattern.ParamsCheck(check, force, same) =>
        ParamsCheck(check, force, same.symm.subst(handler))
      case RoutePattern.ParamsPattern(pattern, force, same, growable) =>
        ParamsPattern(pattern, force, same.symm.subst(handler), growable)
      case RoutePattern.QueryMatched(same) =>
        QueryMatched(same.symm.subst(handler))
    }

  sealed private trait Alts[+A] {
    def ++[B >: A](alts: Alts[B]): Alts[B]
  }
  private object Alts {
    def apply[A](head: A, last: A): NonEmpty[A] = Two(head, last)

    case object Empty extends Alts[Nothing] {
      override def ++[B >: Nothing](alts: Alts[B]): Alts[B] = alts
    }

    sealed trait NonEmpty[+A] extends Alts[A] {
      override def ++[B >: A](alts: Alts[B]): NonEmpty[B]
      def head: A
      def last: A
      def merge[B >: A](merged: Single[B], alts: NonEmpty[B]): NonEmpty[B]
    }

    sealed trait Single[+A] extends NonEmpty[A] { self: A =>
      override def ++[B >: A](alts: Alts[B]): NonEmpty[B] = alts match {
        case Empty             => this
        case single: Single[B] => Two(self, single.head)
        case two: Two[B]       => More(self, Vector(two.head), two.last)
        case more: More[B]     => More(self, more.head +: more.values, more.last)
      }
      override def head: A = self
      override def last: A = self
      override def merge[B >: A](merged: Single[B], alts: NonEmpty[B]): NonEmpty[B] = alts match {
        case _: Single[B]  => merged
        case two: Two[B]   => Two(merged.head, two.last)
        case more: More[B] => More(merged.head, more.values, more.last)
      }
    }

    final private case class Two[+A](head: A, last: A) extends NonEmpty[A] {
      override def ++[B >: A](alts: Alts[B]): NonEmpty[B] = alts match {
        case Empty             => this
        case single: Single[B] => More(head, Vector(last), single.head)
        case two: Two[B]       => More(head, Vector(last, two.head), two.last)
        case more: More[B]     => More(head, last +: more.head +: more.values, more.last)
      }
      override def merge[B >: A](merged: Single[B], alts: NonEmpty[B]): NonEmpty[B] = alts match {
        case _: Single[B]  => Two(head, merged.head)
        case two: Two[B]   => More(head, Vector(merged.head), two.last)
        case more: More[B] => More(head, merged.head +: more.values, more.last)
      }
    }

    final private case class More[+A](head: A, values: Seq[A], last: A) extends NonEmpty[A] {
      override def ++[B >: A](alts: Alts[B]): NonEmpty[B] = alts match {
        case Empty             => this
        case single: Single[B] => More(head, values :+ last, single.head)
        case two: Two[B]       => More(head, values :+ last :+ two.head, two.last)
        case more: More[B]     => More(head, (values :+ last :+ more.head) ++ more.values, more.last)
      }
      override def merge[B >: A](merged: Single[B], alts: NonEmpty[B]): NonEmpty[B] = alts match {
        case _: Single[B]  => More(head, values, merged.head)
        case two: Two[B]   => More(head, values :+ merged.head, two.last)
        case more: More[B] => More(head, (values :+ merged.head) ++ more.values, more.last)
      }
    }
  }

  private object Empty extends Routes[Route.NoHandler] {
    override def ++[H1[O <: Args] >: Route.NoHandler[O]](routes: Routes[H1]): Routes[H1] = routes
    override def /:(prefix: String): Routes[Route.NoHandler] = this
    override def dispatch(parser: PathParser): Dispatch[NoHandler] = Dispatch.NotFound
  }

  final private case class NonEmpty[+H[_ <: Args]](trace: PathTrace[Args._0, H]) extends Routes[H] {
    override def ++[H1[O <: Args] >: H[O]](routes: Routes[H1]): Routes[H1] = routes match {
      case rs: NonEmpty[H1] => NonEmpty(trace ++ rs.trace)
      case Empty            => this
    }
    override def /:(prefix: String): Routes[H] = NonEmpty(SegmentConst(prefix, trace))
    final override def dispatch(parser: PathParser): Dispatch[H] = ???
  }

  sealed private trait PathTrace[I <: Args, +H[_ <: Args]] {
    def path: Alts[MorePathAlt[I, H]]
    def noPath: Alts[NoMorePathAlt[I, H]]
    def ++[H1[O <: Args] >: H[O]](trace: PathTrace[I, H1]): PathTrace[I, H1] = {
      val (path1, combinedCache) = (path, trace.path) match {
        case (Alts.Empty, result) => (result, None)
        case (result, Alts.Empty) => (result, None)
        case (nePath: Alts.NonEmpty[MorePathAlt[I, H]], neTracePath: Alts.NonEmpty[MorePathAlt[I, H1]]) =>
          nePath.last.combine(neTracePath.head) match {
            case Some(combined) =>
              (nePath.merge(combined, neTracePath), Some((nePath.last, neTracePath.head, combined)))
            case None =>
              (nePath ++ neTracePath, None)
          }
      }
      val noPath1 = (noPath, trace.noPath) match {
        case (Alts.Empty, result) => result
        case (result, Alts.Empty) => result
        case (neNoPath: Alts.NonEmpty[NoMorePathAlt[I, H]], neTraceNoPath: Alts.NonEmpty[NoMorePathAlt[I, H1]]) =>
          combinedCache match {
            case Some((last, head, combined)) if (neNoPath.last eq last) && (neTraceNoPath.head eq head) =>
              neNoPath.merge(combined.asInstanceOf[NoMorePathAlt[I, H1]], neTraceNoPath)
            case _ =>
              neNoPath.last.combine(neTraceNoPath.head) match {
                case Some(combined) => neNoPath.merge(combined, neTraceNoPath)
                case None           => neNoPath ++ neTraceNoPath
              }
          }
      }
      (path1, noPath1) match {
        case (Alts.Empty, singleNoPath)                   => singleNoPath.asInstanceOf[NoMorePathAlt[I, H1]]
        case (singlePath: MorePathAlt[I, H1], Alts.Empty) => singlePath
        case (singlePath: MorePathAlt[I, H1], singleNoPath: NoMorePathAlt[I, H1]) if singlePath eq singleNoPath =>
          singlePath
        case (nePath: Alts.NonEmpty[MorePathAlt[I, H1]], _) =>
          PathAlts(nePath, noPath1)
      }
    }
  }

  sealed private trait PathAlt[I <: Args, +H[_ <: Args]] extends PathTrace[I, H] with Alts.Single[PathAlt[I, H]] {
    def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[PathAlt[I, H1]]
  }
  sealed private trait MorePathAlt[I <: Args, +H[_ <: Args]] extends PathAlt[I, H] with Alts.Single[MorePathAlt[I, H]] {
    final override def path: MorePathAlt[I, H] = this
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[MorePathAlt[I, H1]]
  }
  sealed private trait SegmentAlt[I <: Args, +H[_ <: Args]] extends MorePathAlt[I, H] {
    final override def noPath: Alts[NoMorePathAlt[I, H]] = Alts.Empty
  }

  final private case class SegmentConst[I <: Args, +H[_ <: Args]](segment: String, next: PathTrace[I, H])
      extends SegmentAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[MorePathAlt[I, H1]] =
      alt match {
        case t: SegmentConst[I, H1] if t.segment == segment =>
          Some(SegmentConst(segment, next ++ t.next))
        case t: SegmentConst[I, H1] =>
          Some(SegmentConsts(Map(segment -> next, t.segment -> t.next)))
        case t: SegmentConsts[I, H1] =>
          Some {
            SegmentConsts {
              t.segments.get(segment) match {
                case Some(next1) => t.segments.updated(segment, next ++ next1)
                case None        => t.segments.updated(segment, next)
              }
            }
          }
        case _ => None
      }
  }

  final private case class SegmentConsts[I <: Args, +H[_ <: Args]](segments: Map[String, PathTrace[I, H]])
      extends SegmentAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[MorePathAlt[I, H1]] =
      alt match {
        case t: SegmentConst[I, H1] =>
          Some {
            SegmentConsts {
              segments.get(t.segment) match {
                case Some(next) => segments.updated(t.segment, next ++ t.next)
                case None       => segments.updated(t.segment, t.next)
              }
            }
          }
        case t: SegmentConsts[I, H1] =>
          Some {
            SegmentConsts {
              t.segments.foldLeft(segments: Map[String, PathTrace[I, H1]]) {
                case (acc, (segment, next1)) =>
                  acc.get(segment) match {
                    case Some(next) => acc.updated(segment, next ++ next1)
                    case None       => acc.updated(segment, next1)
                  }
              }
            }
          }
        case _ => None
      }
  }

  final private case class SegmentCheck[I <: Args, +H[_ <: Args]](check: ValueCheck[String],
                                                                  force: Boolean,
                                                                  next: PathTrace[I, H])
      extends SegmentAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[MorePathAlt[I, H1]] =
      alt match {
        case t: SegmentCheck[I, H1] if t.check == check && t.force == force =>
          Some(SegmentCheck(check, force, next ++ t.next))
        case _ => None
      }
  }

  final private case class SegmentPattern[R, I <: Args, +H[_ <: Args]](pattern: ValuePattern[String, R],
                                                                       force: Boolean,
                                                                       next: PathTrace[I#Append[R], H],
                                                                       growable: I <:< I with Args.Growable)
      extends SegmentAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[MorePathAlt[I, H1]] =
      alt match {
        case t: SegmentPattern[_, I, H1] if t.pattern == pattern && t.force == force =>
          Some(SegmentPattern(pattern, force, next ++ t.next.asInstanceOf[PathTrace[I#Append[R], H1]], growable))
        case _ => None
      }
  }

  sealed private trait NoMorePathAlt[I <: Args, +H[_ <: Args]]
      extends PathAlt[I, H]
      with Alts.Single[NoMorePathAlt[I, H]] {
    final override def noPath: NoMorePathAlt[I, H] = this
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[NoMorePathAlt[I, H1]]
  }
  sealed private trait SegmentsAlt[I <: Args, +H[_ <: Args]]
      extends MorePathAlt[I, H]
      with NoMorePathAlt[I, H]
      with Alts.Single[SegmentsAlt[I, H]] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[SegmentsAlt[I, H1]]
  }

  final private case class SegmentsCheck[I <: Args, +H[_ <: Args]](check: ValueCheck[String],
                                                                   force: Boolean,
                                                                   next: QueryTrace[I, H])
      extends SegmentsAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[SegmentsAlt[I, H1]] =
      alt match {
        case t: SegmentsCheck[I, H1] if t.check == check && t.force == force =>
          Some(SegmentsCheck(check, force, next ++ t.next))
        case _ => None
      }
  }

  final private case class SegmentsPattern[R, I <: Args, +H[_ <: Args]](pattern: ValuePattern[String, R],
                                                                        force: Boolean,
                                                                        next: QueryTrace[I#Append[R], H],
                                                                        growable: I <:< I with Args.Growable)
      extends SegmentsAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[SegmentsAlt[I, H1]] =
      alt match {
        case t: SegmentsPattern[_, I, H1] if t.pattern == pattern && t.force == force =>
          Some(SegmentsPattern(pattern, force, next ++ t.next.asInstanceOf[QueryTrace[I#Append[R], H1]], growable))
        case _ => None
      }
  }

  final private case class PathMatched[I <: Args, +H[_ <: Args]](next: QueryTrace[I, H]) extends NoMorePathAlt[I, H] {
    override def path: Alts[MorePathAlt[I, H]] = Alts.Empty
    override def combine[H1[O <: Args] >: H[O]](alt: PathAlt[I, H1]): Option[NoMorePathAlt[I, H1]] =
      alt match {
        case t: PathMatched[I, H1] => Some(PathMatched(next ++ t.next))
        case _                     => None
      }
  }

  final private case class PathAlts[I <: Args, +H[_ <: Args]](path: Alts.NonEmpty[MorePathAlt[I, H]],
                                                              noPath: Alts[NoMorePathAlt[I, H]])
      extends PathTrace[I, H]

  sealed private trait QueryTrace[I <: Args, +H[_ <: Args]] {
    def alts: Alts.NonEmpty[QueryAlt[I, H]]
    final def ++[H1[O <: Args] >: H[O]](trace: QueryTrace[I, H1]): QueryTrace[I, H1] = {
      val alts1 = alts.last.combine(trace.alts.head) match {
        case Some(combined) => alts.merge(combined, trace.alts)
        case None           => alts ++ trace.alts
      }
      alts1 match {
        case alt: QueryAlt[I, H1] => alt
        case _                    => QueryAlts(alts1)
      }
    }
  }

  sealed private trait QueryAlt[I <: Args, +H[_ <: Args]] extends QueryTrace[I, H] with Alts.Single[QueryAlt[I, H]] {
    final override def alts: QueryAlt[I, H] = this
    def combine[H1[O <: Args] >: H[O]](alt: QueryAlt[I, H1]): Option[QueryAlt[I, H1]]
  }

  final private case class ParamCheck[I <: Args, +H[_ <: Args]](name: String,
                                                                check: ValueCheck[Seq[String]],
                                                                force: Boolean,
                                                                next: QueryTrace[I, H])
      extends QueryAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: QueryAlt[I, H1]): Option[QueryAlt[I, H1]] = alt match {
      case t: ParamCheck[I, H1] if t.check == check && t.force == force =>
        Some(ParamCheck(name, check, force, next ++ t.next))
      case _ => None
    }
  }

  final private case class ParamPattern[R, I <: Args, +H[_ <: Args]](name: String,
                                                                     pattern: ValuePattern[Seq[String], R],
                                                                     force: Boolean,
                                                                     next: QueryTrace[I#Append[R], H],
                                                                     growable: I <:< I with Args.Growable)
      extends QueryAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: QueryAlt[I, H1]): Option[QueryAlt[I, H1]] = alt match {
      case t: ParamPattern[_, I, H1] if t.pattern == pattern && t.force == force =>
        Some(ParamPattern(name, pattern, force, next ++ t.next.asInstanceOf[QueryTrace[I#Append[R], H1]], growable))
      case _ => None
    }
  }

  final private case class ParamsCheck[I <: Args, +H[_ <: Args]](check: ValueCheck[Map[String, ParamValues]],
                                                                 force: Boolean,
                                                                 handler: H[I])
      extends QueryAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: QueryAlt[I, H1]): Option[QueryAlt[I, H1]] = None
  }

  final private case class ParamsPattern[R, I <: Args, +H[_ <: Args]](
      pattern: ValuePattern[Map[String, ParamValues], R],
      force: Boolean,
      handler: H[I#Append[R]],
      growable: I <:< I with Args.Growable
  ) extends QueryAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: QueryAlt[I, H1]): Option[QueryAlt[I, H1]] = None
  }

  final private case class QueryMatched[I <: Args, +H[_ <: Args]](handler: H[I]) extends QueryAlt[I, H] {
    override def combine[H1[O <: Args] >: H[O]](alt: QueryAlt[I, H1]): Option[QueryAlt[I, H1]] = None
  }

  final private case class QueryAlts[I <: Args, +H[_ <: Args]](alts: Alts.NonEmpty[QueryAlt[I, H]])
      extends QueryTrace[I, H]
}
