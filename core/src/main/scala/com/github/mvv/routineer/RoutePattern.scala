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

sealed trait RoutePattern[O <: Args] {
  def trace: RoutePattern.PathTrace[Args._0, O, RoutePattern.PathEnd]
}

object RoutePattern {
  sealed trait PathTrace[I <: Args, O <: Args, +E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]]
  final case class SegmentCheck[I <: Args, O <: Args, +E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
      check: ValueCheck[String],
      force: Boolean,
      next: PathTrace[I, O, E]
  ) extends PathTrace[I, O, E]
  final case class SegmentPattern[A, I <: Args, O <: Args, +E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
      pattern: ValuePattern[String, A],
      force: Boolean,
      next: PathTrace[I#Append[A], O, E],
      growable: I <:< I with Args.Growable
  ) extends PathTrace[I, O, E]
  final case class SegmentsCheck[I <: Args, O <: Args, +E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
      check: ValueCheck[Seq[String]],
      force: Boolean,
      next: E[I, O]
  ) extends PathTrace[I, O, E]
  final case class SegmentsPattern[A, I <: Args, O <: Args, +E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
      pattern: ValuePattern[Seq[String], A],
      force: Boolean,
      end: E[I#Append[A], O],
      growable: I <:< I with Args.Growable
  ) extends PathTrace[I, O, E]
  final case class PathMatched[I <: Args, O <: Args, +E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](end: E[I, O])
      extends PathTrace[I, O, E]

  sealed trait PathEnd[I <: Args, O <: Args]
  object PathEnd {
    final case class WithoutQuery[I <: Args, O <: Args](same: Args.Same[I, O]) extends PathEnd[I, O]
    final case class WithQuery[I <: Args, O <: Args](trace: QueryTrace[I, O]) extends PathEnd[I, O]
  }

  sealed trait QueryTrace[I <: Args, O <: Args]
  final case class ParamCheck[I <: Args, O <: Args](name: String,
                                                    check: ValueCheck[Seq[String]],
                                                    force: Boolean,
                                                    next: QueryTrace[I, O])
      extends QueryTrace[I, O]
  final case class ParamPattern[A, I <: Args, O <: Args](name: String,
                                                         check: ValuePattern[Seq[String], A],
                                                         force: Boolean,
                                                         next: QueryTrace[I#Append[A], O],
                                                         growable: I <:< I with Args.Growable)
      extends QueryTrace[I, O]
  final case class ParamsCheck[I <: Args, O <: Args](check: ValueCheck[Map[String, ParamValues]],
                                                     force: Boolean,
                                                     same: Args.Same[I, O])
      extends QueryTrace[I, O]
  final case class ParamsPattern[A, I <: Args, O <: Args](pattern: ValuePattern[Map[String, ParamValues], A],
                                                          force: Boolean,
                                                          same: Args.Same[I#Append[A], O],
                                                          growable: I <:< I with Args.Growable)
      extends QueryTrace[I, O]
  final case class QueryMatched[I <: Args, O <: Args](same: Args.Same[I, O]) extends QueryTrace[I, O]

  sealed trait OnlyPath[O <: Args] extends RoutePattern[O] {
    def ?>(name: String, check: ValueCheck[Seq[String]]): PathWithPartialQuery[O]
    def ?>(name: String, check: ValueCheck.Backtrack[Seq[String]]): PathWithPartialQuery[O]
    def ?>[A](name: String, pattern: ValuePattern[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]]
    def ?>[A](name: String, pattern: ValuePattern.Backtrack[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]]
    def ?+(check: ValueCheck[Map[String, ParamValues]]): PathWithFullQuery[O]
    def ?+(check: ValueCheck.Backtrack[Map[String, ParamValues]]): PathWithFullQuery[O]
    def ?+[A](check: ValuePattern[Map[String, ParamValues], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithFullQuery[O#Append[A]]
    def ?+[A](check: ValuePattern.Backtrack[Map[String, ParamValues], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithFullQuery[O#Append[A]]
    override def trace: RoutePattern.PathTrace[Args._0, O, PathEnd.WithoutQuery]
  }

  sealed trait PartialPath[O <: Args] extends OnlyPath[O] {
    def />(check: ValueCheck[String]): PartialPath[O]
    def />(check: ValueCheck.Force[String]): PartialPath[O]
    def />[A](pattern: ValuePattern[String, A])(implicit growable: O <:< O with Args.Growable): PartialPath[O#Append[A]]
    def />[A](pattern: ValuePattern.Force[String, A])(
        implicit growable: O <:< O with Args.Growable
    ): PartialPath[O#Append[A]]
    def /+(check: ValueCheck[Seq[String]]): FullPath[O]
    def /+(check: ValueCheck.Force[Seq[String]]): FullPath[O]
    def /+[A](check: ValuePattern[Seq[String], A])(implicit growable: O <:< O with Args.Growable): FullPath[O#Append[A]]
    def /+[A](check: ValuePattern.Force[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): FullPath[O#Append[A]]
  }

  sealed trait FullPath[O <: Args] extends OnlyPath[O]

  sealed trait PathWithPartialQuery[O <: Args] extends RoutePattern[O] {
    def &>(name: String, check: ValueCheck[Seq[String]]): PathWithPartialQuery[O]
    def &>(name: String, check: ValueCheck.Backtrack[Seq[String]]): PathWithPartialQuery[O]
    def &>[A](name: String, pattern: ValuePattern[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]]
    def &>[A](name: String, pattern: ValuePattern.Backtrack[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]]
    def &+(check: ValueCheck[Map[String, ParamValues]]): PathWithFullQuery[O]
    def &+(check: ValueCheck.Backtrack[Map[String, ParamValues]]): PathWithFullQuery[O]
    def &+[A](check: ValuePattern[Map[String, ParamValues], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithFullQuery[O#Append[A]]
    def &+[A](check: ValuePattern.Backtrack[Map[String, ParamValues], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithFullQuery[O#Append[A]]
    override def trace: PathTrace[Args._0, O, PathEnd.WithQuery]
  }

  sealed trait PathWithFullQuery[O <: Args] extends RoutePattern[O] {
    override def trace: PathTrace[Args._0, O, PathEnd.WithQuery]
  }

  private trait CompletePathTrace[O <: Args] {
    def apply[O1 <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
        next: PathTrace[O, O1, E]
    ): PathTrace[Args._0, O1, E]
  }

  val Root: PartialPath[Args._0] = PartialPathImpl(new CompletePathTrace[Args._0] {
    override def apply[O1 <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
        next: PathTrace[Args._0, O1, E]
    ): PathTrace[Args._0, O1, E] = next
  })

  final private case class PartialPathImpl[I <: Args](complete: CompletePathTrace[I]) extends PartialPath[I] {
    override def />(check: ValueCheck[String]): PartialPath[I] =
      PartialPathImpl(new CompletePathTrace[I] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            next: PathTrace[I, O, E]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentCheck(check, false, next))
      })
    override def />(check: ValueCheck.Force[String]): PartialPath[I] =
      PartialPathImpl(new CompletePathTrace[I] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            next: PathTrace[I, O, E]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentCheck(ValueCheck(check.pattern), true, next))
      })
    override def />[A](
        pattern: ValuePattern[String, A]
    )(implicit growable: I <:< I with Args.Growable): PartialPath[I#Append[A]] =
      PartialPathImpl(new CompletePathTrace[I#Append[A]] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            next: PathTrace[I#Append[A], O, E]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentPattern(pattern, false, next, growable))
      })
    override def />[A](
        pattern: ValuePattern.Force[String, A]
    )(implicit growable: I <:< I with Args.Growable): PartialPath[I#Append[A]] =
      PartialPathImpl(new CompletePathTrace[I#Append[A]] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            next: PathTrace[I#Append[A], O, E]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentPattern(pattern.pattern, true, next, growable))
      })
    override def /+(check: ValueCheck[Seq[String]]): FullPath[I] =
      FullPathImpl(new CompletePathMatched[I] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            end: E[I, O]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentsCheck(check, false, end))
      })
    override def /+(check: ValueCheck.Force[Seq[String]]): FullPath[I] =
      FullPathImpl(new CompletePathMatched[I] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            end: E[I, O]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentsCheck(ValueCheck(check.pattern), true, end))
      })
    override def /+[A](
        pattern: ValuePattern[Seq[String], A]
    )(implicit growable: I <:< I with Args.Growable): FullPath[I#Append[A]] =
      FullPathImpl(new CompletePathMatched[I#Append[A]] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            end: E[I#Append[A], O]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentsPattern(pattern, false, end, growable))
      })
    override def /+[A](
        pattern: ValuePattern.Force[Seq[String], A]
    )(implicit growable: I <:< I with Args.Growable): FullPath[I#Append[A]] =
      FullPathImpl(new CompletePathMatched[I#Append[A]] {
        override def apply[O <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
            end: E[I#Append[A], O]
        ): PathTrace[Args._0, O, E] =
          complete(SegmentsPattern(pattern.pattern, true, end, growable))
      })
    override def ?>(name: String, check: ValueCheck[Seq[String]]): PathWithPartialQuery[I] =
      PartialQueryImpl(new CompleteQueryTrace[I] {
        override def apply[O <: Args](next: QueryTrace[I, O]): PathTrace[Args._0, O, PathEnd.WithQuery] =
          complete(PathMatched(PathEnd.WithQuery(ParamCheck(name, check, true, next))))
      })
    override def ?>(name: String, check: ValueCheck.Backtrack[Seq[String]]): PathWithPartialQuery[I] =
      PartialQueryImpl(new CompleteQueryTrace[I] {
        override def apply[O <: Args](next: QueryTrace[I, O]): PathTrace[Args._0, O, PathEnd.WithQuery] =
          complete(PathMatched(PathEnd.WithQuery(ParamCheck(name, ValueCheck(check.pattern), false, next))))
      })
    override def ?>[A](name: String, pattern: ValuePattern[Seq[String], A])(
        implicit growable: I <:< I with Args.Growable
    ): PathWithPartialQuery[I#Append[A]] =
      PartialQueryImpl(new CompleteQueryTrace[I#Append[A]] {
        override def apply[O <: Args](next: QueryTrace[I#Append[A], O]): PathTrace[Args._0, O, PathEnd.WithQuery] =
          complete(PathMatched(PathEnd.WithQuery(ParamPattern(name, pattern, true, next, growable))))
      })
    override def ?>[A](name: String, pattern: ValuePattern.Backtrack[Seq[String], A])(
        implicit growable: I <:< I with Args.Growable
    ): PathWithPartialQuery[I#Append[A]] =
      PartialQueryImpl(new CompleteQueryTrace[I#Append[A]] {
        override def apply[O <: Args](next: QueryTrace[I#Append[A], O]): PathTrace[Args._0, O, PathEnd.WithQuery] =
          complete(PathMatched(PathEnd.WithQuery(ParamPattern(name, pattern.pattern, false, next, growable))))
      })
    override def ?+(check: ValueCheck[Map[String, ParamValues]]): PathWithFullQuery[I] =
      FullQueryImpl(complete(PathMatched(PathEnd.WithQuery(ParamsCheck(check, true, Args.same[I])))))
    override def ?+(check: ValueCheck.Backtrack[Map[String, ParamValues]]): PathWithFullQuery[I] =
      FullQueryImpl(
        complete(PathMatched(PathEnd.WithQuery(ParamsCheck(ValueCheck(check.pattern), false, Args.same[I]))))
      )
    override def ?+[A](
        pattern: ValuePattern[Map[String, ParamValues], A]
    )(implicit growable: I <:< I with Args.Growable): PathWithFullQuery[I#Append[A]] =
      FullQueryImpl(
        complete(PathMatched(PathEnd.WithQuery(ParamsPattern(pattern, true, Args.same[I#Append[A]], growable))))
      )
    override def ?+[A](
        pattern: ValuePattern.Backtrack[Map[String, ParamValues], A]
    )(implicit growable: I <:< I with Args.Growable): PathWithFullQuery[I#Append[A]] =
      FullQueryImpl(
        complete(
          PathMatched(PathEnd.WithQuery(ParamsPattern(pattern.pattern, false, Args.same[I#Append[A]], growable)))
        )
      )
    override def trace: PathTrace[Args._0, I, PathEnd.WithoutQuery] =
      complete(PathMatched(PathEnd.WithoutQuery(Args.same[I])))
  }

  private trait CompletePathMatched[O <: Args] {
    def apply[O1 <: Args, E[EI <: Args, EO <: Args] <: PathEnd[EI, EO]](
        end: E[O, O1]
    ): PathTrace[Args._0, O1, E]
  }

  final private case class FullPathImpl[O <: Args](complete: CompletePathMatched[O]) extends FullPath[O] {
    override def ?>(name: String, check: ValueCheck[Seq[String]]): PathWithPartialQuery[O] =
      PartialQueryImpl(new CompleteQueryTrace[O] {
        override def apply[O1 <: Args](next: QueryTrace[O, O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(PathEnd.WithQuery(ParamCheck(name, check, true, next)))
      })
    override def ?>(name: String, check: ValueCheck.Backtrack[Seq[String]]): PathWithPartialQuery[O] =
      PartialQueryImpl(new CompleteQueryTrace[O] {
        override def apply[O1 <: Args](next: QueryTrace[O, O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(PathEnd.WithQuery(ParamCheck(name, ValueCheck(check.pattern), false, next)))
      })
    override def ?>[A](name: String, pattern: ValuePattern[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]] =
      PartialQueryImpl(new CompleteQueryTrace[O#Append[A]] {
        override def apply[O1 <: Args](next: QueryTrace[O#Append[A], O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(PathEnd.WithQuery(ParamPattern(name, pattern, true, next, growable)))
      })
    override def ?>[A](name: String, pattern: ValuePattern.Backtrack[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]] =
      PartialQueryImpl(new CompleteQueryTrace[O#Append[A]] {
        override def apply[O1 <: Args](next: QueryTrace[O#Append[A], O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(PathEnd.WithQuery(ParamPattern(name, pattern.pattern, false, next, growable)))
      })
    override def ?+(check: ValueCheck[Map[String, ParamValues]]): PathWithFullQuery[O] =
      FullQueryImpl(complete(PathEnd.WithQuery(ParamsCheck(check, true, Args.same[O]))))
    override def ?+(check: ValueCheck.Backtrack[Map[String, ParamValues]]): PathWithFullQuery[O] =
      FullQueryImpl(
        complete(PathEnd.WithQuery(ParamsCheck(ValueCheck(check.pattern), false, Args.same[O])))
      )
    override def ?+[A](
        pattern: ValuePattern[Map[String, ParamValues], A]
    )(implicit growable: O <:< O with Args.Growable): PathWithFullQuery[O#Append[A]] =
      FullQueryImpl(
        complete(PathEnd.WithQuery(ParamsPattern(pattern, true, Args.same[O#Append[A]], growable)))
      )
    override def ?+[A](
        pattern: ValuePattern.Backtrack[Map[String, ParamValues], A]
    )(implicit growable: O <:< O with Args.Growable): PathWithFullQuery[O#Append[A]] =
      FullQueryImpl(
        complete(
          PathEnd.WithQuery(ParamsPattern(pattern.pattern, false, Args.same[O#Append[A]], growable))
        )
      )
    override def trace: PathTrace[Args._0, O, PathEnd.WithoutQuery] =
      complete(PathEnd.WithoutQuery(Args.same[O]))
  }

  private trait CompleteQueryTrace[O <: Args] {
    def apply[O1 <: Args](next: QueryTrace[O, O1]): PathTrace[Args._0, O1, PathEnd.WithQuery]
  }

  final private case class PartialQueryImpl[O <: Args](complete: CompleteQueryTrace[O])
      extends PathWithPartialQuery[O] {
    override def &>(name: String, check: ValueCheck[Seq[String]]): PathWithPartialQuery[O] =
      PartialQueryImpl(new CompleteQueryTrace[O] {
        override def apply[O1 <: Args](next: QueryTrace[O, O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(ParamCheck(name, check, true, next))
      })
    override def &>(name: String, check: ValueCheck.Backtrack[Seq[String]]): PathWithPartialQuery[O] =
      PartialQueryImpl(new CompleteQueryTrace[O] {
        override def apply[O1 <: Args](next: QueryTrace[O, O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(ParamCheck(name, ValueCheck(check.pattern), false, next))
      })
    override def &>[A](name: String, pattern: ValuePattern[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]] =
      PartialQueryImpl(new CompleteQueryTrace[O#Append[A]] {
        override def apply[O1 <: Args](next: QueryTrace[O#Append[A], O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(ParamPattern(name, pattern, true, next, growable))
      })
    override def &>[A](name: String, pattern: ValuePattern.Backtrack[Seq[String], A])(
        implicit growable: O <:< O with Args.Growable
    ): PathWithPartialQuery[O#Append[A]] =
      PartialQueryImpl(new CompleteQueryTrace[O#Append[A]] {
        override def apply[O1 <: Args](next: QueryTrace[O#Append[A], O1]): PathTrace[Args._0, O1, PathEnd.WithQuery] =
          complete(ParamPattern(name, pattern.pattern, false, next, growable))
      })
    override def &+(check: ValueCheck[Map[String, ParamValues]]): PathWithFullQuery[O] =
      FullQueryImpl(complete(ParamsCheck(check, true, Args.same[O])))
    override def &+(check: ValueCheck.Backtrack[Map[String, ParamValues]]): PathWithFullQuery[O] =
      FullQueryImpl(complete(ParamsCheck(ValueCheck(check.pattern), false, Args.same[O])))
    override def &+[A](
        pattern: ValuePattern[Map[String, ParamValues], A]
    )(implicit growable: O <:< O with Args.Growable): PathWithFullQuery[O#Append[A]] =
      FullQueryImpl(complete(ParamsPattern(pattern, true, Args.same[O#Append[A]], growable)))
    override def &+[A](
        pattern: ValuePattern.Backtrack[Map[String, ParamValues], A]
    )(implicit growable: O <:< O with Args.Growable): PathWithFullQuery[O#Append[A]] =
      FullQueryImpl(complete(ParamsPattern(pattern.pattern, false, Args.same[O#Append[A]], growable)))
    override def trace: PathTrace[Args._0, O, PathEnd.WithQuery] = complete(QueryMatched(Args.same[O]))
  }

  final private case class FullQueryImpl[O <: Args](trace: PathTrace[Args._0, O, PathEnd.WithQuery])
      extends PathWithFullQuery[O]
}
