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

trait PathParser {
  def nextSegment: PathParser.NextSegment
  def restOfPath: PathParser.RestOfPath
}

object PathParser {
  sealed trait NextSegment
  final case class Segment(segment: String, parser: PathParser) extends NextSegment
  final case class PathEnd(parser: QueryParser) extends NextSegment
  final case class Failure(error: String) extends NextSegment with RestOfPath

  sealed trait RestOfPath
  final case class Segments(segments: String, parser: QueryParser) extends RestOfPath
}
