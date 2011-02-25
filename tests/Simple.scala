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

package com.github.mvv.routineer.tests

import com.github.mvv.routineer._
import org.specs._

object SimpleSpec extends Specification {
  "Empty route set" in {
    val rs = Routes[Any, Any]()
    rs((), "") must_== None
    rs((), "/") must_== None
    rs((), "aaaa") must_== None
    rs((), "/aaaa") must_== None
    rs((), "/aaaa/bbbb") must_== None
    rs((), "/aaaa/bbbb/") must_== None
  }

  "Empty path" in {
    val rs = Routes[Any, Any](PathSpec.empty when (r => r))
    rs(1, "").map(_.apply) must_== Some(1)
    rs(2, "/").map(_.apply) must_== Some(2)
    rs((), "/aaaa") must_== None
    rs((), "/aaaa/bbbb") must_== None
  }

  "Const path" in {
    val rs = Routes[Any, Any]("a" whenDo (r => r))
    rs((), "") must_== None
    rs((), "a").isDefined must_== true
    rs((), "/a").isDefined must_== true
    rs((), "/a/").isDefined must_== true
    rs((), "/a/b").isDefined must_== false
  }

  "Star pattern" in {
    val rs1 = Routes("a" /> * when ((_: Any, s) => s))
    rs1((), "") must_== None
    rs1((), "/") must_== None
    rs1((), "/a") must_== None
    rs1((), "/aaaa") must_== None
    rs1((), "/a/bbbb").map(_.apply) must_== Some("bbbb")
    val rs2 = Routes("a" /> * /> * when ((_: Any, s1, s2) => s1 + s2))
    rs2((), "") must_== None
    rs2((), "/") must_== None
    rs2((), "/a/bbbb") must_== None
    rs2((), "/a/bbbb/") must_== None
    rs2((), "/a/bbbb/cccc").map(_.apply) must_== Some("bbbbcccc")
    rs2((), "/a/bbbb/cccc/d") must_== None
    val rs3 = Routes(* when ((_: Any, s) => s))
    rs3((), "") must_== None
    rs3((), "aaaa").map(_.apply) must_== Some("aaaa")
    rs3((), "/aaaa").map(_.apply) must_== Some("aaaa")
    rs3((), "/aaaa/").map(_.apply) must_== Some("aaaa")
    rs3((), "/aaaa/bbbb") must_== None
    val rs4 = Routes(* /> "b" when ((_: Any, s) => s))
    rs4((), "") must_== None
    rs4((), "/") must_== None
    rs4((), "/b") must_== None
    rs4((), "/aaaa") must_== None
    rs4((), "/aaaa/b").map(_.apply) must_== Some("aaaa")
  }

  "Maximum path spec length" in {
    val rs0 = Routes[Any, String](PathSpec.empty when (_ => ""))
    rs0((), "").map(_.apply) must_== Some("")
    val rs1 = Routes(* when ((_: Any, s1) => s1))
    rs1((), "a").map(_.apply) must_== Some("a")
    val rs2 = Routes(* /> * when ((_: Any, s1, s2) => s1 + s2))
    rs2((), "a/b").map(_.apply) must_== Some("ab")
    val rs3 = Routes(* /> * /> * when ((_: Any, s1, s2, s3) => s1 + s2 + s3))
    rs3((), "a/b/c").map(_.apply) must_== Some("abc")
    val rs4 = Routes(* /> * /> * /> * when {
                       (_: Any, s1, s2, s3, s4) => s1 + s2 + s3 + s4
                     })
    rs4((), "a/b/c/d").map(_.apply) must_== Some("abcd")
    val rs5 = Routes(* /> * /> * /> * /> * when {
                       (_: Any, s1, s2, s3, s4, s5) => s1 + s2 + s3 + s4 + s5
                     })
    rs5((), "a/b/c/d/e").map(_.apply) must_== Some("abcde")
    val rs6 = Routes(* /> * /> * /> * /> * /> * when {
                       (_: Any, s1, s2, s3, s4, s5, s6) =>
                         s1 + s2 + s3 + s4 + s5 + s6
                     })
    rs6((), "a/b/c/d/e/f").map(_.apply) must_== Some("abcdef")
    val rs7 = Routes(* /> * /> * /> * /> * /> * /> * when {
                       (_: Any, s1, s2, s3, s4, s5, s6, s7) =>
                         s1 + s2 + s3 + s4 + s5 + s6 + s7
                     })
    rs7((), "a/b/c/d/e/f/g").map(_.apply) must_== Some("abcdefg")
  }
}
