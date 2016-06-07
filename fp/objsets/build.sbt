name := course.value + "-" + assignment.value

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % "test"

//koauth OAuth
libraryDependencies += "com.hunorkovacs" %% "koauth" % "1.1.0"
//libraryDependencies += "org.apache.httpcomponents" %% "httpclient" % "4.5.2"

// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

//for twitter OAuth
libraryDependencies ++= Seq(
//  "oauth.signpost" % "signpost-core" % "1.2.1.2",
//  "oauth.signpost" % "signpost-commonshttp4" % "1.2.1.2",
  "org.apache.httpcomponents" % "httpclient" % "4.5.2")

// this is important to include ScalaCheck
libraryDependencies ++= assignmentsMap.value.values.flatMap(_.dependencies).toSeq

// include the common dir
commonSourcePackages += "common"

courseId := "bRPXgjY9EeW6RApRXdjJPw"

val depsQuickcheck = Seq("org.scalacheck" %% "scalacheck" % "1.11.3")

// See documentation in ProgFunBuild.scala
assignmentsMap := {
  val styleSheetPath = (baseDirectory.value / ".." / ".." / "project" / "scalastyle_config.xml").getPath
  Map(
    "example" -> Assignment(
      packageName = "example",
      key = "g4unnjZBEeWj7SIAC5PFxA",
      itemId = "xIz9O",
      partId = "d5jxI",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath),
    "recfun" -> Assignment(
      packageName = "recfun",
      key = "SNYuDzZEEeWNVyIAC92BaQ",
      itemId = "Ey6Jf",
      partId = "PzVVY",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath,
      dependencies = depsQuickcheck),
    "funsets" -> Assignment(
      packageName = "funsets",
      key = "FNHHMDfsEeWAGiIAC46PTg",
      itemId = "BVa6a",
      partId = "IljBE",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath,
      dependencies = depsQuickcheck),
    "objsets" -> Assignment(
      packageName = "objsets",
      key = "6PTXvD99EeWAiCIAC7Pj9w",
      itemId = "Ogg05",
      partId = "7hlkb",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath,
      dependencies = depsQuickcheck,
      options = Map("grader-timeout" -> "1800")),
    "patmat" -> Assignment(
      packageName = "patmat",
      key = "BwkTtD9_EeWFZSIACtiVgg",
      itemId = "uctOq",
      partId = "2KYZc",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath,
      dependencies = depsQuickcheck),
    "forcomp" -> Assignment(
      packageName = "forcomp",
      key = "CPJe397VEeWLGArWOseZkw",
      itemId = "nVRPb",
      partId = "v2XIe",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath,
      dependencies = depsQuickcheck,
      options = Map("grader-timeout" -> "1800"))
  )
}
