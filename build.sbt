import sbtcrossproject.{crossProject, CrossType}

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.12", "2.12.4", "2.13.0-M3")

val rawVersion = "0.1.0"
val sharedSettings = Seq(
  organization := "com.nthportal",
  name := "eq-strawman",
  description := "Strawman implementation for equality and hash typeclasses",

  isSnapshot := true,
  version := rawVersion + { if (isSnapshot.value) "-SNAPSHOT" else "" },

  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.12", "2.12.4"),

  autoAPIMappings := true,

  scalacOptions ++= {
    if (isSnapshot.value) Seq()
    else safeSplitVersion(scalaVersion.value) match {
      case Array(2, 11, _) => Seq("-optimize")
      case Array(2, 12, patch) if patch <= 2 => Seq("-opt:l:project")
      case Array(2, 12, patch) if patch > 2 => Seq("-opt:l:inline", "-opt:l:method")
      case _ => Seq()
    }
  },
)

val publishSettings = Seq(
  publishMavenStyle := true,
  licenses := Seq("The Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://github.com/NthPortal/eq-strawman")),

  pomExtra := {
    <scm>
      <url>https://github.com/NthPortal/eq-strawman</url>
      <connection>scm:git:git@github.com:NthPortal/eq-strawman.git</connection>
      <developerConnection>scm:git:git@github.com:NthPortal/eq-strawman.git</developerConnection>
    </scm>
      <developers>
        <developer>
          <id>NthPortal</id>
          <name>NthPortal</name>
          <url>https://github.com/NthPortal</url>
        </developer>
      </developers>
  },
)

def safeSplitVersion(version: String): Array[Int] = {
  def splitDots(ver: String): Array[Int] = ver split '.' map { _.toInt }

  version split '-' match {
    case Array(v) => splitDots(v)
    case Array(v, _*) => splitDots(v)
  }
}

lazy val eql =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(sharedSettings, publishSettings)

lazy val eqlJVM    = eql.jvm
lazy val eqlJS     = eql.js
lazy val eqlNative = eql.native
