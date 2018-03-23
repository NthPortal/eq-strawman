logLevel := Level.Warn

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "0.6.21")
addSbtPlugin("org.portable-scala" % "sbt-crossproject"         % "0.3.1")  // (1)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.3.1")  // (2)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.2")
