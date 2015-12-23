lazy val root = (project in file(".")).
  settings(
    name := "trees",
    version := "0.0.1",
    scalaVersion := "2.10.5"
  )

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.6" % "test",
							"org.specs2" %% "specs2-junit" % "3.6.6" % "test",
							"org.specs2" %% "specs2-scalacheck" % "3.6.6" % "test",
							"junit" % "junit" % "4.8.1" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")