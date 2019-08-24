// Only run WartRemover on 2.12
def mojitoWarts(sv: String) =
  CrossVersion.partialVersion(sv) match {
    case Some((2, 12)) =>
      Warts.allBut(
        Wart.Any,                // false positives
        Wart.Nothing,            // false positives
        Wart.ImplicitConversion  // we know what we're doing
      )
    case _ => Nil
  }

lazy val buildSettings = Seq(
  name := "mojito",
  version := "0.0.1",
  organization := "org.billzabob",
  libraryDependencies += "org.tpolecat" %% "atto-core"  % "0.6.5",
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",
  libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"
  licenses ++= Seq(
    ("MIT", url("http://opensource.org/licenses/MIT"))
  ),
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value)
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary)

lazy val commonSettings =
  Seq(
    wartremoverErrors in (Compile, compile) := mojitoWarts(scalaVersion.value),
    wartremoverErrors in (Test,    compile) := mojitoWarts(scalaVersion.value),
    parallelExecution in Test := false
  )

lazy val mojito = project.in(file(".")).settings(buildSettings ++ commonSettings)
