name := "Pokedex-CLEAN"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.3.6",
  "com.squareup.okhttp3" % "okhttp" % "4.10.0",
  "io.monix" %% "monix-execution" % "3.4.0",
  "io.monix" %% "monix-eval" % "3.4.0",
  "io.github.juliano" % "pokeapi-scala_3" % "0.1.0",
  "org.scalactic" %% "scalactic" % "3.2.12",
  "org.scalatest" %% "scalatest" % "3.2.12" % "test",
  "com.typesafe.play" %% "play-json" % "2.8.0"
)