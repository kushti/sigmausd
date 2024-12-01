name := "sigmausd"

version := "0.1"

scalaVersion := "2.13.12"

resolvers ++= Seq(
  "Bintray" at "https://jcenter.bintray.com/", //for org.ethereum % leveldbjni-all
  "Typesafe maven releases" at "https://dl.bintray.com/typesafe/maven-releases/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "com.squareup.okhttp3" % "mockwebserver" % "4.12.0",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
  "org.mockito" % "mockito-core" % "5.11.0" % Test
)

val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "org.ergoplatform" %% "ergo-appkit" % "5.0.4",
  "org.ergoplatform" %% "kiosk" % "1.0.0",
  "org.ergoplatform" %% "ergo-core" % "5.0.20"
)