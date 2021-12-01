import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

    val scalaTestVersion = "3.2.9"
    val scalaMockVersion = "5.1.0"

    val betterFilesVersion = "3.9.1"

    val tapirVersion = "0.19.0"
    val circeVersion = "0.13.0"

    val upickleVersion = "1.4.2"

    val scalatraVersion = "2.7.1"
    val servletApiVersion = "3.1.0"

    val shapelessVersion = "2.3.7"

    val shapeless = Seq( "com.chuusai" %% "shapeless" % shapelessVersion )

    val betterFiles = Seq( "com.github.pathikrit" %% "better-files" % betterFilesVersion )

    val scalaTest = Def.setting( Seq( "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ) )

    val scalaMock = Seq( "org.scalamock" %% "scalamock" % scalaMockVersion % "test" )

    val tapir = Def.setting( Seq( "com.softwaremill.sttp.tapir" %%% "tapir-core" % tapirVersion ) )
//                     "com.softwaremill.sttp.tapir" %% "tapir-akka-http-server" % tapirVersion  exclude("com.typesafe.akka", "akka-stream_3")  exclude("com.typesafe.akka", "akka-http_3"),
//                     "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
//                     "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % tapirVersion,
//                     "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % tapirVersion)

    val circe = Seq( "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion )

    val upickle = Def.setting( Seq( "com.lihaoyi" %%% "upickle" % upickleVersion ) )

}
