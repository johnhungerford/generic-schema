import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

    val scalaTestVersion = "3.2.9"
    val scalaMockVersion = "5.1.0"

    val betterFilesVersion = "3.9.1"

    val tapirVersion = "0.19.1"
    val circeVersion = "0.14.1"

    val upickleVersion = "1.4.2"

    val betterFiles = Seq( "com.github.pathikrit" %% "better-files" % betterFilesVersion )

    val scalaTest = Def.setting( Seq( "org.scalatest" %%% "scalatest" % scalaTestVersion % "test" ) )

    val scalaMock = Seq( "org.scalamock" %% "scalamock" % scalaMockVersion % "test" )

    val tapir = Def.setting( Seq(
        "com.softwaremill.sttp.tapir" %%% "tapir-core" % tapirVersion,
    ) )

    val tapirCirce = Def.setting( Seq(
        "com.softwaremill.sttp.tapir" %%% "tapir-json-circe" % tapirVersion,
    ) )

    val tapirOpenApi = Seq(
        "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % tapirVersion,
        "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % tapirVersion,
    )

    val circeCore = Def.setting( Seq(
        "io.circe" %%% "circe-core" % circeVersion,
    ) )

    val circeParser = Def.setting( Seq(
        "io.circe" %%% "circe-parser" % circeVersion % "test",
    ) )

    val upickle = Def.setting( Seq( "com.lihaoyi" %%% "upickle" % upickleVersion ) )

}
