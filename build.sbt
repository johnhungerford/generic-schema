import sbt._
import Dependencies._
//import sbtassembly.AssemblyPlugin.assemblySettings

lazy val projectVersion = "1.1-SNAPSHOT"

/*
   ##############################################################################################
   ##                                                                                          ##
   ##                                  SETTINGS DEFINITIONS                                    ##
   ##                                                                                          ##
   ##############################################################################################
 */

// integrationConfig and wipConfig are used to define separate test configurations for integration testing
// and work-in-progress testing
lazy val IntegrationConfig = config( "integration" ) extend( Test )
lazy val WipConfig = config( "wip" ) extend( Test )

lazy val commonSettings =
    inConfig( IntegrationConfig )( Defaults.testTasks ) ++
    inConfig( WipConfig )( Defaults.testTasks ) ++
    Seq(
        version := projectVersion,
        description := "A generic schema library for describing data objects",
        licenses := List( "Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt" ) ),
        homepage := Some( url( "https://johnhungerford.github.io" ) ),
        startYear := Some( 2021 ),
        scalaVersion := "3.2.1-RC1-bin-20220823-3ad97df-NIGHTLY",
        resolvers ++= Seq( "Maven Central" at "https://repo1.maven.org/maven2/",
            "JCenter" at "https://jcenter.bintray.com",
            "Local Ivy Repository" at s"file://${System.getProperty( "user.home" )}/.ivy2/local/default" ),
        javacOptions ++= Seq( "-source", "1.8", "-target", "1.8" ),
        scalacOptions += "-target:jvm-1.8",
        useCoursier := false,
        libraryDependencies ++= scalaTest.value,
        // `sbt test` should skip tests tagged IntegrationTest
        Test / testOptions := Seq( Tests.Argument( "-l", "org.hungerford.generic.schema.test.tags.IntegrationTest" ) ),
        // `sbt integration:test` should run only tests tagged IntegrationTest
        IntegrationConfig / parallelExecution := false,
        IntegrationConfig / testOptions := Seq( Tests.Argument( "-n", "org.hungerford.generic.schema.test.tags.IntegrationTest" ) ),
        // `sbt wip:test` should run only tests tagged WipTest
        WipConfig / testOptions := Seq( Tests.Argument( "-n", "org.hungerford.generic.schema.test.tags.WipTest" ) ),
    )

val Snapshot = "-SNAPSHOT".r

lazy val publishSettings = Seq(
    credentials += Credentials( Path.userHome / ".sbt" / "sonatype_credentials" ),
    organization := "io.github.johnhungerford.generic.schema",
    organizationName := "johnhungerford",
    organizationHomepage := Some( url( "https://johnhungerford.github.io" ) ),
    pomIncludeRepository := { _ => false },
    scmInfo := Some(
        ScmInfo(
            url("https://github.com/johnhungerford/generic-schema"),
            "scm:git@github.com:johnhungerford/generic-schema.git"
        )
    ),
    developers := List(
        Developer(
            id    = "johnhungerford",
            name  = "John Hungerford",
            email = "jiveshungerford@gmail.com",
            url   = url( "https://johnhungerford.github.io" )
        )
    ),
    publishTo := {
        val nexus = "https://s01.oss.sonatype.org/"
        if ( isSnapshot.value ) Some( "snapshots" at nexus + "content/repositories/snapshots" )
        else Some( "releases" at nexus + "service/local/staging/deploy/maven2" )
    },
    ThisBuild / publishMavenStyle := true,
)

lazy val disablePublish = Seq(
    publish := {}
)

lazy val disableBuild = Seq(
    Docker / publish := {}
)

lazy val buildSettings = Seq(
    assembly / assemblyMergeStrategy := {
        case PathList( "META-INF", "MANIFEST.MF" ) => MergeStrategy.discard
        case PathList( "reference.conf" ) => MergeStrategy.concat
        case x => MergeStrategy.last
    },
    assembly / test := {},
    Compile / run / mainClass := Some( "Main" ),
    dockerBaseImage := "openjdk:8",
    dockerUpdateLatest := true,
    dockerUsername := Some( "johnhungerford" ),
)

/*
   ##############################################################################################
   ##                                                                                          ##
   ##                                  PROJECT DEFINITIONS                                     ##
   ##                                                                                          ##
   ##############################################################################################
 */

lazy val root = ( project in file( "." ) )
  .disablePlugins( sbtassembly.AssemblyPlugin, SbtPgp )
  .aggregate(
      core.projects( JVMPlatform ),
      core.projects( JSPlatform ),
      gsUPickle.projects( JVMPlatform ),
      gsUPickle.projects( JSPlatform ),
      gsCirce.projects( JVMPlatform ),
      gsCirce.projects( JSPlatform ),
      gsTapir.projects( JVMPlatform ),
      gsTapir.projects( JSPlatform ),
  )
  .settings(
      name := "generic-schema",
      disablePublish,
      disableBuild,
  )

lazy val coreMacros = ( crossProject( JSPlatform, JVMPlatform ) in file( "core-macros") )
  .configs( IntegrationConfig, WipConfig )
  .disablePlugins( sbtassembly.AssemblyPlugin )
  .settings(
      name := "core-macros",
      commonSettings,
      publishSettings,
      disableBuild,
  )

lazy val core = ( crossProject( JSPlatform, JVMPlatform ) in file( "core" ) )
  .configs( IntegrationConfig, WipConfig )
  .disablePlugins( sbtassembly.AssemblyPlugin )
  .dependsOn( coreMacros )
  .settings(
      name := "core",
      commonSettings,
      publishSettings,
      disableBuild,
  )

lazy val gsUPickle = ( crossProject( JSPlatform, JVMPlatform ) in file( "gs-upickle" ) )
  .configs( IntegrationConfig, WipConfig )
  .dependsOn( core % "compile->compile;test->test" )
  .disablePlugins( sbtassembly.AssemblyPlugin )
  .settings(
      name := "gs-upickle",
      commonSettings,
      publishSettings,
      disableBuild,
      libraryDependencies ++= upickle.value,
  )

lazy val gsCirce = ( crossProject( JSPlatform, JVMPlatform ) in file( "gs-circe" ) )
  .configs( IntegrationConfig, WipConfig )
  .dependsOn( core % "compile->compile;test->test" )
  .disablePlugins( sbtassembly.AssemblyPlugin )
  .settings(
      name := "gs-circe",
      commonSettings,
      publishSettings,
      disableBuild,
      libraryDependencies ++= circeCore.value ++ circeParserTest.value,
  )

lazy val gsTapir = ( crossProject( JSPlatform, JVMPlatform ) in file( "gs-tapir" ) )
  .configs( IntegrationConfig, WipConfig )
  .dependsOn( core % "compile->compile;test->test" )
  .disablePlugins( sbtassembly.AssemblyPlugin )
  .settings(
      name := "gs-tapir",
      commonSettings,
      publishSettings,
      disableBuild,
      libraryDependencies ++= tapir.value,
  )

lazy val exampleApp = ( crossProject( JSPlatform, JVMPlatform ) in file( "example-app" ) )
  .configs( IntegrationConfig, WipConfig )
  .dependsOn( core % "compile->compile;test->test", gsTapir, gsCirce )
  .settings(
      name := "example-app",
      commonSettings,
      disablePublish,
      libraryDependencies ++= circeCore.value ++ circeParser.value,
  )
  .jvmSettings(
      buildSettings,
      libraryDependencies ++= tapir.value ++ tapirCirce.value ++ tapirOpenApi ++ tapirZioHttp,
  )
  .jsSettings(
      scalaJSUseMainModuleInitializer := true,
  )
