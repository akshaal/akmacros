import sbt._
import Keys._

import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._

object build extends Build {
    val sharedSettings = Defaults.defaultSettings ++ Seq(
        licenses := Seq("Apache 2.0" -> url("http://opensource.org/licenses/apache2.0.php")),
        homepage := Some(url("http://www.akshaal.info/search/label/macro")),

        organization := "info.akshaal",
        version := "0.4-SNAPSHOT",

        scalaVersion := "2.10.0-M7",
        scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
        javacOptions ++= Seq("-Xmx1536m", "-Xms512m", "-Xss10m"),
        javaOptions ++= Seq("-Xmx1536m", "-Xms512m", "-Xss10m"),

        libraryDependencies <<= scalaVersion { scala_version =>
            Seq(
                "org.scala-lang" % "scala-reflect" % scala_version % "provided",

                // Test dependencies
                "org.specs2" %% "specs2" % "1.12.1.1" % "test" cross CrossVersion.full,
                "org.scalacheck" % "scalacheck" % "1.10.0" % "test" cross CrossVersion.full
            )
        },

        ensimeConfig := sexp(
            key(":compiler-args"), sexp("-Ywarn-dead-code", "-Ywarn-shadowing"),
            key(":formatting-prefs"), sexp(
                key(":alignParameters"), true,
                key(":alignSingleLineCaseStatements"), true,
                key(":compactControlReadability"), true,
                key(":doubleIndentClassDeclaration"), true,
                key(":preserveDanglingCloseParenthesis"), true,
                key(":indentSpaces"), 4
            )
        ),

        publishMavenStyle := true,

        publishArtifact in Test := false,

        publishTo <<= version { (v: String) =>
            val nexus = "https://oss.sonatype.org/"
            if (v.trim.endsWith("SNAPSHOT"))
                Some("snapshots" at nexus + "content/repositories/snapshots")
            else
                Some("releases" at nexus + "service/local/staging/deploy/maven2")
        },

        pomExtra := (
            <scm>
                <url>git@github.com:akshaal/akmacros.git</url>
                <connection>scm:git@github.com:akshaal/akmacros.git</connection>
            </scm>
            <developers>
                <developer>
                    <id>akshaal</id>
                    <name>Evgeny Chukreev</name>
                    <url>http://akshaal.info</url>
                </developer>
            </developers>
        )
    )

    lazy val root = Project(
        id = "macros",
        base = file("."),
        settings = sharedSettings
    )
}
