import sbt._
import Keys._

import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._

object build extends Build {
    val sharedSettings = Defaults.defaultSettings ++ Seq(
        organization := "info.akshaal",
        version := "0.1",
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
        ))

    lazy val root = Project(
        id = "macros",
        base = file("."),
        settings = sharedSettings
    )
}
