scalaVersion := "2.13.3"

lazy val root = (project in file(".")).
  settings(
    name := "metacache",
    version := "1.0",
    mainClass in Compile := Some("metacache.Main")
  )

cancelable in Global := true

val workaround = {
  sys.props += "packaging.type" -> "jar"
  ()
}

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}

val catsVersion       = "2.2.0"
val circeVersion      = "0.13.0"
val datastaxVersion   = "4.9.0"
val finagleVersion    = "20.9.0"
val tikaVersion       = "1.24.1"
val lz4Version        = "1.7.1"
val scalacheckVersion = "1.14.3" 

// Storage
libraryDependencies += "com.datastax.oss"        % "java-driver-core"      % datastaxVersion

// Web Server
libraryDependencies += "com.twitter"            %% "finagle-http"          % finagleVersion

// File-content reader
libraryDependencies += "org.apache.tika"         % "tika-core"             % tikaVersion
libraryDependencies += "org.apache.tika"         % "tika-parsers"          % tikaVersion

// Functional Traits
libraryDependencies += "org.typelevel"          %% "cats-core"             % catsVersion

// Functional Instances
libraryDependencies += "io.catbird"             %% "catbird-finagle"       % finagleVersion

// JSON serialisation / deserialisation
libraryDependencies += "io.circe"               %% "circe-core"            % circeVersion
libraryDependencies += "io.circe"               %% "circe-generic"         % circeVersion
libraryDependencies += "io.circe"               %% "circe-parser"          % circeVersion

// XXHash
libraryDependencies += "org.lz4"                 % "lz4-java"              % lz4Version

// Testing
libraryDependencies += "org.scalacheck"         %% "scalacheck"            % scalacheckVersion % Test
