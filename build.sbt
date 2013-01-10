name := "Neurogenesis"

version := "0.1-beta"

scalaVersion := "2.9.1"


libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  deps :+ ("org.scala-lang" % "scala-compiler" % sv)
}

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.1" =>
      deps :+ ("jline" % "jline" % "0.9.94") // ("org.scala-lang" % "jline" % "2.9.1")
    case x if x.startsWith("2.8") =>
      deps :+ ("jline" % "jline" % "0.9.94")
    case x       => error("Unsupported Scala version " + x)
  }
}

// JUNG
libraryDependencies ++= Seq("io", "algorithms", "api", "visualization", "graph-impl").map { sub =>
  "net.sf.jung" % ("jung-" + sub) % "2.0.1" 
}

// Scalala
libraryDependencies += "org.scalala" %% "scalala" % "1.0.0.RC2" // "1.0.0.RC3-SNAPSHOT"

// scala.swing
libraryDependencies <+= scalaVersion { sv => "org.scala-lang" % "scala-swing" % sv }

javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

scalacOptions ++= Seq("-no-specialization","-deprecation","-target:jvm-1.5")

mainClass in (Compile,packageBin) := Some("neurogenesis.doubleprecision.EvoLauncher")

javaOptions += "-Xmx2g"
