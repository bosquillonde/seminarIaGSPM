lazy val root = (project in file(".")).
  settings(
    name := "oscar-cp template",
    scalaVersion := "2.11.4",
    resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/",
    libraryDependencies += "oscar" %% "oscar-cp" % "3.1.0-SNAPSHOT" withSources()
  )