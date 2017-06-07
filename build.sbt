name := "libtrails-scala"

version := "1.0"

scalaVersion := "2.12.2"

resolvers ++= Seq(
  "bungee-repo" at "https://oss.sonatype.org/content/repositories/snapshots",
  "spigot-repo" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots/"
)

libraryDependencies += "org.spigotmc" % "spigot-api" % "1.11.2-R0.1-SNAPSHOT"