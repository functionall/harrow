import sbt._
import Keys._
import net.virtualvoid.sbt.cross.CrossPlugin

object Harrow extends Build {


  lazy val publishM2Configuration =
    TaskKey[PublishConfiguration]("publish-m2-configuration",
      "Configuration for publishing to the .m2 repository.")

  lazy val publishM2 =
    TaskKey[Unit]("publish-m2",
      "Publishes artifacts to the .m2 repository.")

  lazy val m2Repo =
    Resolver.file("publish-m2-local",
      Path.userHome / ".m2" / "repository")

 	
  val sharedSettings = Seq(
  
    version := "1.0",
    organization := "io.functionall",
    

    resolvers ++= Seq(
      "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
      "dtrotts" at "http://maven.davidtrott.com/repository",
      "twitter-repo" at "http://maven.twttr.com"
    ),

    publishM2Configuration <<= (packagedArtifacts, checksums in publish, ivyLoggingLevel) map { (arts, cs, level) =>
      Classpaths.publishConfig(arts, None, resolverName = m2Repo.name, checksums = cs, logging = level)
    },
    publishM2 <<= Classpaths.publishTask(publishM2Configuration, deliverLocal),
    otherResolvers += m2Repo,

    publishMavenStyle := true,
        
    libraryDependencies ++= Seq(
		"com.dongxiguo" %% "fastring" % "0.2.1",
        "org.apache.thrift" % "libthrift" % "0.8.0",
        "com.github.scopt" %% "scopt" % "3.2.0"
    ),
    
    crossScalaVersions := Seq("2.9.2", "2.10.3"),
     
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq("-encoding", "utf8"),
    scalacOptions += "-deprecation",
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    javacOptions in doc := Seq("-source", "1.6")
	
  )


  lazy val crossBuildSettings: Seq[Setting[_]] = CrossPlugin.crossBuildingSettings ++ CrossBuilding.scriptedSettings ++ Seq(
    CrossBuilding.crossSbtVersions := Seq("0.12", "0.13")
  )

  lazy val harrow = Project(
    id = "harrow",
    base = file("."),
    settings = Project.defaultSettings 
  ).aggregate(
    harrowCore,
    harrowSbtPlugin  
  )
  
  lazy val harrowCore = Project(
    id = "harrow-core",
    base = file("harrow-core"),
    settings = Project.defaultSettings ++ sharedSettings
  )
  
  lazy val harrowSbtPlugin = Project(
    id = "harrow-sbt-plugin",
    base = file("harrow-sbt-plugin"),
    settings = Project.defaultSettings ++ sharedSettings 
  ).settings(
    sbtPlugin := true,
    publishMavenStyle := false
  ).dependsOn(harrowCore)
  

}
