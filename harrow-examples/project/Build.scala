import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

object HarrowExamples extends Build {

  lazy val harrowCore = RootProject(
    file("..")
  )
    
  lazy val samples = Project(
    id = "harrow-examples",
    base = file("."),
    settings = Project.defaultSettings ++ harrow.sbt.HarrowSBT.harrowSettings ++ Seq(
  
    	version := "1.0",
    	organization := "io.functionall",
    
    	scalaVersion := "2.10.3",
    	scalacOptions ++= Seq("-encoding", "utf8"),
    	scalacOptions += "-deprecation",
    	javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    	javacOptions in doc := Seq("-source", "1.6"),
    	
    	harrowLazyImmutable := true,
    	harrowJavaPackage := "com.keano.harrow",
    	
    	libraryDependencies ++= Seq(
       		 "org.apache.thrift" % "libthrift" % "0.8.0"
    	),
    	
    	EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed
    		
  	)
  	
  ).dependsOn(harrowCore)
  

}
