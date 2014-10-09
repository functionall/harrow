package harrow.sbt

import sbt._
import Keys._
import java.io.File
import harrow.core.HarrowData
import harrow.core.Harrow
import harrow.core.ThriftIDLTemplate
import harrow.core.FlyweightTemplate
import harrow.core.TReadProtocolTemplate

/**
 * SBT plugin for Harrow..
 */
object HarrowSBT extends Plugin {

  val harrowSourceDirectory = SettingKey[File]("harrow-source-directory", "Source directory for harrow files. Defaults to src/main/harrow")
  val harrowOutputDirectory = SettingKey[File]("harrow-output-directory", "Directory where the generated files should be placed. defaults to sourceManaged")

  val harrowJavaPackage = SettingKey[String]("harrow-java-package", "The package namespace to use for generated code")
  val harrowLazyImmutable = SettingKey[Boolean]("harrow-lazy-immutable", "Option to generate lazy immutable style (impure) flyweight code")

  val harrowGenerateIDL = TaskKey[Seq[File]]("harrow-generate-idl", "Generate Thrift IDL from a harrow script")
  val harrowGenerateFly = TaskKey[Seq[File]]("harrow-generate-fly", "Generate flyweight implementation from a harrow script")
  val harrowGenerateTRead = TaskKey[Seq[File]]("harrow-generate-tread", "Generate Thrift TProtocol implemenation from a harrow script")

  lazy val harrowSettings: Seq[Setting[_]] = Seq[Setting[_]](

    harrowSourceDirectory <<= (sourceDirectory in Compile) { _ / "harrow" },
    harrowOutputDirectory <<= (sourceManaged in Compile),
    harrowLazyImmutable := false,
    harrowJavaPackage := "",

    harrowGenerateIDL <<= (streams, harrowSourceDirectory, harrowOutputDirectory, harrowJavaPackage) map {
      (out, source, target, namespace) =>
        generateIDL(source, target, namespace, out.log, out.cacheDirectory / "idl")
    },

    harrowGenerateFly <<= (streams, harrowSourceDirectory, harrowOutputDirectory, harrowJavaPackage, harrowLazyImmutable) map {
      (out, source, target, namespace, immutable) =>
        generateFlys(source, target, namespace, immutable, out.log, out.cacheDirectory / "fly")
    },

    harrowGenerateTRead <<= (streams, harrowSourceDirectory, harrowOutputDirectory, harrowJavaPackage, harrowLazyImmutable) map {
      (out, source, target, namespace, immutable) =>
        generateTReads(source, target, namespace, immutable, out.log, out.cacheDirectory / "tproto")
    },

    managedClasspath <<= (classpathTypes, update) map { (cpt, up) =>
      Classpaths.managedJars(Compile, cpt, up)
    }
    
   ) ++ Seq[Setting[_]](
      resourceGenerators in Compile <+= harrowGenerateIDL,
      sourceGenerators in Compile <+= harrowGenerateFly,
      sourceGenerators in Compile <+= harrowGenerateTRead,
      watchSources <++= (harrowSourceDirectory) map { (tdir) => (tdir ** "*.harrow").get }
   )

  /**
   *
   */
  def generateIDL(sourceDir: File, idlDir: File, namespace: String, logger: Logger, cache: File): Seq[File] = {

    val doIt = FileFunction.cached(cache, inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { files: Set[File] =>
      if (!idlDir.exists)
        idlDir.mkdirs
      files.foreach { script =>
        logger.info(s"Harrow :: compiling harrow DSL to thrift: ${script.getName()}")
        val model = Harrow.loadModel(script)
        ThriftIDLTemplate.generate(model, namespace, idlDir.getPath(), script.getName())
      }
      (idlDir ** "*.thrift").get.toSet
    }
    doIt((sourceDir ** "*.harrow").get.toSet).toSeq

  }

  /**
   *
   */
  def compileThrift(sourceDir: File, thriftDir: File, thriftBin: String,
    language: String, options: Seq[String], logger: Logger, cache: File): Seq[File] = {

    val doIt = FileFunction.cached(cache, inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { files: Set[File] =>
      if (!thriftDir.exists)
        thriftDir.mkdirs
      val opts = language + options.mkString(":", ",", "")
      files.foreach { schema =>
        val cmd = s"$thriftBin -gen $opts -o ${thriftDir.getPath()} ${schema.getName()}"
        logger.info(s"Harrow :: compiling thrift IDL with command: $cmd")
        cmd ! logger
      }
      (thriftDir ** "*.java").get.toSet
    }
    doIt((sourceDir ** "*.harrow").get.toSet).toSeq

  }

  /**
   *
   */
  def generateFlys(sourceDir: File, flyDir: File, namespace: String, useImmutable: Boolean, logger: Logger, cache: File): Seq[File] = {

    val doIt = FileFunction.cached(cache, inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { files: Set[File] =>
      if (!flyDir.exists)
        flyDir.mkdirs
      files.foreach { script =>
        logger.info(s"Harrow :: generating flyweights from harrow DSL : ${script.getName()}")
        val model = Harrow.loadModel(script)
        FlyweightTemplate.generate(model, namespace, flyDir.getAbsolutePath(), useImmutable)
      }
      (flyDir ** "*Fly.scala").get.toSet
    }
    doIt((sourceDir ** "*.harrow").get.toSet).toSeq

  }

  /**
   *
   */
  def generateTReads(sourceDir: File, treadsDir: File, namespace: String, useImmutable: Boolean, logger: Logger, cache: File): Seq[File] = {

    val doIt = FileFunction.cached(cache, inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { files: Set[File] =>
      if (!treadsDir.exists)
        treadsDir.mkdirs
      files.foreach { script =>
        logger.info(s"Harrow :: generating tprotocol from harrow DSL : ${script.getName()}")
        val model = Harrow.loadModel(script)
        TReadProtocolTemplate.generate(model, namespace, treadsDir.getAbsolutePath(), useImmutable)
      }
      (treadsDir ** "TReads*.scala").get.toSet
    }
    doIt((sourceDir ** "*.harrow").get.toSet).toSeq

  }

}