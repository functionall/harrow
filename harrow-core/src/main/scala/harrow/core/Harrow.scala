package harrow.core

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import java.io.File

/**
 *
 */
case class HarrowContext(inputFile: String = "", outputFolder: String = "", namespace: String = "", lazyImmutable: Boolean = false, doThrift: Boolean = true, doFly: Boolean = false, doTReads: Boolean = false)

/**
 * Executor for the Harrow code generator
 */
object Harrow extends HarrowDSL {

  lazy val parser = new scopt.OptionParser[HarrowContext]("harrow") {
    head("harrow", "1.0")
    opt[String]('s', "source") required () valueName ("<source>") action { (x, c) =>
      c.copy(inputFile = x)
    } text ("Harrow DSL script")
    opt[String]('o', "output") required () valueName ("<folder>") action { (x, c) =>
      c.copy(outputFolder = x)
    } text ("destination folder to generate code to (default is target)")
    opt[String]('n', "namespace") required () valueName ("<namespace>") action { (x, c) =>
      c.copy(namespace = x)
    } text ("java package namespace to generate code to")
    opt[Unit]('l', "lazy") optional () action { (x, c) =>
      c.copy(lazyImmutable = true)
    } text ("specify to generate lazy immutable classes")
    opt[Unit]('t', "thrift") optional () action { (x, c) =>
      c.copy(doThrift = true)
    } text ("specify to generate Thrift IDL (default is true)")
    opt[Unit]('f', "fly") optional () action { (x, c) =>
      c.copy(doFly = true)
    } text ("specify to generate flyweights (default is false)")
    opt[Unit]('r', "treads") optional () action { (x, c) =>
      c.copy(doFly = true)
    } text ("specify to generate TProtocol(read-only) code (default is false)")
  }

  /**
   *
   */
  def main(args: Array[String]) {

    parser.parse(args, HarrowContext()) map { parseArgs =>

      println(s"Parsing DSL from ${parseArgs.inputFile}")
      
      val inputFile = new File(parseArgs.inputFile)
      val model = loadModel(inputFile)
      val outputFolder = new File(parseArgs.outputFolder)

      // generate the code

      if (parseArgs.doThrift) {
        val thriftFolder = new File(outputFolder, "idl-gen")
        thriftFolder.mkdirs()
        println("Generating Thrift IDL")
        ThriftIDLTemplate.generate(model, parseArgs.namespace, thriftFolder.getAbsolutePath(), inputFile.getCanonicalFile().getName())
      }

      if (parseArgs.doFly) {
        val flyFolder = new File(outputFolder, "fly-gen")
        flyFolder.mkdirs()
        println("Generating Flyweights")
        FlyweightTemplate.generate(model, parseArgs.namespace, flyFolder.getAbsolutePath())
      }

      if (parseArgs.doTReads) {
        val tReadsFolder = new File(outputFolder, "tprotocol-gen")
        tReadsFolder.mkdirs()
        println("Generating TProtocol (read) implementation")
        TReadProtocolTemplate.generate(model, parseArgs.namespace, tReadsFolder.getAbsolutePath())
      }

    }

  }
  
  /**
   * 
   */
  def loadModel(script: File): HarrowData = {
    
    def parseScript(input: File): HarrowData = {
        val folderPath = input.getParent()
        val inputSource = io.Source.fromFile(input)
        val inputAsString = inputSource.mkString
        inputSource.close
        parseAll(dataModel, inputAsString) match {
          case Success(result, _) =>
            val includedfiles = result.headers.collect { case Include(f) => f }
            val models = includedfiles.map(includedFile => {
              val f = if (!includedFile.startsWith(folderPath))
                new File(folderPath, includedFile)
              else
                new File(includedFile)
              parseScript(f)
            })
            models.foldLeft(result)((model, included) => model += included)
          case NoSuccess(msg, next) =>
            throw new HarrowException(s"Failed to parse harrow DSL ::: $msg ?? ${next.pos}")
        }
      }

      val definition = parseScript(script)
      definition.validate
      definition.resolve
      
  }
  
  

}