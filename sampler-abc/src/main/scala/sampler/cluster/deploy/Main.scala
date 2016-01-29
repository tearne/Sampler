package sampler.cluster.deploy

import java.nio.file.Paths
import scala.sys.process._
import org.apache.commons.io.FileUtils
import sampler.cluster.deploy.bash.Rsync
import sampler.cluster.deploy.bash.Script
import java.nio.file.Path
import java.io.File
import sampler.cluster.deploy.bash.SSH

trait Deployer {

  trait Operation
  case object Augment extends Operation
  case object Destroy extends Operation
  case object Redeploy extends Operation
  
  import scopt.Read
  object Operation{
    def fromString(str: String): Option[Operation] = {
      Vector(Augment, Destroy, Redeploy).find(_.toString == str)
    }
    
    implicit val operationRead: Read[Operation] = 
      Read.reads[Operation]{str => 
        fromString(str).getOrElse(throw new Exception("Could not parse: "+str))
      }
  }
  
  case class Job(
      configFile: File, 
      clusterTag: String, 
      operation: Operation
  )
  
  val parser = new scopt.OptionParser[Job]("scopt") { // need case class for Config?
    head("scopt", "3.x")
    
    opt[File]('c', "config") required() valueName("<file>") action{ (x, c) =>
      c.copy(configFile = x)
    } text ("cluster properties file is required")
    
    opt[String]('n', "nodes") action { (x, c) =>
      c.copy(clusterTag = x)
    } text ("cluster is name of cluster instance")
    
    opt[Operation]('o', "operation") required() valueName("<task>") action{ (x,c) =>
      c.copy(operation = x)  
    } text ("message")
  }
  
  def run(args: Array[String], provider: Provider) {
    
  }

  val defaultFileName = "cluster.json"
  val path = Paths.get(System.getProperty("user.home"), defaultFileName)

  val jobOption = parser.parse(args, Job(path.toFile(), "", Destroy))
  println("Config = "+jobOption.get)

  System.exit(0)

  jobOption.foreach{job =>
    
    job.operation match {
      case Augment => ???
      case Destroy => ???
      case Redeploy => ???
    }
  }  
  
  def go(job: Job, configFile: File) {
    val configString = FileUtils.readFileToString(configFile)
    val props = Properties.fromJSON(configString)
    val provider: Provider = SoftLayer.buildProvider(configString)
    
    val ip = (node: Node) => node.publicIp.get
    val nodes = provider.getAllNodes.filter(_.clusterName == job.clusterTag)
    println(s"Found ${nodes.size} nodes: ")
    nodes.map("  " + _.toString).foreach(println)

    val seeds = List(
      Util.getAssertOne(nodes.filter(_.seedRole == Some("seed1"))),
      Util.getAssertOne(nodes.filter(_.seedRole == Some("seed2")))
    )
    
    val ssh = new SSH(props.privateKeyPath)
    val rsync = new Rsync(props.privateKeyPath)
    
    def tearDown(nodes: Set[Node]){
      nodes.foreach{node =>
        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.killJava(props.applicationMain)
        )).!

        //TODO not needed if rsync deletes?
//        Process(ssh.foregroundCommand(
//            provider.instanceUser, 
//            ip(node), 
//            Script.deleteOld(props.payloadTarget)
//        )).!
      }
    }
    
    def deploy(nodes: Set[Node]){
      def isRunningABC(node: Node): Boolean = {
        val process = Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.checkJavaRunning(props.applicationMain))).!
        if (process == 0) true else false //Assuming exit code of 0 means model is running
      }
      
      def upload(node: Node, props: Properties, runScriptPath: Path) {
        Process(rsync(
          provider.instanceUser,
          ip(node),
          props.payloadLocal,
          props.payloadTargetParent)).!

        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.unTar("~/deploy", "dataIn.tar.gz"))).!

        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.unTar("~/deploy", "jdk-8u71-linux-x64.tar.gz"))).!

        Process(ssh.scpCommand(
          provider.instanceUser,
          ip(node),
          runScriptPath,
          props.payloadTarget)).!
      }

      def execute(node: Node, props: Properties, runScriptName: String) {
        //TODO logging.  Otherwise commands can silently fail
        val cmd = ssh.backgroundCommand(
          provider.instanceUser,
          ip(node),
          s"${props.payloadTarget}/$runScriptName")
        println(cmd)
        Process(cmd).!
      }
      
      nodes
        .filter{!isRunningABC(_)}
        .foreach { node =>
          val runScript: String = Script.startApplication(
            ip(node),
            props.vmExtraArgs,
            props.applicationMain,
            ip(seeds(0)),
            ip(seeds(1)))
          val runScriptPath = Util.writeToTempFile(runScript, "run", ".sh")
    
          upload(node, props, runScriptPath)
          execute(node, props, runScriptPath.getFileName.toString)
        }
    }
    
    job.operation match {
      case Destroy => 
        tearDown(nodes)
      case Redeploy => 
        tearDown(nodes)
        deploy(nodes)
      case Augment => 
        deploy(nodes)
    }
  }
    
}

object Main extends App {

  trait Operation
  case object Augment extends Operation
  case object Destroy extends Operation
  case object Redeploy extends Operation
  
  import scopt.Read
  object Operation{
    def fromString(str: String): Option[Operation] = {
      Vector(Augment, Destroy, Redeploy).find(_.toString == str)
    }
    
    implicit val operationRead: Read[Operation] = 
      Read.reads[Operation]{str => 
        fromString(str).getOrElse(throw new Exception("Could not parse: "+str))
      }
  }
  
  case class Job(
      configFile: File, 
      clusterTag: String, 
      operation: Operation
  )
 
  val parser = new scopt.OptionParser[Job]("scopt") { // need case class for Config?
    head("scopt", "3.x")
    
    opt[File]('c', "config") required() valueName("<file>") action{ (x, c) =>
      c.copy(configFile = x)
    } text ("cluster properties file is required")
    
    opt[String]('n', "nodes") action { (x, c) =>
      c.copy(clusterTag = x)
    } text ("cluster is name of cluster instance")
    
    opt[Operation]('o', "operation") required() valueName("<task>") action{ (x,c) =>
      c.copy(operation = x)  
    } text ("message")
  }

  val defaultFileName = "cluster.json"
  val path = Paths.get(System.getProperty("user.home"), defaultFileName)

  val jobOption = parser.parse(args, Job(path.toFile(), "", Destroy))
  println("Config = "+jobOption.get)

  System.exit(0)

  jobOption.foreach{job =>
    
    job.operation match {
      case Augment => ???
      case Destroy => ???
      case Redeploy => ???
    }
  }  
  
  def go(job: Job, configFile: File) {
    val configString = FileUtils.readFileToString(configFile)
    val props = Properties.fromJSON(configString)
    val provider: Provider = SoftLayer.buildProvider(configString)
    
    val ip = (node: Node) => node.publicIp.get
    val nodes = provider.getAllNodes.filter(_.clusterName == job.clusterTag)
    println(s"Found ${nodes.size} nodes: ")
    nodes.map("  " + _.toString).foreach(println)

    val seeds = List(
      Util.getAssertOne(nodes.filter(_.seedRole == Some("seed1"))),
      Util.getAssertOne(nodes.filter(_.seedRole == Some("seed2")))
    )
    
    val ssh = new SSH(props.privateKeyPath)
    val rsync = new Rsync(props.privateKeyPath)
    
    def tearDown(nodes: Set[Node]){
      nodes.foreach{node =>
        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.killJava(props.applicationMain)
        )).!

        //TODO not needed if rsync deletes?
//        Process(ssh.foregroundCommand(
//            provider.instanceUser, 
//            ip(node), 
//            Script.deleteOld(props.payloadTarget)
//        )).!
      }
    }
    
    def deploy(nodes: Set[Node]){
      def isRunningABC(node: Node): Boolean = {
        val process = Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.checkJavaRunning(props.applicationMain))).!
        if (process == 0) true else false //Assuming exit code of 0 means model is running
      }
      
      def upload(node: Node, props: Properties, runScriptPath: Path) {
        Process(rsync(
          provider.instanceUser,
          ip(node),
          props.payloadLocal,
          props.payloadTargetParent)).!

        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.unTar("~/deploy", "dataIn.tar.gz"))).!

        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.unTar("~/deploy", "jdk-8u71-linux-x64.tar.gz"))).!

        Process(ssh.scpCommand(
          provider.instanceUser,
          ip(node),
          runScriptPath,
          props.payloadTarget)).!
      }

      def execute(node: Node, props: Properties, runScriptName: String) {
        //TODO logging.  Otherwise commands can silently fail
        val cmd = ssh.backgroundCommand(
          provider.instanceUser,
          ip(node),
          s"${props.payloadTarget}/$runScriptName")
        println(cmd)
        Process(cmd).!
      }
      
      nodes
        .filter{!isRunningABC(_)}
        .foreach { node =>
          val runScript: String = Script.startApplication(
            ip(node),
            props.vmExtraArgs,
            props.applicationMain,
            ip(seeds(0)),
            ip(seeds(1)))
          val runScriptPath = Util.writeToTempFile(runScript, "run", ".sh")
    
          upload(node, props, runScriptPath)
          execute(node, props, runScriptPath.getFileName.toString)
        }
    }
    
    job.operation match {
      case Destroy => 
        tearDown(nodes)
      case Redeploy => 
        tearDown(nodes)
        deploy(nodes)
      case Augment => 
        deploy(nodes)
    }
  }
  
  
      
//  parser.parse(args, Config(path.toFile(), "", TearDown)) match {
//    case Some(configuration) =>
//
//      //      val config = {
//      //        val fileName = "cluster.json"
//      //        val path = Paths.get(System.getProperty("user.home"), fileName)
//      //        FileUtils.readFileToString(path.toFile)
//      //      }
//
//      val config = {
//        FileUtils.readFileToString(configuration.configFile)
//      }
//
//      val props = Properties.fromJSON(config)
//      //val provider: Provider = LocalProvider.buildProvider(config, configuration.cluster)
//      //  val provider = AWS.buildProvider(config, configuration.cluster)
//      //  val provider = CloudSigma.buildProvider(config, configuration.cluster)
//      val provider: Provider = SoftLayer.buildProvider(config)
//
//      //TODO configure from json?
//      // ************ IMPORTANT ***************
//      // Switch between public/private when running inside/outside data centre
//      // Failure to do this may result in public network charges
//      val ipPrivate = (node: Node) => node.privateIp.get
//      val ip = (node: Node) => node.publicIp.get
//      //val nodes = provider.getNodes
//
//      val allNodes = provider.getAllNodes.filter(_.clusterName == configuration.cluster)
//
//      val seeds = List(
//        Util.getAssertOne(allNodes.filter(_.seedRole == Some("seed1"))),
//        Util.getAssertOne(allNodes.filter(_.seedRole == Some("seed2"))))
//      //val workers = nodes.filter(_.role != Some("seed1")).filter(_.role != Some("seed2"))
//      //val allNodes = seeds ++ workers
//
//      println(s"Found ${allNodes.size} nodes: ")
//      allNodes.map("  " + _.toString).foreach(println)
//
//      val ssh = new SSH(props.privateKeyPath)
//      val rsync = new Rsync(props.privateKeyPath)
//
//      if (configuration.tearDown || configuration.redeploy) { // just kill everything
//        println("Tearing down running instances on this cluster")
//        allNodes.foreach(cleanUp)
//      }
//
//      if (!configuration.tearDown) { // redeploy everything - unless augmenting and in that case run if no model already running
//        println("Starting deployment")
//        allNodes.filter { node => !isRunningABC(node) }
//          .foreach { node =>
//            val runScript: String = Script.startApplication(
//              ipPrivate(node),
//              props.vmExtraArgs,
//              props.applicationMain,
//              ipPrivate(seeds(0)),
//              ipPrivate(seeds(1)))
//            val runScriptPath = Util.writeToTempFile(runScript, "run", ".sh")
//
//            upload(node, props, runScriptPath)
//            execute(node, props, runScriptPath.getFileName.toString)
//          }
//      }
//
//      def cleanUp(node: Node) {
//        Process(ssh.foregroundCommand(
//          provider.instanceUser,
//          ip(node),
//          Script.killJava(props.applicationMain))).!
//
//        //	  Process(ssh.foregroundCommand(
//        //		    provider.instanceUser, 
//        //		    ip(node), 
//        //		    Script.deleteOld(props.payloadTarget)
//        //	  )).!
//      }
//
//      def isRunningABC(node: Node): Boolean = {
//        val process = Process(ssh.foregroundCommand(
//          provider.instanceUser,
//          ip(node),
//          Script.checkJavaRunning(props.applicationMain))).!
//        if (process == 0) true else false //Assuming exit code of 0 means model is running
//      }
//
//      def upload(node: Node, props: Properties, runScriptPath: Path) {
//        Process(rsync(
//          provider.instanceUser,
//          ip(node),
//          props.payloadLocal,
//          props.payloadTargetParent)).!
//
//        Process(ssh.foregroundCommand(
//          provider.instanceUser,
//          ip(node),
//          Script.unTar("~/deploy", "dataIn.tar.gz"))).!
//
//        Process(ssh.foregroundCommand(
//          provider.instanceUser,
//          ip(node),
//          Script.unTar("~/deploy", "jdk-8u71-linux-x64.tar.gz"))).!
//
//        Process(ssh.scpCommand(
//          provider.instanceUser,
//          ip(node),
//          runScriptPath,
//          props.payloadTarget)).!
//      }
//
//      def execute(node: Node, props: Properties, runScriptName: String) {
//        //TODO logging.  Otherwise commands can silently fail
//        val cmd = ssh.backgroundCommand(
//          provider.instanceUser,
//          ip(node),
//          s"${props.payloadTarget}/$runScriptName")
//        println(cmd)
//        Process(cmd).!
//      }
//
//      println("----==== Cluster ready captain ====----")
//      allNodes.foreach { println }
//
//    case None => println("Bad arguments given")
//  }

}