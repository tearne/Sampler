package sampler.cluster.deploy

import java.nio.file.Paths
import scala.sys.process._
import org.apache.commons.io.FileUtils
import sampler.cluster.deploy.bash.SSH
import sampler.cluster.deploy.bash.Rsync
import sampler.cluster.deploy.bash.Script
import java.nio.file.Path
import java.io.File

object Main extends App {

  case class Config(configFile: File = new File("."), cluster: String, mode: String = "",
                    augment: Boolean, tearDown: Boolean, redeploy: Boolean)

  val parser = new scopt.OptionParser[Config]("scopt") { // need case class for Config?
    head("scopt", "3.x")
    opt[File]('c', "config") required () valueName ("<file>") action { (x, c) =>
      c.copy(configFile = x)
    } text ("cluster properties file is required")
    opt[String]('n', "cluster") action { (x, c) =>
      c.copy(cluster = x)
    } text ("cluster is name of cluster instance")
    opt[Boolean]('a', "augment") action { (x, c) =>
      c.copy(augment = x)
    } text ("augment existing cluster")
    opt[Boolean]('t', "teardown") action { (x, c) =>
      c.copy(tearDown = x)
    } text ("teardown existing cluster")
    opt[Boolean]('r', "redeploy") action { (x, c) =>
      c.copy(redeploy = x)
    } text ("redeploy existing cluster")
    checkConfig { c =>
      if ((c.tearDown && c.redeploy)
        || (c.augment && c.redeploy)
        || (c.augment && c.tearDown))
        failure("cannot simultaneously tearDown, redeploy or augment - choose 1 option") else success
    }
  }

  val defaultFileName = "cluster.json"
  val path = Paths.get(System.getProperty("user.home"), defaultFileName)

  parser.parse(args, Config(path.toFile(), "", "", false, false, false)) match {
    case Some(configuration) =>

      //      val config = {
      //        val fileName = "cluster.json"
      //        val path = Paths.get(System.getProperty("user.home"), fileName)
      //        FileUtils.readFileToString(path.toFile)
      //      }

      val config = {
        FileUtils.readFileToString(configuration.configFile)
      }

      val props = Properties.fromJSON(config)
      //val provider: Provider = LocalProvider.buildProvider(config, configuration.cluster)
      //  val provider = AWS.buildProvider(config, configuration.cluster)
      //  val provider = CloudSigma.buildProvider(config, configuration.cluster)
      val provider: Provider = SoftLayer.buildProvider(config)

      //TODO configure from json?
      // ************ IMPORTANT ***************
      // Switch between public/private when running inside/outside data centre
      // Failure to do this may result in public network charges
      //  val ip = (node: Node) => node.privateIp.get
      val ip = (node: Node) => node.publicIp.get
      //val nodes = provider.getNodes

      val allNodes = provider.getAllNodes.filter(_.clusterName == configuration.cluster)

      val seeds = List(
        Util.getAssertOne(allNodes.filter(_.seedRole == Some("seed1"))),
        Util.getAssertOne(allNodes.filter(_.seedRole == Some("seed2"))))
      //val workers = nodes.filter(_.role != Some("seed1")).filter(_.role != Some("seed2"))
      //val allNodes = seeds ++ workers

      println(s"Found ${allNodes.size} nodes: ")
      allNodes.map("  " + _.toString).foreach(println)

      val ssh = new SSH(props.privateKeyPath)
      val rsync = new Rsync(props.privateKeyPath)

      if (configuration.tearDown || configuration.redeploy) { // just kill everything
        allNodes.foreach(cleanUp)
      }

      if (!configuration.tearDown) { // redeploy everything - unless augmenting and in that case run if no model already running
        allNodes.filter { node => !isRunningABC(node) }
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

      def cleanUp(node: Node) {
        Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.killJava(props.applicationMain))).!

        //	  Process(ssh.foregroundCommand(
        //		    provider.instanceUser, 
        //		    ip(node), 
        //		    Script.deleteOld(props.payloadTarget)
        //	  )).!
      }

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

      println("----==== Cluster ready captain ====----")
      allNodes.foreach { println }

    case None => println("Bad arguments given")
  }

}