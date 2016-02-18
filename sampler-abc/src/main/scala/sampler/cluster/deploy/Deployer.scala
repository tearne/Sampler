package sampler.cluster.deploy

import java.io.File
import java.nio.file.Path
import org.apache.commons.io.FileUtils
import sampler.cluster.deploy.bash.{ Rsync, SSH, Script }
import scala.sys.process._
import scopt.Read
import org.slf4j.LoggerFactory

object Deployer {
  val log = LoggerFactory.getLogger(getClass.getName)
  val processLog = ProcessLogger(log.debug, log.error)

  def apply(args: Array[String], providerBuilder: String => Provider): Unit = {
    parser
      .parse(args, Job(null, "", Operation.default))
      .foreach { job => run(job, providerBuilder) }
  }

  trait Operation
  case object Augment extends Operation
  case object Destroy extends Operation
  case object Redeploy extends Operation
  case object ListIdle extends Operation

  object Operation {
    val default = ListIdle
    val values = Vector(Augment, Destroy, Redeploy, ListIdle)

    def fromString(str: String): Option[Operation] = {
      values.find(_.toString.toLowerCase == str.toLowerCase)
    }

    implicit val operationRead: Read[Operation] =
      Read.reads[Operation] { str =>
        fromString(str).getOrElse(throw new Exception("Could not parse: " + str))
      }
  }

  case class Job(
    configFile: File,
    clusterTag: String,
    operation: Operation = Operation.default)

  val parser = new scopt.OptionParser[Job]("Deployer") {
    opt[File]('c', "config") required () valueName ("<file>") action { (x, c) =>
      c.copy(configFile = x)
    } text ("Cluster properties file ")

    opt[String]('n', "name") required () valueName ("<clusterName>") action { (x, c) =>
      c.copy(clusterTag = x)
    } text ("Name of the cluster.  E.g. 'myCluster' if instances have been tagged with \"cluster:myCluster\".")

    opt[Operation]('o', "operation") valueName ("<operation>") action { (x, c) =>
      c.copy(operation = x)
    } text ("Cluster operation to perform.")
  }

  def process(cmd: String): Int = {
    log.debug("About to run: " + cmd)
    Process(cmd).!(processLog)
  }
  def process(cmds: Seq[String]): Int = {
    log.debug("About to run: " + cmds)
    Process(cmds).!(processLog)
  }

  def run(job: Job, providerBuilder: String => Provider): Unit = {
    log.info(job.toString)
    val config = FileUtils.readFileToString(job.configFile)
    val props = Properties.fromJSON(config)
    log.info(props.toString)
    val provider: Provider = providerBuilder(config)

    log.info("Requesting instance data...")
    val allNodes = provider.getAllNodes
    val nodes = allNodes.filter(_.clusterNameOpt == Some(job.clusterTag))
    log.info(s"... cluster has ${nodes.size} nodes (out of ${allNodes.size} instances total): ")
    nodes.map("  " + _.toString).foreach(log.info)

    assume(nodes.size > 0, s"Found 0 nodes in cluster ${job.clusterTag} (${allNodes.size} nodes overall)")

    val seeds = List(
      Util.getAssertOne(nodes.filter(_.seedRoleOpt == Some("seed1"))),
      Util.getAssertOne(nodes.filter(_.seedRoleOpt == Some("seed2"))))

    val ssh = new SSH(props.privateKeyPath)
    val rsync = new Rsync(props.privateKeyPath)

    job.operation match {
      case Destroy =>
        tearDown(nodes)
      case Redeploy =>
        tearDown(nodes)
        deploy(nodes)
      case Augment =>
        deploy(nodes)
      case ListIdle =>
        listIdle(nodes)
    }

    def listIdle(nodes: Set[Node]): Unit = {
      log.info("Requesting idle/busy status for cluster ...")
      val report = nodes.par.map{ node =>
        if (isRunningABC(node)) s" (*) Busy: $node"
        else s" ( ) Idle: $node"
      }.toIndexedSeq.sorted
      report.foreach(log.info)
    }

    def tearDown(nodes: Set[Node]): Unit = {
      nodes.foreach { node =>
        process(ssh.foregroundCommand(
          provider.instanceUser,
          node.ip,
          Script.killJava(props.applicationMain)))
      }
    }

    def deploy(nodes: Set[Node]): Unit = {
      def upload(node: Node, props: Properties, runScriptPath: Path): Unit = {
        process(rsync(
          provider.instanceUser,
          node.ip,
          props.payloadLocal,
          props.payloadTargetParent))

        process(ssh.scpCommand(
          provider.instanceUser,
          node.ip,
          runScriptPath,
          props.payloadTarget))
      }

      def execute(node: Node, props: Properties, runScriptName: String): Unit = {
        val cmd = ssh.backgroundCommand(
          provider.instanceUser,
          node.ip,
          s"${props.payloadTarget}/$runScriptName")
        process(cmd)
      }

      nodes
        .foreach { node =>
          log.info("Working on " + node)
          if (!isRunningABC(node)) {
            val runScript: String = Script.startApplication(
              node.ip,
              props.vmExtraArgs,
              props.applicationMain,
              seeds(0).ip,
              seeds(1).ip)
            val runScriptPath = Util.writeToTempFile(runScript, "run", ".sh")

            upload(node, props, runScriptPath)
            execute(node, props, runScriptPath.getFileName.toString)
          }
        }
    }
    
    def isRunningABC(node: Node): Boolean = {
      val cmd = ssh.foregroundCommand(
        provider.instanceUser,
        node.ip,
        Script.checkJavaRunning(props.applicationMain))
      val exitCode = process(cmd)
      exitCode == 0 //Assuming exit code of 0 means model is running
    }
  }
}
