package sampler.cluster.deploy

import java.io.File
import java.nio.file.Path
import org.apache.commons.io.FileUtils
import sampler.cluster.deploy.bash.{Rsync, SSH, Script}
import scala.sys.process._
import scopt.Read

trait Deployer {
  def apply(args: Array[String], provider: Provider): Unit = {
    parser
      .parse(args, Job(null, "", null))
      .foreach{job => run(job, provider)}
  }

  trait Operation
  case object Augment extends Operation
  case object Destroy extends Operation
  case object Redeploy extends Operation

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

  val parser = new scopt.OptionParser[Job]("scopt") {
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

  def run(job: Job, provider: Provider): Unit = {
    val config = FileUtils.readFileToString(job.configFile)
    val props = Properties.fromJSON(config)
    //val provider: Provider = SoftLayer.buildProvider(config)

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

    job.operation match {
      case Destroy =>
        tearDown(nodes)
      case Redeploy =>
        tearDown(nodes)
        deploy(nodes)
      case Augment =>
        deploy(nodes)
    }

    def tearDown(nodes: Set[Node]): Unit = {
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

    def deploy(nodes: Set[Node]): Unit = {
      def isRunningABC(node: Node): Boolean = {
        val process = Process(ssh.foregroundCommand(
          provider.instanceUser,
          ip(node),
          Script.checkJavaRunning(props.applicationMain))).!
        if (process == 0) true else false //Assuming exit code of 0 means model is running
      }

      def upload(node: Node, props: Properties, runScriptPath: Path): Unit =  {
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

      def execute(node: Node, props: Properties, runScriptName: String): Unit = {
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
  }
}
