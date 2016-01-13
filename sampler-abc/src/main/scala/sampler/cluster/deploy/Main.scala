package sampler.cluster.deploy

import java.nio.file.Paths
import scala.sys.process._
import org.apache.commons.io.FileUtils
import sampler.cluster.deploy.bash.SSH
import sampler.cluster.deploy.bash.Rsync
import sampler.cluster.deploy.bash.Script
import java.nio.file.Path

object Main extends App {
	
	val config = {
	  val fileName = "cluster.json"
		val path = Paths.get(System.getProperty("user.home"), fileName)
		FileUtils.readFileToString(path.toFile)
	}

	val props = Properties.fromJSON(config)
	//val provider: Provider = LocalProvider.buildProvider(config)
//  val provider = AWS.buildProvider(config)
//  val provider = CloudSigma.buildProvider(config)
	  val provider: Provider = SoftLayer.buildProvider(config)
  
  //TODO configure from json?
  // ************ IMPORTANT ***************
  // Switch between public/private when running inside/outside data centre
  // Failure to do this may result in public network charges
//  val ip = (node: Node) => node.privateIp.get
  val ip = (node: Node) => node.publicIp.get
  val nodes = provider.getNodes
  
  val seeds = List(
  		Util.getAssertOne(nodes.filter(_.role == Some("seed1"))),
  		Util.getAssertOne(nodes.filter(_.role == Some("seed2")))
  	)
  val workers = nodes.filter(_.role == Some("worker"))
  val allNodes = seeds ++ workers
  
  println(s"Found ${allNodes.size} nodes: ")
  allNodes.map("  "+_.toString).foreach(println)
    
  val ssh = new SSH(props.privateKeyPath)
  val rsync = new Rsync(props.privateKeyPath)
  
 if(props.tearDown)
  allNodes.foreach(cleanUp)
  
  allNodes.foreach{node => 
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
  
  def cleanUp(node: Node) {
    Process(ssh.foregroundCommand(
		    provider.instanceUser, 
		    ip(node), 
		    Script.killJava  
	  )).!
	  
//	  Process(ssh.foregroundCommand(
//		    provider.instanceUser, 
//		    ip(node), 
//		    Script.deleteOld(props.payloadTarget)
//	  )).!
  }
  
  def upload(node: Node, props: Properties, runScriptPath: Path){
    Process(rsync(
    		provider.instanceUser, 
    		ip(node), 
        props.payloadLocal, 
        props.payloadTargetParent
    )).!
    
//    Process(ssh.foregroundCommand(
//        provider.instanceUser, 
//    		ip(node), 
//        Script.unTar("~/deploy", "dataIn.tar.gz")
//    )).!
	  
    Process(ssh.scpCommand(
        provider.instanceUser, 
        ip(node), 
        runScriptPath, 
        props.payloadTarget
    )).!
  }
  
  def execute(node: Node, props: Properties, runScriptName: String){
    //TODO logging.  Otherwise commands can silently fail
    val cmd = ssh.backgroundCommand(
        provider.instanceUser, 
        ip(node), 
        s"${props.payloadTarget}/$runScriptName"
    )
    println(cmd)
    Process(cmd).!
  }
  
  println("----==== Cluster ready captain ====----")
  allNodes.foreach{println}
}