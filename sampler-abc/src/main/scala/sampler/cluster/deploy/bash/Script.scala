package sampler.cluster.deploy.bash

import sampler.cluster.deploy.Node
import java.nio.file.Path

object Script {
  
  def killJava(mainClass: String) = {
    val className = mainClass.split('.').last
    s"pkill -f 'java.*$className'"
  }
  
  def checkJavaRunning(mainClass: String) = {
    val className = mainClass.split('.').last
    s"jps | grep $className"
  }
  	
  def unTar(parentDir: String, tarFile: String) = {
    s"cd $parentDir && tar xvzf $tarFile"
  }
    
  //TODO make the -Xmx above configurable from props?
  //TODO jvm args in config
  def startApplication(hostIP: String, vmExtraArgs: Seq[String], mainClass: String, seedOneIP: String, seedTwoIP: String) = 
"""#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
cd $DIR
"""+
s"""
java \\
${vmExtraArgs.mkString(" ")} \\
-Dakka.remote.netty.tcp.hostname=$hostIP \\
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@$seedOneIP:2552 \\
-Dakka.cluster.seed-nodes.1=akka.tcp://ABC@$seedTwoIP:2552 \\
-Dconfig.file=application.conf \\
-Dlogback.configurationFile=logback.xml \\
-cp "lib/*" \\
$mainClass
"""
}