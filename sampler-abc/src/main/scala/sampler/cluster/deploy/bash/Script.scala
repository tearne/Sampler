package sampler.cluster.deploy.bash

import sampler.cluster.deploy.Node
import java.nio.file.Path

object Script {
  
 // def killJava() = "killall java > killAllResult.txt 2>&1"
  //def killJava() = "pkill -f 'java.*TBMIFitApp'"
  def killJava(mainClass: String) = "pkill -f 'java.*TBMIFit'"//+mainClass.split(".").tail+"'"  
  
  def checkJavaRunning(mainClass: String) = "/root/deploy/jdk1.8.0_71/bin/jps | grep 'TBMIFit'"//+mainClass.split(".").tail+"'" //assumes jdk installed which it should be
  	
  //No longer needed if rsync uses --delete option
  //def deleteOld(dir: String) = s"rm $dir/run*.sh && rm -r $dir/log" 
  
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
/root/deploy/jdk1.8.0_71/bin/java \\
${vmExtraArgs.mkString(" ")} \\
-Dakka.remote.netty.tcp.hostname=$hostIP \\
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@$seedOneIP:2552 \\
-Dakka.cluster.seed-nodes.1=akka.tcp://ABC@$seedTwoIP:2552 \\
-Dconfig.file=application.conf \\
-Dlogback.configurationFile=/root/deploy/logback.xml \\
-cp "lib/*" \\
$mainClass
"""
}