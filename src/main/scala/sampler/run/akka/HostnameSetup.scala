package sampler.run.akka

import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import scala.util.{Try, Success, Failure}

trait HostnameSetup {
	val log = LoggerFactory.getLogger(this.getClass())
	
	if(ConfigFactory.load().getBoolean("cluster.inet-bind")){
		Try{
			java.net.InetAddress.getLocalHost.getHostAddress
		}match{
			case Success(addr) => 
				System.setProperty("akka.remote.netty.hostname", addr)
				log.info("Binding to local host address "+addr)
			case Failure(_) => 
				log.warn("Falling back to config hostname instead of inet host address")
		}
	} else log.info("Binding with hostname in config")
	ConfigFactory.invalidateCaches()
}