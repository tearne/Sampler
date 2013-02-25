package sampler.run

import com.typesafe.config.ConfigFactory
import akka.actor.{ ActorRef, Props, Actor, ActorSystem }
import sampler.run.cluster.Particle
import sampler.run.cluster.Encapsulator
import sampler.math.Random

trait IndividualConfig{
	val sender = ConfigFactory.parseString("""
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    transport = "akka.remote.netty.NettyRemoteTransport"
    netty {
      hostname = "127.0.0.1"
      port = 2552
    }
  }
}""") 
	val receiver = ConfigFactory.parseString("akka.remote.netty.port = 2553")
		.withFallback(sender)
}

trait ClusterConfig{
	val sender = ConfigFactory.parseString("""
akka {
  actor {
    provider = "akka.cluster.ClusterActorRefProvider"
  }
  remote {
    transport = "akka.remote.netty.NettyRemoteTransport"
	log-remote-lifecycle-events = off		
    netty {
      hostname = "127.0.0.1"
      port = 2552
    }
  }
  extensions = ["akka.cluster.Cluster"]   
  cluster {
    seed-nodes = [
      "akka://MySystem@127.0.0.1:2553",
	  "akka://MySystem@127.0.0.1:2552"
    ] 
    auto-down = on
  }
}""") 
	val receiver = ConfigFactory.parseString("akka.remote.netty.port = 2553")
		.withFallback(sender)
}

object Config extends ClusterConfig

object Sender extends App{
	val system = ActorSystem("MySystem",Config.sender)
	val actor = system.actorFor("akka://MySystem@127.0.0.1:2553/user/receiver")

    case class MsgA(str: String)
    actor ! MsgA("Hello world")
    
    val outer = new ConcreteEnvironment
    actor ! new outer.Message
    
    val random = new Random
    val model = new sampler.run.cluster.CoinModel
    val population: Seq[Particle[model.Parameters]] = (1 to 1000000000).par.map(i => Particle(model.prior.sample(random), 1.0, Double.MaxValue)).seq
    val encapsulated = Encapsulator(model)(population)
    actor ! encapsulated
    
    println("sent msg")
}

object Receiver1 extends App{
	val system = ActorSystem("MySystem",Config.receiver)
	val actor = system.actorOf(Props(new Actor{
		def receive = {
			case m => println(self+" got a message: "+m)
		}
	}), name = "receiver")
}

object Receiver2 extends App{
	val system = ActorSystem("MySystem",Config.sender)
	val actor = system.actorOf(Props(new Actor{
		def receive = {
			case m => println(self+" got a message: "+m)
		}
	}), name = "receiver")
}


trait Environment{
    type Message <: MsgBase
    protected trait MsgBase
}

class ConcreteEnvironment extends Environment with Serializable{
	class Message() extends MsgBase with Serializable
}