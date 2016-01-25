//package sampler.abc.actor.main.component
//
//import akka.actor.Actor
//import scala.concurrent.ExecutionContext
//
//trait WorkDispatcherComponentImpl extends WorkDispatcherComponent {
//	this: Actor => 
//		
//	val workDispatcher = 
//		context.system.dispatchers.lookup("sampler.work-dispatcher")
//}
//
//trait WorkDispatcherComponent {
//	val workDispatcher: ExecutionContext
//}