package sampler.abc.actor.sub.worker

import java.util.concurrent.atomic.AtomicBoolean

trait AborterComponentImpl extends AborterComponent{
	val aborter = new Aborter {}
}

trait AborterComponent {
	val aborter: Aborter
}

trait Aborter {
	private val aborted: AtomicBoolean = new AtomicBoolean(false)
	def abort() { aborted.set(true) }
	def reset() { aborted.set(false) }
	def isAborted = aborted.get
	def checkIfAborted() { 
		if(isAborted) throw new DetectedAbortionException() 
  }
}