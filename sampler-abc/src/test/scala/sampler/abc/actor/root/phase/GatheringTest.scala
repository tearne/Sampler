package sampler.abc.actor.root.phase

import org.scalatest.FreeSpec

class GatheringTest extends FreeSpec {
  "Gathering should" - {
    "Ignore report completed message" in {
      fail("TODO")
    }

    "Worker failure triggers allocation of new job" in {
      fail("TODO")
      // TODO consider ability to reallocated weight jobs
      //  Note that if the failed job was a weighing job
      // then it doesn't get reallocated, we just loose all
      // those scored particles.  Not ideal, but if a weigh
      // job caused a failure then resubmitting it probably
      // wouldn't work better
    }

    "New scored partilcles added" in {
      fail("TODO")
    }

    "Mix payload particles added" in {
      fail("TODO")
    }

    "Weighted particles added and triggers flush" in {
      fail("TODO")
    }

    "Weighted particles added but not time to flush" in {
      fail("TODO")
    }

    "MixNow message" in {
      fail("TODO")
    }

    "Be tested" in {fail("TODO")}
  }
}
