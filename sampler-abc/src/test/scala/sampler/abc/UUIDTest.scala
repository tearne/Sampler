package sampler.abc

import com.fasterxml.uuid.{EthernetAddress, Generators}
import org.scalatest.FreeSpec

class UUIDTest extends FreeSpec {

  "UUID should" - {
    "Give UUID equality" in {
      val instance = UUID.generate()

      assert(instance === instance)
    }

    "Generate different UUID" in {
      val instance1 = UUID.generate()
      val instance2 = UUID.generate()
      val instance3 = UUID.generate()

      assert(instance1 !== instance2)
      assert(instance2 !== instance3)
      assert(instance3 !== instance1)
    }

    "Return consistent node ID for this node" in {
      val instance1 = UUID.generate()
      val instance2 = UUID.generate()
      val instance3 = UUID.generate()

      val expectedNodeId = Generators
        .timeBasedGenerator(EthernetAddress.fromInterface())
        .generate
        .node

      assert(instance1.generatingNodeId === expectedNodeId)
      assert(instance2.generatingNodeId === expectedNodeId)
      assert(instance3.generatingNodeId === expectedNodeId)
    }
    "Know the ID of this node" in {
      val expectedNode = Generators
        .timeBasedGenerator(EthernetAddress.fromInterface())
        .generate
        .node

      assert(UUID.thisNodeId === expectedNode)
    }
  }
}
