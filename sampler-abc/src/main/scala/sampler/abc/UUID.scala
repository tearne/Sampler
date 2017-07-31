package sampler.abc

import java.util.{UUID => JUUID}

import com.fasterxml.uuid.{EthernetAddress, Generators}

/*
This wrapper is necessary for mocking, since java.util.UUID is 'final'
 */
case class UUID(id: JUUID) {
  def generatingNodeId: Long = id.node()
}

object UUID{
  val generator = Generators
    .timeBasedGenerator(EthernetAddress.fromInterface())

  def generate(): UUID = UUID(generator.generate())

  val thisNodeId: Long = generator
    .generate
    .node

}