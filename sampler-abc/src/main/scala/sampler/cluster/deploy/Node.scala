package sampler.cluster.deploy

import scala.collection.JavaConversions._

//case class IP(addr: String){
//  def isAlive(): Boolean = ???
//}

//TODO does role really need to be an option?  Exception if anything other than seeOne/Two or worker?
case class Node(hostname: String, publicIp: Option[String], privateIp: Option[String], 
    clusterName: String, seedRole: Option[String])
//case class Node(ip: Option[IP], role: Option[String])

//In Main
//val allNodes: Set[Node] = myProvider.getNodes
//
//object Node{
//  implicit class NodeListOps()
//}
//
//
//val master = nodes.filterAlive.filterRole("master").getAssertOne
//
//nodes.collect{
//  case n @ Node(_,_,Some("master")) => n
//}
//nodes.filter(_.role.map(_=="master").getOrElse(false))

//trait Node {
//	def publicIp: String
//	def privateIp: String
//
////	def running: Set[Node]
////	def filterByTag: Set[Node]
////	def toNode: Node
//}

//case class CloudNode(context: ComputeServiceContext, publicIp: String, privateIp: String) extends Node{
//  //implicit val context: ComputeServiceContext
//	val service = context.getComputeService
//
//			def running(): Set[ComputeMetadata] = {
//					service
//					.listNodes
//					.filter { jCNode =>
//					service.getNodeMetadata(jCNode.getId()).getStatus == Status.RUNNING
//					}
//					.toSet
//			}
//
//			def filterByTag(nodes: Set[ComputeMetadata], tag: Tag): Set[ComputeMetadata] = {
//					nodes
//					.filter { node =>
//					val meta = node.getUserMetadata
//					meta.containsKey(tag.key) && meta.get(tag.key) == tag.value
//					}
//			}
//
//			def toNode(computeMeta: ComputeMetadata) = {
//				val meta = service.getNodeMetadata(computeMeta.getId)
//						CloudNode(
//						    context,
//								Util.getAssertOne(meta.getPublicAddresses.toSet),
//								Util.getAssertOne(meta.getPrivateAddresses.toSet)
//								)
//			}			
//}
//			
//case class localNode(publicIp: String, privateIp: String) extends Node{
//  
//
//}
