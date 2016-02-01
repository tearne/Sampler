package sampler.cluster.deploy

case class Node(
	hostname: String,
	publicIp: Option[String],
	privateIp: Option[String],
  clusterName: Option[String],
  seedRole: Option[String])
