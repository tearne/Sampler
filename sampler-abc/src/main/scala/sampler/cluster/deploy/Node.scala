package sampler.cluster.deploy

case class Node(
	hostname: String,
	ip: String,
  clusterName: Option[String],
  seedRole: Option[String])
