package sampler.cluster.deploy

case class Node(
	hostname: String,
	ip: String,
  clusterNameOpt: Option[String],
  seedRoleOpt: Option[String])
