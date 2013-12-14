package sampler.cluster.deploy

import net.schmizz.sshj.SSHClient
import net.schmizz.sshj.common.IOUtils
import java.util.concurrent.TimeUnit
import net.schmizz.sshj.userauth.keyprovider.PKCS8KeyFile
import net.schmizz.sshj.userauth.method.AuthPublickey
import java.nio.file.Paths
import net.schmizz.sshj.transport.verification.PromiscuousVerifier
import java.nio.file.Path
import net.schmizz.sshj.xfer.FileSystemFile

object SSHJ extends App{
	
	copyFile("ec2-54-194-127-213.eu-west-1.compute.amazonaws.com", Paths.get(System.getProperty("user.home")).resolve(".s3cfg"))
	
	def copyFile(server: String, toUpload: Path){
		val ssh = new SSHClient()
		ssh.addHostKeyVerifier(new PromiscuousVerifier())
		
		val keyFile: PKCS8KeyFile = new PKCS8KeyFile();
		val file = Paths.get("/mnt").resolve("hgfs").resolve("share").resolve("AWS").resolve("otkp.pem").toFile
		assert(file.exists)
		
//		ssh.useCompression()	// Needs JZlib on classpath for this to work
		ssh.connect(server)
		keyFile.init(file)
		ssh.auth("ec2-user", new AuthPublickey(keyFile));
		val session = ssh.startSession()
		
		ssh.newSCPFileTransfer().upload(new FileSystemFile(toUpload.toFile()), "")
		ssh.disconnect()
	}
	
	def runCommand(server: String, command: String){
		val ssh = new SSHClient()
		ssh.addHostKeyVerifier(new PromiscuousVerifier())
		
		val keyFile: PKCS8KeyFile = new PKCS8KeyFile();
		val file = Paths.get("/mnt").resolve("hgfs").resolve("share").resolve("AWS").resolve("otkp.pem").toFile
		assert(file.exists)
		
		ssh.connect(server)
		keyFile.init(file)
		ssh.auth("ec2-user", new AuthPublickey(keyFile));
		val session = ssh.startSession()
		
		
		val cmd = session.exec(command)
		System.out.println(IOUtils.readFully(cmd.getInputStream()).toString())
	    cmd.join(10, TimeUnit.SECONDS)
	    System.out.println("\n** exit status: " + cmd.getExitStatus())
		
		session.close()
		
		ssh.disconnect()
	}
}