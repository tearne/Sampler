/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.cluster.deploy.aws

import scala.collection.JavaConversions._
import java.nio.file.{Paths,Files}
import com.amazonaws.auth.PropertiesCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.GetObjectRequest
import com.amazonaws.regions.Region
import com.amazonaws.regions.Regions;

object S3Download extends App{
	val wd = Paths.get("/mnt/hgfs/EC2/upload")
	private val keyPath = Paths.get("/mnt/hgfs/EC2/AWS-Key.properties")
	private val keyProps = new PropertiesCredentials(Files.newInputStream(keyPath))
	val s3 = new AmazonS3Client(keyProps)
	s3.setRegion(Region.getRegion(Regions.EU_WEST_1))
	
	val bucketName = "ot-bucket"
	val key = "Rowan.jpg"
		
	val file = wd.resolve(key).toFile()
	val s3Obj = s3.getObject(new GetObjectRequest(bucketName, key), file)
}