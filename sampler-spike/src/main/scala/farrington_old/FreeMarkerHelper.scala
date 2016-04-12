package farrington_old

import freemarker.template.Configuration
import java.nio.file.Paths
import java.nio.file.Path
import freemarker.template.TemplateExceptionHandler
import java.nio.file.Files

object FreeMarkerHelper{
	val cfg = new Configuration(Configuration.VERSION_2_3_21);
	
	val cl = getClass.getClassLoader
	val templateDir = Paths.get(cl.getResource("farrington").toURI()).toFile()
	
	cfg.setDirectoryForTemplateLoading(templateDir);
	cfg.setDefaultEncoding("UTF-8");
	cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);  

	def writeFile(
			subs: Map[String, String], 
			tmplName: String, 
			path: Path
	){
		val subsAsJava = collection.JavaConversions.mapAsJavaMap(subs)
		val out = Files.newBufferedWriter(path)
		cfg.getTemplate(tmplName).process(subsAsJava, out)
		out.close()
	}
}