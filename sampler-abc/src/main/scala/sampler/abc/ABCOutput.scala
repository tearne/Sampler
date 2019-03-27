package sampler.abc

import java.nio.file.Path

import org.apache.commons.io.FileUtils
import play.api.libs.json.{JsObject, Json}
import sampler.io.Tokenable
import sampler.r.script.RScript

case class ABCOutput[P](
  config: ABCConfig,
  population: Population[P]
) {

  private val ste: StackTraceElement = Thread.currentThread.getStackTrace()(2)
  private val thisPackage = ste.getClassName.takeWhile(_ != '$')

  private def toJSON()(implicit tokenable: Tokenable[P]): JsObject = {
    Json.obj(
      "runMetaData" -> Map(
        "package" -> thisPackage,
        //"description" -> config.description
        // TODO any other metadata?
      ),
      "population" -> population.toJSON()
    )
  }

  private def saveOutput(outDir: Path)(implicit tokenable: Tokenable[P]): Unit = {
    FileUtils.writeStringToFile(
      outDir.resolve("output.json").toFile, // assign name here so that
      Json.prettyPrint(toJSON())
    )
  }

  private def makePlot(outDir: Path): Unit = {
    // TODO any other metadata wanted on the plots?
    val script =
      raw"""
        |lapply(c("ggplot2", "grid","GGally", "scales", "reshape2", "jsonlite", "RColorBrewer", "plyr", "stringr"), require, character.only = T)
        |
        |# Load in the data and get the right bits out for plotting
        |output = fromJSON("output.json")
        |
        |runMeta = output$$runMetaData
        |# popMeta = output$$population
        |
        |package = runMeta$$package
        |description = runMeta$$description
        |
        |setwd("$outDir")
        |files <- list.files()
        |nGens = length(files)
        |
        |scores = list()
        |meta = list()
        |params = list()
        |
        |# Glue all the bits from different generations together
        |for (i in 1 : nGens) {
        |    gen_data = as.data.frame(fromJSON(files[i], flatten = TRUE))
        |    gen_scores = as.data.frame(matrix(unlist(gen_data$$'particle.details.s')))
        |    gen_scores$$gen = i
        |    scores = rbind(scores, gen_scores)
        |
        |    gen_meta = gen_data[, 3 : 5]
        |    meta = rbind(meta, gen_meta)
        |
        |    gen_params = gen_data[, str_which(names(gen_data), "particle.summary.")]
        |    gen_params$$gen = i
        |    params = rbind(params, gen_params)
        |}
        |
        |# Format Params and scores dataframes
        |names(params) = gsub("particle.summary.", "", names(params))
        |long_params = melt(params, id.vars = c("gen", "weight"))
        |long_params$$gen = as.factor(long_params$$gen)
        |scores$$gen = as.factor(scores$$gen)
        |
        |### format dataframe of meta data ###
        |meta = ddply(meta, ~ generation + acceptance.ratio + tolerance, summarise, popSize = length(generation))
        |names(meta) = c("Generation", "Acceptance", "Tolerance", "PopSize")
        |meta$$Tolerance[meta$$Tolerance > 1e100] = Inf
        |meta = meta[, c(1, 2, 4, 3)]
        |meta$$logTolerance = log10(meta$$Tolerance)
        |long_meta = melt(meta, id.var = c("Generation"))
        |
        |# Define function for colouring metadata plot
        |isFinite = function(x) { ! is.infinite(x)}
        |
        |### begin plotting ###
        |setwd("$outDir")
        |pdf(paste(description,".pdf"), width = 12, height = 12, title = description)
        |
        |### plot scores ###
        |ggplot(scores, aes(V1)) +
        |    geom_density() +
        |    facet_grid(rows = vars(gen), switch = "y", scales = "free") +
        |    theme(text = element_text(size = 18)) +
        |    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) +
        |    xlab("Scores") +
        |    ggtitle("Distribution of Scores by Generation")
        |
        |### plot meta data ###
        |ggplot(long_meta, aes(x = Generation, y = value, fill = isFinite(value))) +
        |    geom_bar(stat = "identity") +
        |    facet_grid(variable ~ ., scales = "free_y") +
        |    theme(text = element_text(size = 18)) +
        |    scale_fill_brewer(palette = "Set1") +
        |    scale_x_continuous(breaks = c(1 : nGens), labels = c(1 : nGens)) +
        |    ggtitle("Generation Metadata")
        |
        |### plot parameter fit ###
        |ggplot(long_params, aes(x = value, color = gen)) +
        |    geom_line(stat = "density") +
        |    facet_wrap(~ variable, scale = "free") +
        |    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) +
        |    theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1)) +
        |    theme(text = element_text(size = 18)) +
        |    ggtitle("Parameter Density by Generation")
        |
        |### plot posterior data ###
        |p = (params[params$$gen == nGens,])
        |idx = union(which(names(p) == "gen"), which(names(p) == "weight"))
        |p = p[, - idx]
        |median = round(sapply(p, quantile, probs = c(0.5), na.rm = TRUE), 4)
        |
        |# define the colour scale
        |colfunc <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
        |colourTrans_trans = function() {trans_new("colourTrans", function(x) x ^ 0.3, function(x) x ^ 0.3)}
        |
        |
        |plotList = list()
        |paramNames = names(p)
        |nVars = ncol(p)
        |
        |correlation = function(x) {
        |    cor = cor(x)[[2]]
        |    round(cor, 2)
        |}
        |
        |for (i in 1 : nVars) {
        |    for (j in 1 : nVars) {
        |        counter = (i - 1) * nVars + j
        |        if (i > j) {
        |            # Below the diagonal
        |            plotList[[counter]] =
        |            ggplot(p, aes_string(x = paramNames[j], y = paramNames[i])) +
        |                stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
        |                scale_fill_gradientn(colours = colfunc(100), trans = "colourTrans") +
        |                guides(fill = FALSE) +
        |                geom_hline(yintercept = median[[i]]) +
        |                geom_vline(xintercept = median[[j]]) +
        |                theme_bw() +
        |                theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        |                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        |                theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 12))
        |        } else if (i == j) {
        |            # Density plots along the diagonal
        |            text <- grobTree(textGrob(median[[i]],x=0.1,y=0.95,hjust=0,gp=gpar(col="red")))
        |            plotList[[counter]] =
        |            ggplot(p, aes_string(paramNames[i], fill = 1)) +
        |                geom_density(alpha = 0.25) +
        |                # vline and label (median)
        |                geom_vline(xintercept = median[[i]]) +
        |                annotation_custom(text) +
        |                scale_y_continuous(breaks = NULL) +
        |                theme_bw() +
        |                theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        |                theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 12))
        |        } else {
        |            # Correlations in the upper triangle
        |            plotList[[counter]] <-
        |            ggally_text(correlation(p[, c(i, j)]),color="black") +
        |                theme_bw() +
        |                theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
        |        }
        |    }
        |}
        |
        |# Put the plots in the matrix
        |ggmatrix(plotList, nVars, nVars,
        |xAxisLabels = paramNames,
        |yAxisLabels = paramNames) +
        |    theme(text = element_text(size = 18)) +
        |    ggtitle("Probability Density")
      """.stripMargin
    RScript(script, outDir.resolve("myScript.r"))
  }

  def makeOutputs(outDir: Path)(implicit tokenable: Tokenable[P]): Unit = {
    saveOutput(outDir)
    makePlot(outDir)
  }
}
