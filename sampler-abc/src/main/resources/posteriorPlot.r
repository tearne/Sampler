lapply(c("ggplot2", "ggpubr", "grid", "GGally", "scales", "reshape2", "jsonlite", "RColorBrewer", "plyr", "stringr"), require, character.only = T)

# Define function to extract the parameters and their weights
loadParams = function(file){
	raw = fromJSON(file)
	df = data.frame(
	Gen = factor(raw$generation),
	Weight = raw$'particle-details'$w,
	raw$'particle-details'$p
	)
}

# Define function to grab the scores out of a file
loadMeta = function(file) {
	raw = fromJSON(file)
	df = data.frame(
	Gen = factor(raw$generation),
	Acceptance = (raw$'acceptance-ratio'),
	popSize = nrow(raw$'particle-details'),
	Tolerance = raw$tolerance
	)
}

loadScores = function(file) {
	raw = fromJSON(file)
	df = data.frame(
	Gen = factor(raw$generation),
	Score = unlist(raw$'particle-details'$s)
	)
}
# Grab the task description from meta data
task = fromJSON("Gen001.json")$breadcrumbs$task
description = fromJSON("Gen001.json")$breadcrumbs$upstream$description

# Load the params from all "Gen **.json" files
params = ldply(lapply(Sys.glob("Gen*.json"), loadParams))

# If any columns contain lists, these must be unnested before plotting.
remove = c()
for (i in 3 : ncol(params)) {
	if (typeof(params[, i]) == "list") {
		expanded = data.frame(do.call(rbind, params[, i]))
		for (j in 1 : ncol(expanded)) {
			names(expanded)[j] = paste(names(params)[i], j, sep = ".")
		}
		params = cbind(params, expanded)
		remove = c(remove, i)
	}
}
# Remove the original nested lists
if (length(remove) > 0) {
	params = params[, - remove]
}
# how many variables are there? remove 2 columns (Gen, weight)
nVars = ncol(params) - 2
paramNames = names(params)[- c(1, 2)]

# Load the scores
scores = ldply(lapply(Sys.glob("Gen*.json"), loadScores))
genMeta = ldply(lapply(Sys.glob("Gen*.json"), loadMeta))

genMeta$Tolerance[genMeta$Tolerance > 1e100] = Inf
genMeta$logTolerance = log10(genMeta$Tolerance)
long_meta = melt(genMeta, id.vars = c("Gen"))

nGens = nlevels(params$Gen)

# Define function for colouring metadata plot
isFinite = function(x) { ! is.infinite(x)}

##### Begin plotting #####
pdf(paste(task, ".pdf", sep = ""), width = 12, height = 12, title = description)

### plot scores ###
ggplot(scores, aes(Score)) +
	geom_density() +
	facet_grid(rows = vars(Gen), switch = "y", scales = "free") +
	theme(text = element_text(size = 18)) +
	theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) +
	xlab("Scores") +
	ggtitle("Distribution of Scores by Generation")

### plot meta data ###
ggplot(long_meta, aes(x = Gen, y = value, fill = isFinite(value))) +
	geom_bar(stat = "identity") +
	facet_grid(variable ~ ., scales = "free_y") +
	theme(text = element_text(size = 18)) +
	scale_fill_brewer(palette = "Set1") +
	ggtitle("Generation Metadata")

### plot parameter fit by generation ###
plotList = list()
for (i in 1 : nVars) {
	p = params[, c(1, i + 2)]
	names(p) = c("Gen", "value")

	if (is.integer(p$value)) {
		binwidth = 1
		text <- grobTree(textGrob("Integer", x = 0.85, y = 0.9, gp = gpar(col = "red")))
	}else {
		binwidth = (max(p$value) - min(p$value)) / 30
		text <- grobTree(textGrob("", x = 0.85, y = 0.9, gp = gpar(col = "red")))
	}

	plotList[[i]] =
	ggplot(p, aes(x = value, color = Gen)) +
		geom_line(stat = "density", bw = binwidth) +
		annotation_custom(text) +
		xlab(paramNames[i]) +
		scale_colour_hue(h=c(-270, 0)) +
		theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) +
		theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1)) +
		theme(text = element_text(size = 18))
}

# Arrange the plots into a grid
plotGridSize = ceiling(sqrt(nVars))
figure = ggarrange(plotlist = plotList,
ncol = plotGridSize, nrow = plotGridSize,
common.legend = TRUE, legend = "right")
annotate_figure(figure, top = text_grob("Parameter Density by Generation", size = 24))

# grab the parameters from the last generation only and define medians
finalParams = params[which(params$Gen == nGens), - c(1)]
median = round(sapply(finalParams, quantile, probs = c(0.5), na.rm = TRUE), 4)

# Define the colour scale (for 2D density plot)
colfunc <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
colourTrans_trans = function() {trans_new("colourTrans", function(x) x ^ 0.3, function(x) x ^ 0.3)}

# Define function to compute correlations
correlation = function(x) {
	cor = cor(x)[[2]]
	round(cor, 2)
}

### plot posterior data ###
plotList = list()
for (i in 2 : (nVars + 1)) {
	for (j in 2 : (nVars + 1)) {
		counter = ((i - 1) - 1) * nVars + (j - 1)
		if (i > j) {
			# Below the diagonal - plot 2D density
			plotList[[counter]] =
			ggplot(finalParams, aes_string(x = paramNames[j - 1], y = paramNames[i - 1])) +
				stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
				scale_fill_gradientn(colours = colfunc(100), trans = "colourTrans") +
				guides(fill = FALSE) +
				geom_hline(yintercept = median[[i]]) +
				geom_vline(xintercept = median[[j]]) +
				theme_bw() +
				theme(panel.border = element_blank(), panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
				theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 12))
		} else if (i == j) {
			if (is.integer(finalParams[, i])) {
				binwidth = 1
			} else {
				binwidth = (max(finalParams[, i]) - min(finalParams[, i])) / 30
			}
			text <- grobTree(textGrob(median[[i]], x = 0.1, y = 0.95, hjust = 0, gp = gpar(col = "red")))

			# Density plots along the diagonal - plot parameter density
			plotList[[counter]] =
			ggplot(finalParams, aes_string(paramNames[i - 1], fill = 1)) +
				geom_density(alpha = 0.25, bw = binwidth) +
				geom_vline(xintercept = median[[i]]) +
				annotation_custom(text) +
				scale_y_continuous(breaks = NULL) +
				theme_bw() +
				theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
				theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 12))
		}else {
			# Above Diagonal - place correlations
			plotList[[counter]] <-
			ggally_text(correlation(finalParams[, c(i, j)]), color = "black") +
				theme_bw() +
				theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
		}
	}
}
# Put the plots in the matrix
ggmatrix(plotList, nVars, nVars,
xAxisLabels = paramNames,
yAxisLabels = paramNames) +
	theme(text = element_text(size = 18)) +
	ggtitle("Probability Density")