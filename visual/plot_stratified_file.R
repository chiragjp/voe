source('../vibration/post_process.R')

library(getopt, quietly=T)
library(ggplot2, quietly=T)
library(RColorBrewer)

### relies on a strata information file

spec <- matrix(c('infile', 'i', 1, 'character',
				'outdirectory', 'o', 1, 'character',
				'varname', 'v', 1, 'character',
				'dictionary', 'd', 2, 'character'
				), nrow=4, byrow=TRUE)

opt <- getopt(spec)
infile <- opt$infile
outdirectory <- opt$outdirectory
varname <- opt$varname
cat(sprintf('%s\n', varname))

########### initialize dataframes
vibData <- load_vibration_data(infile)
vibFrame <- vibData$vibFrame
nStrat <- length(unique(vibFrame$stratum))
variableInformation <- NULL
if(!is.null(opt$dictionary)) {
	load(opt$dictionary)
	variableInformation <- dictionary[dictionary$varname == varname, ]
}
strataInformation <- read.csv(file.path(dirname(infile), 'strata_information.csv'), stringsAsFactors=F)
strataTitle <- strataInformation$stratTitle[1]
strataLabels <- strataInformation$stratLabels
probs <- c(.01, .5, .99)
hProbs <- probs
pQuant <- quantilesPvalue(vibFrame, probs) 
hQuant <- quantilesHR (vibFrame, hProbs)
RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)

#######
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p <- ggplot(vibFrame, aes(HR, -log10(pvalue)))
p <- p + geom_point(aes(colour=factor(stratum)), alpha=1/10) + scale_colour_manual(values=cbbPalette,name = strataTitle, labels=strataLabels)
p <- p + theme_bw()
p <- p + geom_hline(yintercept=-log10(0.05))
varnameTitle <- varname
if(!is.null(variableInformation)) {
	varDesc <- variableInformation[1, 'variable_description']
	varnameTitle <- sprintf('%s\n',varDesc)
}
### plot 01, 50, and 99	
p <- p + guides(colour = guide_legend(override.aes = list(alpha = 1)))
x <- max(vibFrame$HR)
p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant) 
p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant)
	 	
y <- max(c(-log10(vibFrame$pvalue), -log10(0.05)))
p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant) 
p <- p + geom_text(aes(x=HR, y=y, label=round(hProbs*100, 2), hjust=-.1, vjust=-.1), data=hQuant)

p <- p + ggtitle(sprintf('%s RHR=%.02f', varnameTitle, RHR)) + xlab('Hazard Ratio')

filename <- sprintf('%s_stratum.pdf', varname)
outfile <- file.path(outdirectory,filename)
ggsave(file=outfile, plot=p, h=7, width=7)