source('../vibration/post_process.R')
source('load_gathered_file.R')
library(getopt, quietly=T)
library(ggplot2, quietly=T)
library(RColorBrewer)
### does a scatter plot for effects that have and do not have that adjustment

spec <- matrix(c('infile', 'i', 1, 'character',
				'outdirectory', 'o', 1, 'character',
				'varname', 'v', 1, 'character',
				'adjustment_index', 'a', 1, 'integer',
				'dictionary', 'd', 2, 'character',
				'adjustment_file','f',2, 'character'
				), nrow=6, byrow=TRUE)

# NBINS <- 75
opt <- getopt(spec)
infile <- opt$infile
outdirectory <- opt$outdirectory
varname <- opt$varname
findVar <- opt$adjustment_index
cat(sprintf('%s\n', varname))
########### initialize dataframes
vibData <- load_vibration_data(infile)

vibFrame <- vibData$vibFrame
combinations <- vibData$combinations
nFactor <- length(unique(vibFrame$factor_level))
variableInformation <- NULL
if(!is.null(opt$dictionary)) {
	load(opt$dictionary)
	variableInformation <- dictionary[dictionary$varname == varname, ]
}
adjustmentName <- findVar
if(!is.null(opt$adjustment_file)) {
	adjustmentIndexFile <- read.csv(opt$adjustment_file, comment.char="#")
	
	if(sum(varname == adjustmentIndexFile$adjustment_name)) {
		## need to take it out
		prevInd <- adjustmentIndexFile[varname == adjustmentIndexFile$adjustment_name, 'adjustment_index']
		adjustmentIndexFile <- adjustmentIndexFile[adjustmentIndexFile$adjustment_index != prevInd, ]
		adjustmentIndexFile[adjustmentIndexFile$adjustment_index > prevInd, 'adjustment_index'] <- adjustmentIndexFile[adjustmentIndexFile$adjustment_index > prevInd, 'adjustment_index'] -1
		#print(adjustmentIndexFile)
	}
	
	adjustmentName <- subset(adjustmentIndexFile, adjustment_index == findVar)$adjustment_name
	cat(sprintf('%s\n', adjustmentName))
}


## find where that variable exists..
# 9 is smoking
ks <- unique(vibFrame$k)
vibFrame[, 'has_variable'] <- 0
for(ii in 1:length(ks)) {
	k <- ks[ii]
	adjusters <- combinations[[ii]]
 	combIndex <- which(apply(adjusters, 2, function(arr) {sum(arr==findVar)})==1)  ## gives column
	if(length(combIndex)) {
		vibFrame[vibFrame$k == k & (vibFrame$combination_index %in% combIndex), 'has_variable'] <- 1
	}
}


############## keep it simple -- do a plot for each category
yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm=T)
xRange <- range(vibFrame$HR, na.rm=T)
probs <- c(0.01, 0.5, 0.99)
hProbs <- c(0.01, 0.5, 0.99)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for(ii in 1:nFactor) {
	subFrame <- subset(vibFrame, factor_level == ii)
	subFrame$factor_level <- NULL 
	contourData <- getContoursForPctile(subFrame)
	estLevel <- statPerK(subFrame)
	estLevel$HR <- exp(estLevel$estimate)
	pQuant <- quantilesPvalue(subFrame, probs) 
	hQuant <- quantilesHR (subFrame, hProbs)
	RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)
	RPvalue <- round(-log10(pQuant[1,'pvalue']) + log10(pQuant[3,'pvalue']), 2)
	
	p <- ggplot(subFrame, aes(HR, -log10(pvalue))) 
	######## add plotting code here
	p <- p + geom_contour(data=contourData$densityData, aes(x=x,y=y,z=z), breaks=contourData$levels, size=.1)
	p <- p + geom_point(aes(colour=factor(has_variable)), alpha=1/10) + scale_colour_manual(values=cbbPalette)
	p <- p + theme_bw()
	p <- p + geom_point(data=estLevel, color='red', shape=1) + geom_line(data=estLevel, color='red') 
	p <- p + geom_text(aes(HR, -log10(pvalue), label=k,vjust=-1), data=estLevel, color='black')
	
	x <- max(subFrame$HR)
	p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant) 
	p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant)
		 	
	y <- max(c(-log10(subFrame$pvalue), -log10(0.05)))
	p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant) 
	p <- p + geom_text(aes(x=HR, y=y, label=round(hProbs*100, 2), hjust=-.1, vjust=-.1), data=hQuant)
			
	p <- p + geom_hline(yintercept=-log10(0.05))
	p <- p + scale_x_continuous(limits=xRange) + scale_y_continuous(limits=yRange)
	### now write out
	filename <- sprintf('%s_adjustment_%i.pdf', varname, findVar)	
	varnameTitle <- sprintf('%s\n', varname)
	if(nFactor > 1) {
		filename <- sprintf('%s_%i_adjustment_%i.pdf', varname, ii, findVar)
		varnameTitle <- sprintf('%s (%i)', varname, ii)
	}
	
	if(!is.null(variableInformation)) {
		varDesc <- variableInformation[variableInformation$factor_level == ii, 'variable_description']
		factorDesc <- variableInformation[variableInformation$factor_level == ii, 'factor_description']
		if(nFactor > 1) {
			refGroup <- variableInformation[variableInformation$factor_level == 0, 'factor_description']
			varnameTitle <- sprintf('%s\n %s vs. %s', varDesc, factorDesc, refGroup)
		} else {
			varnameTitle <- sprintf('%s\n',varDesc)
		}
		
	}
	
	p <- p + ggtitle(sprintf('%s adjustment=%s\n', varnameTitle, adjustmentName)) 
	p <- p + xlab('Hazard Ratio') + ylab('-log10(pvalue)')
	p <- p + theme(legend.position='none')
	outfile <- file.path(outdirectory,filename)
	ggsave(file=outfile, plot=p, h=7, width=7)
}
