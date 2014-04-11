source('../vibration/post_process.R')
source('load_gathered_file.R')
library(getopt, quietly=T)
library(ggplot2, quietly=T)
library(RColorBrewer)
spec <- matrix(c('infile', 'i', 1, 'character',
				'outdirectory', 'o', 1, 'character',
				'varname', 'v', 1, 'character',
				'dictionary', 'd', 2, 'character'
				), nrow=4, byrow=TRUE)
HEIGHT <- 4
WIDTH <- 4
TEXTSIZE_PVAL <- 3
TEXTSIZE_HR <- 3
TEXTSIZE_K <- 3
TEXTSIZE_TITLE <- 9
NBINS <- 100
opt <- getopt(spec)
infile <- opt$infile
outdirectory <- opt$outdirectory
varname <- opt$varname
cat(sprintf('%s\n', varname))

########### initialize dataframes
vibData <- load_vibration_data(infile)
vibFrame <- vibData$vibFrame
###
nFactor <- length(unique(vibFrame$factor_level))
variableInformation <- NULL
if(!is.null(opt$dictionary)) {
	load(opt$dictionary)
	variableInformation <- dictionary[dictionary$varname == varname, ]
}
#############

### best model
bestMod <- NULL
# if(!is.null(vibData$bicFrame)) {
# 	bicFrame <- vibData$bicFrame
# 	combInd <- bicFrame[which.min(bicFrame[,2]), 3]
# 	bestK <- bicFrame[which.min(bicFrame[,2]), 4]
# 	bestMod <- subset(vibFrame, k == bestK & combination_index == combInd)
# }
###


############## keep it simple -- do a plot for each category
yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm=T)
xRange <- range(vibFrame$HR, na.rm=T)
probs <- c(0.01, 0.5, 0.99)
hProbs <- c(0.01, 0.5, 0.99)


for(ii in 1:nFactor) {
	subFrame <- subset(vibFrame, factor_level == ii)
	subFrame$factor_level <- NULL 
	estLevel <- statPerK(subFrame)
	estLevel$HR <- exp(estLevel$estimate)
	pQuant <- quantilesPvalue(subFrame, probs) 
	hQuant <- quantilesHR (subFrame, hProbs)
	RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)
	RPvalue <- round(-log10(pQuant[1,'pvalue']) + log10(pQuant[3,'pvalue']), 2)
	
	p <- ggplot(subFrame, aes(HR, -log10(pvalue))) 
	p <- p + geom_hex(bins=NBINS) + scale_fill_gradientn(name='', colours=c('blue','yellow')) 
	p <- p + theme_bw()
	
	p <- p + geom_point(data=estLevel, color='red', shape=1) + geom_line(data=estLevel, color='red') 
	p <- p + geom_text(aes(HR, -log10(pvalue), label=k,vjust=-1), data=estLevel, color='black', size=TEXTSIZE_K)
	
	x <- max(subFrame$HR)
	p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant) 
	p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant, size=TEXTSIZE_PVAL)
		 	
	y <- max(c(-log10(subFrame$pvalue), -log10(0.05)))
	p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant) 
	p <- p + geom_text(aes(x=HR, y=y, label=round(hProbs*100, 2), hjust=-.1, vjust=-.1), data=hQuant, size=TEXTSIZE_HR)
			
	p <- p + geom_hline(yintercept=-log10(0.05))
	p <- p + scale_x_continuous(limits=xRange) + scale_y_continuous(limits=yRange)
	
	## plot the best model by BIC
	if(!is.null(bestMod)) {
		p <- p + geom_point(data=subset(bestMod, factor_level == ii), color='cyan', size=5)
	}
	
	### now write out
	filename <- sprintf('%s.pdf', varname)
	
	varnameTitle <- sprintf('%s\n', varname)
	if(nFactor > 1) {
		filename <- sprintf('%s_%i.pdf', varname, ii)
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
	p <- p + ggtitle(sprintf('%s RHR = %.02f\nRP = %.02f', varnameTitle, RHR, RPvalue)) 
	p <- p + xlab('Hazard Ratio') + ylab('-log10(pvalue)')
	p <- p + theme(legend.position='none', 
	plot.title=element_text(size=TEXTSIZE_TITLE), 
	axis.title.x=element_text(size=TEXTSIZE_TITLE), 
	axis.title.y=element_text(size=TEXTSIZE_TITLE)
	); # end theme
	outfile <- file.path(outdirectory,filename)
	ggsave(file=outfile, plot=p, height=HEIGHT, width=WIDTH)
}
