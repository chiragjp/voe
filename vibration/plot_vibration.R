
## vibration of effects 
## plot the VoE distribution
## Chirag Patel cjp@stanford.edu
## 07/05/13

source('post_process.R')
library(ggplot2)
library(RColorBrewer)
CBB_PALETTE <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


vib2d_cox <- function(vibObj, factor_num=1,nbins=20) {
	vibFrame <- vibObj$vibFrame
	nFactor <- length(unique(vibObj$factor_level))
	yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm=T)
	xRange <- range(vibFrame$HR, na.rm=T)
	probs <- c(0.01, 0.5, 0.99)
	hProbs <- c(0.01, 0.5, 0.99)
	subFrame <- subset(vibFrame, factor_level == factor_num)
	subFrame$factor_level <- NULL
	estLevel <- statPerK(subFrame)
	estLevel$HR <- exp(estLevel$estimate)
	pQuant <- quantilesPvalue(subFrame, probs)
	hQuant <- quantilesHR(subFrame, hProbs)
	
	RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)
	RPvalue <- round(-log10(pQuant[1,'pvalue']) + log10(pQuant[3,'pvalue']), 2)
	p <- ggplot(subFrame, aes(HR, -log10(pvalue)))
	if(sum(colnames(subFrame) == 'has_variable')) {
		p <- p + geom_hex(aes(colour=factor(has_variable)),bins=nbins) + scale_fill_gradientn(name='', colours=c('blue','yellow')) 
	} else {
		p <- p + geom_hex(bins=nbins) + scale_fill_gradientn(name='', colours=c('blue','yellow')) 
	}
	p <- p + geom_point(data=estLevel, color='red', shape=1) + geom_line(data=estLevel, color='red') 
	p <- p + geom_text(aes(HR, -log10(pvalue), label=k,vjust=-1), data=estLevel, color='black')
	pQuant$x <- max(subFrame$HR)
	p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant) 
	p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant)
	
	hQuant$y <- max(c(-log10(subFrame$pvalue), -log10(0.05)))
	p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant) 
	p <- p + geom_text(aes(x=HR, y=y, label=round(probs*100, 2), hjust=-.1, vjust=-.1), data=hQuant)
	p <- p + geom_hline(yintercept=-log10(0.05))
	p <- p + scale_x_continuous(limits=xRange) + scale_y_continuous(limits=yRange)
	p <- p + ggtitle(sprintf('RHR = %.02f\nRP = %.02f', RHR, RPvalue)) 
	p <- p + xlab('Hazard Ratio') + ylab('-log10(pvalue)') + theme_bw()
	return(p)
}

vibcontour_cox <- function(vibObj, factor_num=1, alpha=1) {	
	vibFrame <- vibObj$vibFrame
	subFrame <- subset(vibObj$vibFrame, factor_level == factor_num)
	contourData <- getContoursForPctile(subFrame) 
	subFrame$factor_level <- NULL 
	yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm=T)
	xRange <- range(vibFrame$HR, na.rm=T)
	probs <- c(0.01, 0.5, 0.99)
	hProbs <- c(0.01, 0.5, 0.99)
	estLevel <- statPerK(subFrame)
	estLevel$HR <- exp(estLevel$estimate)
	pQuant <- quantilesPvalue(subFrame, probs) 
	hQuant <- quantilesHR(subFrame, hProbs)
	RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)
	RPvalue <- round(-log10(pQuant[1,'pvalue']) + log10(pQuant[3,'pvalue']), 2)
	
	p <- ggplot(subFrame, aes(x=HR, y=-log10(pvalue))) 
	if(sum(colnames(subFrame) == 'has_variable')) {
		p <- p + geom_point(aes(colour=factor(has_variable)), alpha=alpha) + scale_colour_manual(values=CBB_PALETTE)
	} else {
		p <- p + geom_point(alpha=alpha)
	}
	p <- p + geom_contour(data=contourData$densityData, aes(x=x,y=y,z=z), breaks=contourData$levels, size=.3)
	p <- p + geom_point(data=estLevel, color='red', shape=1) + geom_line(data=estLevel, color='red') 
	p <- p + geom_text(aes(HR, -log10(pvalue), label=k,vjust=-1), data=estLevel, color='black')		
	pQuant$x <- max(subFrame$HR)
	p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant) 
	p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant)
	 	
	hQuant$y <- max(c(-log10(subFrame$pvalue), -log10(0.05)))
	p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant) 
	p <- p + geom_text(aes(x=HR, y=y, label=round(probs*100, 2), hjust=-.1, vjust=-.1), data=hQuant)

	p <- p + geom_hline(yintercept=-log10(0.05))
	p <- p + scale_x_continuous(limits=xRange) + scale_y_continuous(limits=yRange)
	p <- p + ggtitle(sprintf('RHR = %.02f\nRP = %.02f', RHR, RPvalue)) 
	p <- p + xlab('Hazard Ratio') + ylab('-log10(pvalue)') + theme_bw()
	return(p)
}

find_adjustment_variable <- function(vibObj, adjustment_num=1) {
	vibFrame <- vibObj$vibFrame
	combinations <- vibObj$combinations
	ks <- unique(vibFrame$k)
	vibFrame[, 'has_variable'] <- 0
	for(ii in 1:length(ks)) {
		k <- ks[ii]
		adjusters <- combinations[[ii]]
	 	combIndex <- which(apply(adjusters, 2, function(arr) {sum(arr==adjustment_num)})==1)  ## gives column
		if(length(combIndex)) {
			vibFrame[vibFrame$k == k & (vibFrame$combination_index %in% combIndex), 'has_variable'] <- 1
		}
	}
	vibObj$vibFrame <- vibFrame
	return(vibObj)
}

plot_vibration_cox <- function(vibObj, type=c('bin', 'contour'), factor_num=1, adjustment_num=NA, ...) {
	### plots the vibration of effects for a cox model
	if(length(type)) {
		type <- type[1]
	}
	
	if(!is.na(adjustment_num)) {
		vibObj <- find_adjustment_variable(vibObj, adjustment_num)
	}
	
	if(type == 'bin') {
		return(vib2d_cox(vibObj, factor_num, ...))
	} else if(type == 'contour') {	
		return(vibcontour_cox(vibObj, factor_num, ...))
	}
	
	return(NULL)
}