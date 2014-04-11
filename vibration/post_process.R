## Chirag Patel
## 4/18/2013
### functions to post processes a vibFrame

library(MASS)

meanEstimate <- function(subFrame) {
	pval <- median(subFrame$pvalue)
	hr <- median(subFrame$estimate)
	return(data.frame(estimate=hr, pvalue=pval))
}

mean_manhattan <- function(arr) {
	### computes a manhattan distance (pairwise differences)
	### then computes the relative distance and means it
	dd <- as.matrix(dist(arr, method='manhattan'))
	dd <- dd / abs(arr)
	mean(dd[upper.tri(dd)])*100
}

cdfPerPvalue <- function(subFrame, pvalues=c(10^(-10:-2), .02, .03, .04, .05, .06, .07, .08, .09, .1)) {
	Fn <- ecdf(subFrame$pvalue)
	data.frame(pvalue=pvalues,cdf=Fn(pvalues), number=Fn(pvalues)*nrow(subFrame))
}

quantilesPvalue <- function(subFrame, probs=c(0.01, .25, 0.5, .75, 0.99)) {
	qs <- quantile(subFrame$pvalue, probs)
	data.frame(probs=probs, pvalue=qs)
}

quantilesHR <- function(subFrame, probs=c()) {
	### change this to estimate.
	qs <- quantile(subFrame$estimate, probs)
	data.frame(probs=probs, HR=exp(qs))
}

quantilesEstimate <- function(subFrame, probs) {
	qs <- quantile(subFrame$estimate, probs)
	data.frame(probs=probs, estimate=qs)
}


statPerK <- function(vibFrame) {
	### computes a mean HR and median p-value for each k and vibration for each k
	estLevel <- data.frame()
	ks <- sort(unique(vibFrame$k))
	levs <- unique(vibFrame$factor_level)
	for(ii in ks) {
		subFrame <- subset(vibFrame, k==ii)
		mn <- meanEstimate(subFrame)
		estLevel <- rbind(estLevel, data.frame(k=ii, estimate=mn$estimate, pvalue=mn$pvalue))
	}
	estLevel
}

statPerKandFactor <- function(vibFrame) {
	levs <- unique(vibFrame$factor_level)
	estLevels <- data.frame()
	for(ii in levs) {
		subFrame <- subset(vibFrame, factor_level == ii)
		estLevel <- statPerK(subFrame)
		estLevel$factor_level <- ii
		estLevels <- rbind(estLevels, estLevel)
	}
	return(estLevels)
}


summary.vibration <- function(vibFrame, bicFrame=NULL) {
	### this is for cox model.
	
	### take in a data.frame and compute all summary stats
	## do per factor? -- yes.
	# HR 99 and HR 1 -- if sign change for the 99 vs. 1?
	# P 99 and P 1; how many < thresholds
	# HR 99/ HR 1
	# -log10P1 + log10P99
	# stat per K (mean HR/ median p per K/factor)
	levs <- unique(vibFrame$factor_level)
	summaryFrame <- data.frame()
	pvalue_cdf <- data.frame()
	bestMod <- NULL;
	if(!is.null(bicFrame)) {
		combInd <- bicFrame[which.min(bicFrame[,2]), 3]
		bestK <- bicFrame[which.min(bicFrame[,2]), 4]
		bestMod <- subset(vibFrame, k == bestK & combination_index == combInd)
	}
	for(ii in levs) {
		subFrame <- subset(vibFrame, factor_level == ii)
		
		hrs <- quantilesHR(subFrame, probs=c(.01,.5, .99))
 		ps <- quantilesPvalue(subFrame, probs=c(.01,.5, .99))
		hr_01 <- hrs[1, 'HR']
		hr_50 <- hrs[2, 'HR']
		hr_99 <- hrs[3, 'HR']
		p_01 <- ps[1, 'pvalue']
		p_50 <- ps[2, 'pvalue']
		p_99 <- ps[3, 'pvalue']
		RHR <- hr_99/hr_01
		vibP <- -log10(p_01) + log10(p_99)
		frm <- data.frame(HR_01=hr_01, HR_50=hr_50, HR_99=hr_99,pvalue_01=p_01,pvalue_50=p_50, pvalue_99=p_99, rHR=RHR, rPvalue=vibP, factor_level=ii)
		if(!is.null(bestMod)) {
			bestSub <- subset(bestMod, factor_level == ii)
			frm$HR_bic <- bestSub[1, 'HR']
			frm$pvalue_bic <- bestSub[1, 'pvalue']
		}
		
		summaryFrame <- rbind(summaryFrame, frm)
		cdfPerP <- cdfPerPvalue(subFrame)
		cdfPerP$factor_level <- ii
		pvalue_cdf <- rbind(pvalue_cdf, cdfPerP)
	}
	
	perK <- statPerKandFactor(vibFrame)
	
	return(list(summary=summaryFrame, pvalue_cdf = pvalue_cdf, summary_per_k=perK))
}
summary.vibration.stratum <- function(vibFrame) {
	## gets a summary per stratum
	## for cox model.	
	strata <- unique(vibFrame$stratum)
	summaryFrame <- data.frame()
	summary_per_k <- data.frame()
	pvalue_cdf <- data.frame()
	for(ii in strata) {
		perStrat <- summary.vibration(subset(vibFrame, stratum == ii))
		perStrat$summary[, 'stratum'] <- ii
		perStrat$summary_per_k[, 'stratum'] <- ii
		perStrat$pvalue_cdf[, 'stratum'] <- ii
		summaryFrame <- rbind(summaryFrame, perStrat$summary)
		summary_per_k <- rbind(summary_per_k, perStrat$summary_per_k)
		pvalue_cdf <- rbind(pvalue_cdf, perStrat$pvalue_cdf)
	}
	
	return(list(summary=summaryFrame, pvalue_cdf=pvalue_cdf, summary_per_k=summary_per_k))
}

getContoursForPctile <- function(vib, pctiles=seq(.05, .95, by=.05)) {
	dens <- kde2d(vib$HR, -log10(vib$pvalue), n = 200)
	### this is from http://stackoverflow.com/questions/16225530/contours-of-percentiles-on-level-plot/16228938#16228938
	### HPDRegionplot code in the emdbook package
	dx <- diff(dens$x[1:2])
	dy <- diff(dens$y[1:2])
	sz <- sort(dens$z)
	c1 <- cumsum(sz) * dx * dy
	levels <- sapply(pctiles, function(x) {
	        approx(c1, sz, xout = 1 - x)$y
	})
	densityData <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
	return(list(levels=levels, densityData=densityData))
}


