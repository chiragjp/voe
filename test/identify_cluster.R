source('post_process.R')
##
load('../vibration_data/all_v4/LBXTR_vibration_gather.Rdata')
vibFrame <- init_vibration_frame(vibFrame)
vibFrame <- recomputePvalue(vibFrame, 'z', 'pvalue')
nFactor <- length(unique(vibFrame$factor_level))
###

### how to identify clusters? kmeans/hclust/dbscan doesn't work well.
library(playwith)
playwith(plot(vibFrame$HR, -log10(vibFrame$pvalue), col=rgb(0,100,0,50,maxColorValue=255)))
save(upperPts, lowerPts, midPts, file='../cluster_data/LBXTR_clusters.Rdata')


###### now plot the lower, mid, and upper points
plot(vibFrame$HR, -log10(vibFrame$pvalue), col=rgb(0,100,0,50,maxColorValue=255))
points(vibFrame$HR[lowerPts], -log10(vibFrame$pvalue)[lowerPts], cex=1, col='green')
points(vibFrame$HR[midPts], -log10(vibFrame$pvalue)[midPts], cex=1, col='red')
points(vibFrame$HR[upperPts], -log10(vibFrame$pvalue)[upperPts], cex=1, col='black')

ks <- unique(vibFrame$k)
lower <- vibFrame[lowerPts,]
upper <- vibFrame[upperPts,]
mid <- vibFrame[midPts, ]
getVariableFrequency <- function(dataFrame, combinations, kIndex) {
	varCount <- rep(0, 13)
	for(ii in 1:nrow(dataFrame)) {
		k <- dataFrame[ii, 'k']
		ind <- which(kIndex == k)
		combinationIndex <- dataFrame[ii, 'combination_index']
		combos <- combinations[[ind]]
		vars <- combos[, combinationIndex]
		varCount[vars] <- varCount[vars] + 1 
	}
	return(varCount)
}
freqLower <- getVariableFrequency(lower, combinations, ks)
freqUpper <- getVariableFrequency(upper, combinations, ks)
freqMid <- getVariableFrequency(mid, combinations, ks)

