### janus effect

load('../vibration_metrics/vibration_metrics.Rdata')
vibration.summaries <- vibration.summaries[vibration.summaries$rHR < 20, ]

#### janus
vibration.summaries[, 'janus'] <- 0
vibration.summaries[vibration.summaries$HR_99 > 1 & vibration.summaries$HR_01 < 1, 'janus'] <- 1

trendForK <- function(varnameMod, factorLevel=1) {
	rws <- subset(summary_per_k, varname == varnameMod & factor_level == factorLevel)
	rws <- rws[order(rws$k), ]
	
	## increase or decrease effect?
	zeroHR <- rws[1, 'estimate']
	adjHR <- rws[nrow(rws), 'estimate']
	
	mod <- lm(I(-log10(pvalue)) ~ estimate, rws)
	cf <- coef(summary(mod))
	attenuate <- 0
	if(adjHR > 0 & zeroHR > 0) {
		if(adjHR < zeroHR) {
			attenuate <- 1
		}
	} else if(adjHR < 0 & zeroHR < 0) {
		if(adjHR > zeroHR) {
			attenuate <- 1
		}
	} else {
		attenuate <- NA
	}
		
	return(data.frame(varname = varnameMod, attenuate=attenuate, factor_level=factorLevel, estimate=cf[2,1], pvalue=cf[2,4]))
}


trends_for_k <- c()
for(ii in 1:nrow(vibration.summaries)) {
	varn <- vibration.summaries[ii, 'varname']
	factorL <- vibration.summaries[ii, 'factor_level']
	trn <- trendForK(varn, factorL)
	trends_for_k <- rbind(trends_for_k, trn)
}
trends_for_k <- as.data.frame(trends_for_k)
vibration.summaries <- cbind(vibration.summaries, trends_for_k[, c('attenuate', 'estimate', 'pvalue')])
#vibration.summaries[, 'traverse_05'] <- ''
#vibration.summaries[(vibration.summaries$pvalue_99 >= 0.05 & vibration.summaries$pvalue_01 <= 0.05), 'traverse_05'] <- 'has pvalues <'
vibration.summaries[, 'robust'] <- '99% of pvalues > 0.05'
vibration.summaries[vibration.summaries$pvalue_99 < 0.05, 'robust'] <- '99% of pvalues < 0.05'

janusPlot <- ggplot(vibration.summaries, aes(x=factor(janus), y=rHR))
janusPlot <- janusPlot + geom_jitter() + geom_boxplot() + facet_grid(. ~ robust) + scale_y_continuous(limits = c(1,2), breaks=seq(0,2, by=.1))
janusPlot <- janusPlot + xlab('Janus Pattern?') + ylab('Relative HR')

attenuatePlot <- ggplot(subset(vibration.summaries, !is.na(attenuate)), aes(x=factor(attenuate), y=rHR))
attenuatePlot <- attenuatePlot + geom_jitter() + geom_boxplot() + facet_grid(. ~ traverse_05) + scale_y_continuous(limits = c(1,2), breaks=seq(0,2, by=.1))
attenuatePlot <- attenuatePlot + xlab('Janus Pattern?') + ylab('Relative HR')
