source('../vibration/post_process.R')
source('../visual/load_gathered_file.R')
directory <- '../../vibration_data/all_v5'

filenames <- dir(directory, '*_gather.Rdata')
vibration.summaries <- data.frame()
pvalue_cdf <- data.frame()
summary_per_k<- data.frame()
for(filename in filenames) {
	cat(sprintf('%s\n', filename))
	vibData <- load_vibration_data(file.path(directory, filename))
	varname <- unlist(strsplit(filename, '_vibration_gather.Rdata'))[1]
	summ <- summary.vibration(vibData$vibFrame, vibData$bicFrame)
	summ$summary$varname <- varname
	summ$pvalue_cdf$varname <- varname
	summ$summary_per_k$varname <- varname
	
	vibration.summaries <- rbind(vibration.summaries, summ$summary)
	pvalue_cdf <- rbind(pvalue_cdf, summ$pvalue_cdf)
	summary_per_k <- rbind(summary_per_k, summ$summary_per_k)
}

save(vibration.summaries,pvalue_cdf,summary_per_k,file='../vibration_metrics/vibration_metrics.Rdata')