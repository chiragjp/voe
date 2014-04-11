directory <- '../vibration_data/all_v2/'
outdirec <- '../plots/all'

filenames <- dir(directory, '*_gather.Rdata')


#collect <- data.frame()
for(ii in seq_along(filenames)) {
	varname <- unlist(strsplit(filenames[ii], '_vibration_gather.Rdata'))[1]
	filein <- file.path(directory, filenames[ii])
	load(filein)
	vibFrame <- as.data.frame(vibFrame)
	colnames(vibFrame) <- c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'k')
 	totalNa <- sum(is.na(vibFrame$estimate))
	totalNaP <- sum(is.na(vibFrame$pvalue))
	if(totalNa | totalNaP)
		cat(sprintf('%s,%i,%i\n', varname, totalNa, totalNaP))
	#collect <- rbind(collect, data.frame(varname=varname, total_na=totalNa))
}

