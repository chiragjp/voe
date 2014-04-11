### need to recompute pvalues

directory <- '../vibration_data/all'

filenames <- dir(directory, '*_gather.Rdata')

recomputePvalue <- function(allData, zStatColName, pValColName) {
	### some pvalues estimated at 0 because test statistics so large; recompute their pvalues
	zeroPval <- !is.na(allData[,pValColName]) & (allData[,pValColName] == 0)
	allData[zeroPval, pValColName] <- pnorm(abs(allData[zeroPval, zStatColName]), lower.tail=F)*2 #two sided pvalue
	return(allData)
}

#collect <- data.frame()
for(ii in seq_along(filenames)) {
	varname <- unlist(strsplit(filenames[ii], '_vibration_gather.Rdata'))[1]
	filein <- file.path(directory, filenames[ii])
	load(filein)
	vibFrame <- as.data.frame(vibFrame)
	colnames(vibFrame) <- c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'k')
	vibFrame <- recomputePvalue(vibFrame, 'z', 'pvalue')
}