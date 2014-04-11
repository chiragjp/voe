source('post_process.R')
directory <- '../vibration_data/bic'

filenames <- dir(directory, '*_gather.Rdata')
bicFrame <- data.frame()

for(filename in filenames) {
	cat(sprintf('%s\n', filename))
	vibData <- load_vibration_data(file.path(directory, filename))
	varname <- unlist(strsplit(filename, '_vibration_gather.Rdata'))[1]
	
	bicNum <- vibData$bicFrame[which.min(vibData$bicFrame[, 2]), 2]
	combNum <- vibData$bicFrame[which.min(vibData$bicFrame[, 2]), 3]
	k <- vibData$bicFrame[which.min(vibData$bicFrame[, 2]), 4]
	bicFrame <- rbind(bicFrame, data.frame(varname=varname, bic=bicNum, k=k, combination_index=combNum))
}

save(bicFrame, file='../vibration_data/bic/bic.Rdata')