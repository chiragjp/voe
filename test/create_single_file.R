direc <- '../vibration_data/all_v4/'
files <- dir(direc, pattern='.Rdata')
varnames <- c()
rowsize <- c()
for (ii in seq_along(files)) {
	filename <- files[ii]
	print(filename)
	varname <- strsplit(filename, '_vibration_data.Rdata')[[1]]
	#vibData <- load_vibration_data(infile)
	infile <- file.path(direc, filename)
	load(infile)
	rowsize <- c(rowsize, nrow(vibFrame))
 	varnames <- c(varnames, varname)
}

bigFrame <- matrix(NA, sum(rowsize), 10)
from <- 1
for (ii in seq_along(files)) {
	filename <- files[ii]
	print(filename)
	varname <- strsplit(filename, '_vibration_data.Rdata')[[1]]
	infile <- file.path(direc, filename)
	load(infile)
	to <- from + nrow(vibFrame) - 1
	print(sprintf('%i:%i', from, to))
	bigFrame[from:to, 1:9] <- vibFrame
	bigFrame[from:to, 10] <- ii
	from <- from + nrow(vibFrame)
}
colnames(bigFrame) <- c(colnames(vibFrame), 'var_index')
save(bigFrame, varnames, file='../vibration_data/all_v4/bigFrame.Rdata')