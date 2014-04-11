library(getopt)
source('../vibration/vibration.R', chdir=T)

spec <- matrix(c(
				'directory', 'd', 1, 'character',
				'varname', 'v', 1, 'character', 
				'outdir', 'o', 1, 'character'),
				 nrow=3, byrow=TRUE);
opt <- getopt(spec)
direc <- opt$directory
varname <- opt$varname
outdir <- opt$outdir

## goes into the directory and gathers up the _vibration.Rdata files
filenames <- dir(direc, sprintf('^%s_[[:digit:]]{1,}_(s[[:digit:]]_){0,1}vibration.Rdata', varname))
outname <- file.path(outdir, sprintf('%s_vibration_gather.Rdata', varname))
retFrame <- list()
combinations <- list()
params <- c()
for(ii in seq_along(filenames)) {
	filename <- file.path(direc, filenames[ii])
	cat(sprintf('%s\n', filename))
	load(filename)
	nrows <- nrow(vibration$vibration)
	if(!is.null(param$stratum)) {
		params <- rbind(params, data.frame(k=rep(param$k,nrows),stratum=rep(param$stratum,nrows)))
	} 	
	retFrame[[ii]] <- vibration
	combinations[[ii]] <- vibration$combinations
}

vibFrame <- gatherVibration(retFrame)
bicFrame <- gatherVibrationBIC(retFrame)
save(vibFrame, combinations, bicFrame, params, file=outname)