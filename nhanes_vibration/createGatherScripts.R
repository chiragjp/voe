### gathers the *_vibration files
library(getopt)

spec <- matrix(c(
				'donedir', 'd', 1, 'character',
				'outdir', 'o', 1, 'character'),
				 nrow=2, byrow=TRUE);
opt <- getopt(spec)

use_bsub <- F
#outdir <- '../out/stratified/'
#donedir <- '../out/stratified/gathered/'
outdir <- opt$outdir
donedir <- opt$donedir

filenames <- dir(outdir, '_vibration.Rdata')
varnames <- strsplit(filenames, 'vibration')
varnames <- unlist(lapply(varnames, function(arr) {
	elem <- arr[1]
	m <- gregexpr('\\_[[:digit:]]{1,}\\_', elem)
	indicies <- m[[1]]
	index <- indicies[length(indicies)]
	substr(elem, 1, index-1)
}))


## get the unique ones
varnames <- unique(varnames)

for(ii in 1:length(varnames)) {
	varname <- varnames[ii]
	#outname <- file.path(donedir, sprintf('%s_vibration.Rdata', varname))
	if(use_bsub) {
		outname <- file.path(donedir, sprintf('%s.out', varname))
		cat(sprintf('bsub -q normal -J %s -o %s Rscript gatherVibrationAnalysisK.R -d %s -v %s -o %s\n', varname, outname, outdir, varname, donedir))
	} else {
		cat(sprintf('Rscript gatherVibrationAnalysisK.R -d %s -v %s -o %s\n', outdir, varname, donedir))
	}
	
}