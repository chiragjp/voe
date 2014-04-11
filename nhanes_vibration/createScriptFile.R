### writes out a shell script to analyze data 

use_bsub <- T
paramdir <- '../../params'
paramfiles <- dir(paramdir, pattern='_param.Rdata')
for(ii in 1:length(paramfiles)) {
	load(file.path(paramdir, paramfiles[ii]))
	jobname <- sprintf('%s_%i_s%i', param$varname, param$k, param$stratum)
	outname <- file.path(paramdir, sprintf('%s.out', jobname))
	pfile <- param$paramfile
	script <- NULL
	if(use_bsub) {
		script <- sprintf('bsub -q short -W 11:59 -J %s -o %s Rscript vibrationAnalysisK.R -p %s', jobname, outname, pfile)
	} else {
		script <- sprintf('Rscript vibrationAnalysisK.R -p %s', pfile)
	}
	#filenumber <- (ii-1) %% splitnumber
	cat(sprintf('%s\n', script))
}
