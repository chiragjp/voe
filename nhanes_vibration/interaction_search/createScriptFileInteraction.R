use_bsub <- T
paramdir <- '../out/interaction'
paramfiles <- dir(paramdir, pattern='_param.Rdata')
for(ii in 1:length(paramfiles)) {
	load(file.path(paramdir, paramfiles[ii]))
	jobname <- sprintf('%s', param$varname)
	outname <- file.path(paramdir, sprintf('%s.out', jobname))
	pfile <- param$paramfile
	script <- NULL
	if(use_bsub) {
		script <- sprintf('bsub -q normal -J %s -o %s Rscript nhanes_covariateInteraction.R -p %s', jobname, outname, pfile)
	} else {
		script <- sprintf('Rscript nhanes_covariateInteraction.R -p %s', pfile)
	}
	cat(sprintf('%s\n', script))
}
