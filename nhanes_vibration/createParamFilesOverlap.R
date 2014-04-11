## creates a param file

## go through and create the params, one for each variable
vars <- data.frame(
	var=c(
	'I(scale(log(LBXVID)))', 
	'I(scale(log(LBXBEC)))', 
	'I(scale(log(LBXBCD)))', 
	'I(scale(log(LBXCOT)))', 
	'physical_activity', 
	'drink_five_per_day', 
	'I(scale(log(LBXLYC)))', 
	'I(scale(log(LBXALC)))',
	'SMD410', 
	'SMQ020'
	),
	varname=c(
		'LBXVID',
		'LBXBEC',
		'LBXBCD',
		'LBXCOT',
		'physical_activity',
		'drink_five_per_day',
		'LBXLYC',
		'LBXALC',
		'SMD410',
		'SMQ020'
	), stringsAsFactors=F
)


adjusters <-  c('male', 'SES_LEVEL', 'education', 'black', 'mexican', 'other_hispanic', 'other_eth', 'BMXBMI', 'cad', 'any_diabetes','any_cancer_self_report', 'LBXBEC', 'LBXCOT', 'drink_five_per_day', 'physical_activity')

family_func <- 'cox'
outpath <- '../out'
inpath <- '../bigTable_ultrasens.Rdata'
parampath <- '../out'
k <- 10

script <- c()
params <- list()
index <- 1
for(ii in 1:nrow(vars)) {
	for(kk in 1:k) {
		param <- list()
		param$outpath <- file.path(outpath, sprintf('%s_%i_vibration.Rdata', vars[ii, 'varname'], kk))
		param$varname <- vars[ii, 'varname']
		param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ %s + RIDAGEYR + cluster(area)', vars[ii, 'var']) 
		param$adjusters <- setdiff(adjusters, vars[ii, 'varname'])
		param$family <- family_func
		param$inpath <- inpath
		param$paramfile <- file.path(parampath, sprintf('%s_%i_param.Rdata', vars[ii, 'varname'], kk))
		param$k <- kk
		params[[index]] <- param
		jobname <- sprintf('%s_%i', vars[ii, 'varname'], kk)
		outname <- file.path(outpath, sprintf('%s_%i.out', vars[ii, 'varname'], kk))
		pfile <- param$paramfile
		script <- c(script, sprintf('bsub -q normal -J %s -o %s Rscript vibrationAnalysisK.R -p %s', jobname, outname, pfile))
		index <- index + 1
	}
}


### now write everything out
for(ii in 1:length(params)) {
	param <- params[[ii]]
	save(param, file=param$paramfile)
}

cat(paste(script, collapse="\n"))
cat("\n")

