source('../xwas mortality/getVariables.R')
vars <- variables
### 

### creates the param files and the script
#adjusters <-  c('male', 'SES_LEVEL', 'education', 'black', 'mexican', 'other_hispanic', 'other_eth', 'BMXBMI', 'cad', 'any_diabetes','any_cancer_self_report', 'LBXCOT', 'drink_five_per_day', 'physical_activity')
#adjusters <-  c('male', 'SES_0', 'SES_1', 'HS', 'LESS_HS', 'black', 'mexican', 'other_hispanic', 'other_eth', 'BMXBMI', 'cad', 'any_diabetes','any_cancer_self_report', 'LBXCOT', 'drink_five_per_day', 'physical_activity')
#adjusters <-  c('male', 'SES_0', 'SES_1', 'HS', 'LESS_HS', 'black', 'mexican', 'other_hispanic', 'BMXBMI', 'cad', 'any_diabetes','any_cancer_self_report', 'LBXCOT', 'drink_five_per_day', 'physical_activity')
#adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + BMXBMI + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'
adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + bmi + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'

family_func <- 'cox'
outpath <- '../vibration_data/interaction'
inpath <- '../bigTable_ultrasens_all.Rdata'
parampath <- '../params/interaction_v2'


logvariable <- function(varname, var_desc_ewas) {
	## log the biochemistry variables?
	## log the environmental
	## log the nutrients
	if(length(grep('^LB', varname))) {
		return(TRUE)
	} else if(length(grep('^UR', varname))) {
		return(TRUE)
	} else if(length(grep('^DR1', varname))) {
		return(TRUE)
	}
	
	return(FALSE)
}

modifyadjustment <- function(varname, adjusters) {
	newformula <- function(aList, index) {
		aList <- aList[-index]
		newform <- paste(aList, collapse=' + ')
		newform <- paste('~', newform)
		return(newform)
	}
	adjustingList <- all.vars(as.formula(adjusters))
	rawList <- attr(terms(as.formula(adjusters)), 'term.labels')
	
	if(varname %in% adjustingList) {
		index <- which(varname == adjustingList)
		return(newformula(rawList, index))
	}
	if(varname == 'LBXCOT' & length(grep('current_past_smoking', adjustingList))) {
		index <- which(adjustingList == 'current_past_smoking')
		if(length(index)) {
			return(newformula(rawList, index))
		}
		return(adjusters)
	} 
	if(length(grep('cad', varname)) & length(grep('cad', adjustingList))) {
		index <- which(adjustingList == 'any_cad')
		if(length(index)) {
			return(newformula(rawList, index))
		}
		return(adjusters)
	}
	
	if(length(grep('BMX', varname)) & length(grep('BMX', adjustingList))) {
		index <- grep('BMX', adjustingList)
		return(newformula(rawList, index))
	}
	return(adjusters)
}

params <- list()
index <- 1

for(ii in 1:nrow(vars)) {
	myadjust <- adjusters
	### need to figure out k dynamically
	varname <- vars[ii, 'varname']
	myadjust <- modifyadjustment(varname, myadjust)
	param <- list()
	param$outpath <- file.path(outpath, sprintf('%s_interaction.Rdata', vars[ii, 'varname']))
	param$varname <- vars[ii, 'varname']
	### check if is not binary
	isBinary <- vars[ii, 'is_binary']
	if(!is.na(isBinary) & isBinary == 1) {
		param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ %s + RIDAGEYR + male + cluster(area)', vars[ii, 'varname']) 
	} else {
		if(logvariable(vars[ii, 'varname'], vars[ii, 'var_desc_ewas'])) {
			param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ I(scale(log(%s+.1))) + RIDAGEYR + male + cluster(area)', vars[ii, 'varname']) 
		} else {
			param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ I(scale(%s)) + RIDAGEYR + male + cluster(area)', vars[ii, 'varname']) 
		}
		
	}
	param$series <- vars[ii, 'series']
	param$adjusters <- myadjust
	param$family <- family_func
	param$inpath <- inpath
	param$paramfile <- file.path(parampath, sprintf('%s_interaction_param.Rdata', vars[ii, 'varname']))
	params[[index]] <- param
	jobname <- sprintf('%s', vars[ii, 'varname'])
	outname <- file.path(outpath, sprintf('%s_interaction.out', vars[ii, 'varname']))
	pfile <- param$paramfile
	index <- index + 1
}


### now write everything out
for(ii in 1:length(params)) {
	param <- params[[ii]]
	save(param, file=param$paramfile)
}