### see createParamFiles.R or createParamFileForVariable.R

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

coreVariables <- c('RIDAGEYR', 'male', 'cluster(area)')

params <- list()
index <- 1
for(ii in 1:nrow(variables)) {
	myadjust <- adjusters
	### need to figure out k dynamically
	varname <- variables$varname[ii]
	myadjust <- modifyadjustment(varname, myadjust)
	k <- length(all.vars(as.formula(myadjust)))
	isBinary <- variables[ii, 'is_binary']
	categoricalLevels <- variables[ii, 'categorical_levels']
	series <- variables[ii, 'series']
	for(kk in 0:k) {  ## this should be from 0 onward
		param <- list()
		param$outpath <- file.path(outpath, sprintf('%s_%i_s%i_vibration.Rdata', varname, kk, stratum))
		param$varname <- varname
		### check if is not binary
 		core <- paste(setdiff(coreVariables, varname), collapse='+')
		
		if(!is.na(isBinary) & isBinary == 1) {
			param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ %s + %s', varname, core)
		} else if(!is.na(categoricalLevels)) {
			param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ factor(%s,levels=c(%s)) + %s', varname,categoricalLevels, core)
		} else {
			if(logvariable(varname, variables[ii, 'var_desc_ewas'])) {
				param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ I(scale(log(%s+.1))) + %s',varname, core) 
			} else {
				param$formula <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ I(scale(%s)) + %s', varname, core) 
			}
		}
		param$series <- series
		param$adjusters <- myadjust
		param$family <- family_func
		param$inpath <- inpath
		param$paramfile <- file.path(parampath, sprintf('%s_%i_s%i_param.Rdata', varname, kk, stratum))
		param$k <- kk
		param$stratum <- stratum
		params[[index]] <- param
		jobname <- sprintf('%s_%i_s%i', varname, kk, stratum)
		outname <- file.path(outpath, sprintf('%s_%i_s%i.out', varname, kk, stratum))
		pfile <- param$paramfile
		index <- index + 1
	}
}


### now write everything out
for(ii in 1:length(params)) {
	param <- params[[ii]]
	save(param, file=param$paramfile)
}