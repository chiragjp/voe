## vibration of effects
## Chirag Patel chirag@hms.harvard.edu
## 02/20/14
library(survival, quietly=T)

run_model <- function(form, data, family='gaussian', ...) {
	args <- list(form, data = data, ...) 
	if(family == 'gaussian') {
		return(do.call(lm, args))
	}
	if(family == 'cox') {
		return(do.call(coxph, args))
	}
	if(family == 'binomial') {
		args <- list(form, data, family=binomial(), ...)
		return(do.call(glm, args))
	}
} 



conductVibrationForK <- function(base_formula,dataFrame,adjustby,k=1,family=c('gaussian', 'binomial', 'cox'), print_progress=T, ...) {
	initFrame <- function(nrows,ncols) {
		matrix(NA,nrows,ncols)
	}
	
	
	addToBase <- function(base_formula, adjustingVariables) {
		form <- base_formula
		if(length(adjustingVariables)) {
			addStr <- as.formula(sprintf('~ . + %s', paste(adjustingVariables, collapse='+')))
			form <- update.formula(base_formula, addStr)
		}
		return(form)
	}
	
	## todo:
	## check if adjustby in dataFrame
	## check family
	## check if k less than |adjustby|
	
	variablename <- attr(terms(base_formula), 'term.labels')[1]
	varname <- all.vars(as.formula(sprintf('~%s', variablename)))
	if(print_progress) print(varname);
	
	if(class(adjustby)=='formula') {
		adjustby <- attr(terms(adjustby), 'term.labels')
	}
	n <- length(adjustby)
	varComb <- combn(n, k)
	retFrame <- NULL
	retFrameCounter <- 1
	bicFrame <- NULL
	for(ii in 1:ncol(varComb)) { # cycle through each possibility
		if(print_progress) cat(sprintf('%i/%i\n',ii, ncol(varComb)));
		
		adjustingVariables <- adjustby[varComb[, ii]]
		strComb <- paste(sort(varComb[, ii]), collapse=',')
		form <- addToBase(base_formula,adjustingVariables)
		if(print_progress) print(form);
		
		## run the model
		est <- tryCatch(
			run_model(form, dataFrame, family, ...), 
			error=function(err) {
				message(err)
				return(NULL)
			}
		)
		
		if(!is.null(est)) {
			## collect the result
			## do unweightedEst here...
			
			frm <- coef(summary(est))
			bicMod <- getBIC(est) # do BIC
			## modify the above...
			### need to get nlevels of variable 
			rowIndex <- grep(varname, rownames(frm))
			nLevels <- length(rowIndex)
			
			if(length(rowIndex) & is.null(retFrame)) {
				nrows <- ncol(varComb) * nLevels 
				ncols <- ncol(frm)
				retFrame <- initFrame(nrows,ncols+2) ## need to add 2 columns for the combination and factor_level
				bicFrame <- initFrame(ncol(varComb), 3) #
				colnames(retFrame) <- c(colnames(frm), 'combination_index', 'factor_level')
				colnames(bicFrame) <- c('edf', 'bic', 'combination_index')
			}
			
			bicFrame[ii, 'combination_index'] <- ii
			bicFrame[ii, 'edf'] <- bicMod[1]
			bicFrame[ii, 'bic'] <- bicMod[2]
			
			for(jj in 1:length(rowIndex)) {
				retFrame[retFrameCounter, 1:ncol(frm)] <- frm[rowIndex[jj], ] 
				retFrame[retFrameCounter, ncol(frm)+1] <- ii
				retFrame[retFrameCounter, ncol(frm)+2] <- jj
				retFrameCounter <- retFrameCounter+1
			}	
			
		}
		
	}
	return(list(vibration=retFrame,bic=bicFrame, k=k,combinations=varComb, family=family, base_formula=base_formula, adjust=adjustby))
}

getBIC <- function(mod) {
	return(extractAIC(mod, k=log(mod$nevent))) # do BIC
}

recomputePvalue <- function(allData, zStatColName, pValColName) {
	### some pvalues estimated at 0 because test statistics so large; recompute their pvalues
	zeroPval <- !is.na(allData[,pValColName]) & (allData[,pValColName] == 0)
	allData[zeroPval, pValColName] <- pnorm(abs(allData[zeroPval, zStatColName]), lower.tail=F)*2 #two sided pvalue
	return(allData)
}

conductVibration <- function(base_formula,dataFrame,adjustby,family=c('gaussian', 'binomial', 'cox'), kMin=NULL, kMax=NULL, print_progress=T, ...) {	
	if(is.null(kMin)) {
		kMin <- 1
	}
	if(is.null(kMax)) {
		n <- length(attr(terms(adjustby), 'term.labels'))
		kMax <- n - 1
	}
	cat(sprintf('running models; k start:%i, k stop:%i\n', kMin, kMax))
	retFrame <- list()
	ii <- 1
	for(k in kMin:kMax) {
		vib <- conductVibrationForK(base_formula, dataFrame, adjustby, k, family, print_progress, ...)
		retFrame[[ii]] <- vib
		ii <- ii + 1
	}
	ret <- gatherFrames(retFrame)
	return(ret)
}

gatherVibration <- function(returnFrames) {
	## gathers up results from multiple runs; see conductVibration
	nrows <- c()
	for(ii in 1:length(returnFrames)) {
		nrows <- c(nrows, nrow(returnFrames[[ii]]$vibration))
	}
	
	retFrame <- matrix(nrow=sum(nrows), ncol=ncol(returnFrames[[1]]$vibration)+1)
	colnames(retFrame) <- c(colnames(returnFrames[[1]]$vibration), 'k')
	
	startIndex <- 1
	for(ii in 1:length(returnFrames)) {
		ncols <- ncol(returnFrames[[ii]]$vibration)
		retFrame[startIndex:(startIndex+nrows[ii]-1), 1:ncols] <- returnFrames[[ii]]$vibration
		retFrame[startIndex:(startIndex+nrows[ii]-1), ncols+1] <- returnFrames[[ii]]$k
		startIndex <- startIndex+nrows[ii]
	}
	return(retFrame)
}

gatherVibrationBIC <- function(returnFrames) {
	nrows <- c()
	for(ii in 1:length(returnFrames)) {
		nrows <- c(nrows, nrow(returnFrames[[ii]]$bic))
	}
	
	retFrame <- matrix(nrow=sum(nrows), ncol=ncol(returnFrames[[1]]$bic)+1)
	colnames(retFrame) <- c(colnames(returnFrames[[1]]$bic), 'k')
	
	startIndex <- 1
	for(ii in 1:length(returnFrames)) {
		ncols <- ncol(returnFrames[[ii]]$bic)
		retFrame[startIndex:(startIndex+nrows[ii]-1), 1:ncols] <- returnFrames[[ii]]$bic
		retFrame[startIndex:(startIndex+nrows[ii]-1), ncols+1] <- returnFrames[[ii]]$k
		startIndex <- startIndex+nrows[ii]
	}
	return(retFrame)	
}

column_headers <- function(vibFrame, family) {
	existingColnames <- colnames(vibFrame)
	newColnames <- NULL
	if(family == 'cox') {
		isRobust <- grep('robust', existingColnames)
		if(isRobust) {
			return(c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'factor_level', 'k'))
		} else {
			c('estimate', 'HR', 'se', 'z', 'pvalue', 'combination_index', 'factor_level', 'k')
		}
	} else if(family == 'gaussian') {
		## to do
		existingColnames[1] <- 'estimate'
		existingColnames[length(existingColnames) - 4] <- 'pvalue'
		return(existingColnames)
	} else if(family == 'binomial') {
		## to do
		existingColnames[1] <- 'estimate'
		existingColnames[length(existingColnames) - 4] <- 'pvalue'
		return(existingColnames)
	} 
	### fill in the rest later for other families
	return(existingColnames)
}

harmonizeFrame <- function(vibFrame, family) {
	vibFrame <- as.data.frame(vibFrame)
	colnames(vibFrame) <- column_headers(vibFrame, family)
	if(family %in% c('binomial')) {
		vibFrame$HR <- exp(vibFrame$estimate)
	}
	return(vibFrame)
}

gatherFrames <- function(returnFrames) {
	bic <- gatherVibrationBIC(returnFrames)
	vibration <- gatherVibration(returnFrames)
	combinations <- list()
	for(ii in 1:length(returnFrames)) {
		combinations[[ii]] <- returnFrames[[ii]]$combinations
	}
	family <- returnFrames[[1]]$family
	base_formula <- returnFrames[[1]]$base_formula
	adjust <- returnFrames[[1]]$adjust
	
	vibration <- harmonizeFrame(vibration, family)
	vibration <- recomputePvalue(vibration, 'z', 'pvalue')
	return(list(vibFrame=vibration, bicFrame=bic, combinations=combinations, adjust=adjust, family=family, base_formula=base_formula))
}