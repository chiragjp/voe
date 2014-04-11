## Chirag Patel
## loads in a gathered data file

load_vibration_data <- function(pathToData) {
	load(pathToData)
	vibFrame <- init_vibration_frame(vibFrame)
	#vibFrame <- recomputePvalue(vibFrame, 'z', 'pvalue')
	if(!is.null(params)) {
		vibFrame <- cbind(vibFrame, stratum=params[, 'stratum'])
	}
	return(list(vibFrame=vibFrame, bicFrame=bicFrame, combinations=combinations))
}

init_vibration_frame <- function(vibFrame) {
	vibFrame <- as.data.frame(vibFrame)
	colnames(vibFrame) <- c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'factor_level', 'k')
	vibFrame
}