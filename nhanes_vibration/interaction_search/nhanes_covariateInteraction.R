library('survival')
library('getopt')
library('survey')
source('nhanes_weights.R')
source('../code/utility.R')
load('../bigTable_ultrasens_all.Rdata')

spec <- matrix(c('param_file', 'p', 1, 'character'), nrow=1, byrow=TRUE)
opt <- getopt(spec)
paramFile <- opt$param_file
load(paramFile)

varname <- param$varname
print(varname)
seriesStr <- unlist(strsplit(param$series, ';'))
#outdirectory <- '~/ultraadjust/vibration_data/interaction'
#outdirectory <- param$outpath
covariates <- c('male', 'RIDAGEYR', 'SES_LEVEL', 'education',  'RIDRETH1', 'bmi', 'any_cad', 'any_family_cad',  'any_diabetes', 'any_cancer_self_report', 'current_past_smoking', 'drink_five_per_day', 'physical_activity', 'LBXTC','any_ht')
base_formula <- param$formula

mainTab <- addWeights(mainTab, seriesStr)


testTab <- mainTab[complete.cases(mainTab[, c(all.vars(as.formula(base_formula)), covariates)]), ]
testTab$RIDRETH1 <- factor(testTab$RIDRETH1, levels=c(3,1,2,4,5))
testTab$SES_LEVEL <- factor(testTab$SES_LEVEL, levels=c(3,1,2))
testTab$education <- factor(testTab$education, levels=c(3,1,2))
testTab$bmi <- factor(testTab$bmi, levels=c(2, 1, 3, 4, 5))
testTab$current_past_smoking <- factor(testTab$current_past_smoking, levels=c(1, 2, 3))

addToBase <- function(base_formula, adjustingVariables) {
	form <- base_formula
	if(length(adjustingVariables)) {
		addStr <- as.formula(sprintf('~ . + %s', paste(adjustingVariables, collapse='+')))
		form <- update.formula(base_formula, addStr)
	}
	return(form)
}

## model without interaction
## add main term
### iterate through each covariate

interactionResult <- function(covar, mydata) {
	baseVariableName <- attr(terms(as.formula(base_formula)), 'term.labels')[1]
	interactionTerm <- sprintf('%s:%s', baseVariableName, covar)
	multTerm <- sprintf('%s*%s', baseVariableName, covar)
	args <- list(addToBase(base_formula, c(multTerm)), data=mydata, weights=mydata$weight)
	test <- do.call(coxph, args)
	coefs <- as.data.frame(coef(summary(test)))
	index <- c(grep('\\:', rownames(coefs)))
	return(coefs[index, ])
}

anovaTables <- data.frame()
covariates <- setdiff(covariates, varname)
for(ii in 1:length(covariates)) {
	covar <- covariates[ii]
	print(covar)
	res <- tryCatch(interactionResult(covar, testTab), error=function(e) {return(NULL);})
	#res <- interactionResult(covar, testTab)
	if(!is.null(res)) {
		res$varname <- varname
		res$adjusting_covariate <- covar
		anovaTables <- rbind(anovaTables, res)
	}
}

#outfile <- file.path(outdirectory, sprintf('%s_interaction.Rdata', varname))
save(anovaTables, file=param$outpath)
