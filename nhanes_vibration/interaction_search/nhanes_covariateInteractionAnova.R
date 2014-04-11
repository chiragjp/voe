library('survival')
library('getopt')
library('survey')
source('nhanes_weights.R')
source('utility.R')
load('../bigTable_ultrasens_all.Rdata')

spec <- matrix(c('param_file', 'p', 1, 'character'), nrow=1, byrow=TRUE)
opt <- getopt(spec)
paramFile <- opt$param_file
load(paramFile)

varname <- param$varname
print(varname)
seriesStr <- unlist(strsplit(param$series, ';'))
outdirectory <- '~/ultraadjust/vibration_data/interaction'
covariates <- c('male', 'RIDAGEYR', 'SES_LEVEL', 'education',  'RIDRETH1', 'bmi', 'any_cad', 'any_family_cad',  'any_diabetes', 'any_cancer_self_report', 'current_past_smoking', 'drink_five_per_day', 'physical_activity', 'LBXTC','any_ht')
base_formula <- param$formula

mainTab <- addWeights(mainTab, seriesStr)
mainTab <- codeBMI(mainTab)

testTab <- mainTab[complete.cases(mainTab[, c(all.vars(as.formula(base_formula)), covariates)]), ]
testTab$RIDRETH1 <- as.factor(testTab$RIDRETH1)
testTab$SES_LEVEL <- as.factor(testTab$SES_LEVEL+1)
testTab$education <- as.factor(testTab$education+1)
testTab$bmi <- as.factor(testTab$bmi+1)
testTab$RIDRETH1 <- C(testTab$RIDRETH1, contr.treatment, base=3)
testTab$education <- C(testTab$education, contr.treatment, base=3)
testTab$SES_LEVEL <- C(testTab$SES_LEVEL, contr.treatment, base=3)
testTab$bmi <- C(testTab$bmi, contr.treatment, base=2)

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

interactionResult <- function(covar, dsn) {
	### use regTermTest
	baseVariableName <- attr(terms(as.formula(base_formula)), 'term.labels')[1]
	interactionTerm <- sprintf('%s:%s', baseVariableName, covar)
	multTerm <- sprintf('%s*%s', baseVariableName, covar)
	test <- svycoxph(addToBase(base_formula, c(multTerm)) , dsn)
	regT <- regTermTest(test, interactionTerm)
	anTable <- data.frame(pvalue=regT$p, Ftest=regT$Ftest, df=regT$df, ddf=regT$ddf)
}

anovaTables <- data.frame()
dsn <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~weight, nest=T, data=testTab)
for(ii in 1:length(covariates)) {
	covar <- covariates[ii]
	print(covar)
	res <- tryCatch(interactionResult(covar, dsn), error=function(e) {return(NULL);})
	if(!is.null(res)) {
		res$varname <- varname
		res$adjusting_covariate <- covar
		anovaTables <- rbind(anovaTables, res)
	}
}

outfile <- file.path(outdirectory, sprintf('%s_interaction.Rdata', varname))
save(anovaTables, file=outfile)
