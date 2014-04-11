### creates a specific set of param files for variables
library(getopt)
spec <- matrix(c(
				'inpath', 'i', 1, 'character',
				'varname', 'v', 1, 'character',
				'strat_varname', 'a', 2, 'character',
				'stratum', 's', 2, 'integer'),
				 nrow=4, byrow=TRUE);
opt <- getopt(spec)
variableName <- opt$varname
inpath <- opt$inpath
stratum <- 0
if(!is.null(opt$stratum)) {
	stratum <- opt$stratum  ## for stratified analysis
}

strat_varname <- opt$strat_varname  ## for stratified analysis

cat(sprintf('%s,%s,%i\n',variableName,inpath,stratum))

source('../../xwas mortality/getVariables.R',chdir=T)
variables <- variables[variables$varname == variableName, ]
### 

### creates the param files and the script
#adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'
#adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'
adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'
adjusters <- paste(adjusters, '+ LBXBEC + LBXBCD + LBXGTC + LBXSCA + URXUCR + URXUMA', sep=" ")
## put in LBXBEC, LBXCRP, LBXBCD, LBXSCA, URXUCR, URXUMA
family_func <- 'cox'
outpath <- '../../out'
parampath <- '../../params'
###

## remove the strat_varname from the list of adjusters
if(!is.null(strat_varname)) {
	adjusters <- sub('~', '', adjusters)
	adjustmentList <- unlist(strsplit(adjusters, '\\+'))
	index <- grep(strat_varname, adjustmentList)
	adjustmentList <- adjustmentList[-index]
	adjusters <- sprintf('~ %s', paste(adjustmentList, collapse='+'))
}

source('writeParamFiles.R')