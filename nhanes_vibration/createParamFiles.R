
### get all variables
source('../xwas mortality/getVariables.R')
#variables <- variables[intersect(which(variables$is_binary==1), grep('^LB', variables$varname )), ]
### 

### creates the param files and the script
adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'
#adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + any_ht'
family_func <- 'cox'
outpath <- '../out'
inpath <- '../bigTable_ultrasens_all.Rdata'
parampath <- '../params'
stratum <- 0 ### not a stratified analysis
###
source('writeParamFiles.R')


