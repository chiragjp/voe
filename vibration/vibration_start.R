## Chirag Patel
## 2/20/14
## sample code to compute the VoE in NHANES
# cluster(area) is to account for correlated observations in NHANES 
# observations are weighted using the weights= argument

source('./vibration.R') ## main vibration code in this file
source('./plot_vibration.R') ## code to do plotting

### load in the data from the website
load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))
## if the above is slow, manually download and load from:
# https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata
###
## restrict dataset to variable of interest (serum cadmium) and adjusting variables (makes code go quickly)
#dat <- mainTab[, c('WTMEC2YR', 'MORTSTAT', 'PERMTH_EXM', 'SES_LEVEL', 'RIDAGEYR', 'male', 'area','LBXTC', 'LBXBCD', 'current_past_smoking', 'any_cad', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1')]
dat <- mainTab[, c('MORTSTAT', 'SDDSRVYR', 'WTMEC4YR', 'PERMTH_EXM', 'SES_LEVEL', 'RIDAGEYR', 'male', 'area', 'LBXT4', 'current_past_smoking', 'any_cad', 'any_family_cad', 'any_cancer_self_report', 'bmi', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1', 'physical_activity', 'drink_five_per_day', 'LBXTC' )]
#dat <- mainTab[, c('MORTSTAT', 'PERMTH_EXM', 'SES_LEVEL', 'RIDAGEYR', 'male', 'area', 'LBXBCD', 'current_past_smoking', 'any_cad', 'any_family_cad', 'any_cancer_self_report', 'bmi', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1', 'physical_activity', 'drink_five_per_day', 'LBXTC' )]

## set of adjusting variables
#covariates <- ~ factor(current_past_smoking, levels=c(0,1,2)) + factor(SES_LEVEL, levels=c(2, 1, 0)) + any_cad + any_ht + any_diabetes + factor(education, levels=c(3,2,1)) + factor(RIDRETH1, levels=c(3, 1,2, 4, 5))
covariates <- ~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht

## base model is Cadmium (LBXBCD), age, sex; cluster(area) is to account for the correlated observations in NHANES
#basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(LBXBCD+.1))) + RIDAGEYR + male + cluster(area)
basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(LBXT4+.1))) + RIDAGEYR + male + cluster(area) 

## compute vibration for LBXBCD, serum levels of the heavy metal cadmium, in association with time to death
dat <- subset(dat, !is.na(WTMEC4YR))
dat <- dat[complete.cases(dat), ]
vib <- conductVibration(basemodel, dat, covariates, family='cox', weights=dat$WTMEC4YR) ## incorporates the sample weighting

## plot the VoE for cadmium
plot_vibration_cox(vib)
plot_vibration_cox(vib, type='contour', alpha=.3)
plot_vibration_cox(vib, type='contour', alpha=.3, adjustment_num=1) # plot VoE for models with and without smoking


