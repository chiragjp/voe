## Chirag Patel
## 2/20/14
## sample code to compute the VoE in NHANES
# cluster(area) is to account for correlated observations in NHANES 
# observations are weighted using the weights= argument



source('./vibration.R') ## main vibration code in this file
source('./plot_vibration.R') ## code to do plotting

### load in the data from the website
load(url('http://stanford.edu/~cjp/voe/nhanes9904_VoE.Rdata'))
## if the above is slow, manually download and load.
###
## restrict dataset to variable of interest (serum cadmium) and adjusting variables (makes code go quickly)
dat <- mainTab[, c('MORTSTAT', 'PERMTH_EXM', 'SES_LEVEL', 'RIDAGEYR', 'male', 'area', 'LBXBCD', 'current_past_smoking', 'any_cad', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1')]
## set of adjusting variables
covariates <- ~ factor(current_past_smoking, levels=c(0,1,2)) + factor(SES_LEVEL, levels=c(2, 1, 0)) + any_cad + any_ht + any_diabetes + factor(education, levels=c(3,2,1)) + factor(RIDRETH1, levels=c(3, 1,2, 4, 5))

## base model is Cadmium (LBXBCD), age, sex; cluster(area) is to account for the correlated observations in NHANES
basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(LBXBCD))) + RIDAGEYR + male + cluster(area) 

## compute vibration for LBXBCD, serum levels of the heavy metal cadmium, in association with time to death
vib <- conductVibration(basemodel, dat, covariates, family='cox', weights=mainTab$WTMEC2YR) ## incorporates the sample weighting

## plot the VoE for cadmium
plot_vibration_cox(vib)
plot_vibration_cox(vib, type='contour', alpha=.3)
plot_vibration_cox(vib, type='contour', alpha=.3, adjustment_num=1) # plot VoE for models with and without smoking


