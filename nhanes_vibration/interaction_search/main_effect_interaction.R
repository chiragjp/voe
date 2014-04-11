library('survival')
source('nhanes_weights.R')
source('../code/utility.R')
load('../bigTable_ultrasens_all.Rdata')


mainTab <- addWeights(mainTab, c('1999-2000', '2001-2002', '2003-2004'))
covariates <- c('male', 'RIDAGEYR', 'SES_LEVEL', 'education',  'RIDRETH1', 'bmi', 'any_cad', 'any_family_cad',  'any_diabetes', 'any_cancer_self_report', 'current_past_smoking', 'drink_five_per_day', 'physical_activity', 'LBXTC','any_ht')


#base_formula <- '~RIDAGEYR + male + LBXMC + cluster(area)'
#testTab <- mainTab[complete.cases(mainTab[, c(all.vars(as.formula(base_formula)), covariates)]), ]
testTab <- mainTab
testTab$RIDRETH1 <- factor(testTab$RIDRETH1, levels=c(3,1,2,4,5))
testTab$SES_LEVEL <- factor(testTab$SES_LEVEL, levels=c(3,1,2))
testTab$education <- factor(testTab$education, levels=c(3,1,2))
testTab$bmi <- factor(testTab$bmi, levels=c(2, 1, 3, 4, 5))
testTab$current_past_smoking <- factor(testTab$current_past_smoking, levels=c(1, 2, 3))


##
#test <- coxph(Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + LBXMC + cluster(area), data=testTab, weights=testTab$weight)
varsWithTC <- c('LBXMC','LBXPLTSI','DR1TSFAT','DR1TTFAT','DR1TS160','DR1TM201','DR1TMFAT','DR1TS180','DR1TM181','LBXHGB','DR1TS140','LBDEONO')

interResults <- c()
for(ii in 1:length(varsWithTC)) {
	print(varsWithTC[ii])
 	formul <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + I(scale(log(%s+.1)))*LBXTC + cluster(area)', varsWithTC[ii])
	myDat <- testTab[complete.cases(testTab[, c(covariates, 'weight', all.vars(as.formula(formul)))]), ]
	test <- coxph(as.formula(formul), data=myDat, weights=myDat$weight)
	interResults <- rbind(interResults, coef(summary(test)))
}


varsWithFCAD <- c('LBXRDW', 'LBXSAPSI')
for(ii in 1:length(varsWithFCAD)) {
	print(varsWithFCAD[ii])
 	formul <- sprintf('Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + I(scale(log(%s+.1)))*any_family_cad + cluster(area)', varsWithFCAD[ii])
	myDat <- testTab[complete.cases(testTab[, c(covariates, 'weight', all.vars(as.formula(formul)))]), ]
	test <- coxph(as.formula(formul), data=myDat, weights=myDat$weight)
	interResults <- rbind(interResults, coef(summary(test)))
}
