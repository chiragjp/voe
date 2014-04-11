source('vibration.R')
load('../bigTable_ultrasens_all.Rdata')

adjusters <- ~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) 
kMin <- 0
#kMax <- ceiling(length(adjusters)/2)
kMax <- 3

#adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'

vars <- all.vars(as.formula(adjusters))

tab <- mainTab[complete.cases(mainTab[, vars]), ]

### subset the data for each dataset
tab.1 <- subset(tab, RIDRETH1 == 3) ### whites

timeElap <- system.time(
	vib <- conductVibration(Surv(PERMTH_EXM, MORTSTAT) ~ factor(current_past_smoking, levels=c(1,2,3))+ RIDAGEYR + cluster(area), tab, adjusters, kMin=kMin, kMax=kMax, family='cox', weights=tab$WTMEC2YR)
)

#test <- conductVibrationForK(Surv(PERMTH_EXM, MORTSTAT) ~ factor(current_past_smoking, levels=c(1,2,3))+ male + RIDAGEYR + cluster(area), tab, adjusters, k=0, family='cox', weights=tab$WTMEC2YR)
#mod <- coxph(Surv(PERMTH_EXM, MORTSTAT) ~ factor(current_past_smoking)*male + RIDAGEYR +  education + cluster(area) + factor(RIDRETH1, levels=c(3, 1, 2, 4,5)), mainTab, mainTab$weights)
#mod <- coxph(Surv(PERMTH_EXM, MORTSTAT) ~ factor(current_past_smoking)*male + RIDAGEYR +  education + cluster(area) + factor(RIDRETH1, levels=c(3, 1, 2, 4,5)), mainTab, mainTab$weights)
######### interaction analysis
## first, need to check if variable of interest and covariate are categorical
### this is going to be hell

# coeffs <- coef(summary(mod))
# index <- grep('current_past_smoking', rownames(coeffs)) ## this gets all the terms that has the interaction term
# onesWithVarname <- rownames(coeffs)[index]
# ## which ones are paired with which?
# 
# library(multcomp)
# contr <- matrix(rep(0, nrow(coeffs)),1)
# contr[1, 2] <- 1
# contr[1, nrow(coeffs)] <- 1
# est <- glht(mod, contr)
# 
# # estimate
# coef(est)
# #se
# summary(est)$test$sigma[1]
# #zstat
# summary(est)$test$tstat[1]
# # pvalue
# summary(est)$test$pvalues[1]
##############