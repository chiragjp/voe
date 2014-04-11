source('vibration.R')
load('../bigTable_ultrasens_all.Rdata')


adjusters <- ~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht
vars <- all.vars(as.formula(adjusters))

keepvars <- c(vars, 'MORTSTAT', 'area', 'PERMTH_EXM', 'current_past_smoking', 'WTMEC2YR', 'RIDAGEYR', 'male')
tab <- mainTab[complete.cases(mainTab[, keepvars]), keepvars]


form.1 <- Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + cluster(area) + as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  factor(current_past_smoking)  + drink_five_per_day + physical_activity  + LBXTC + any_ht
mod.1 <- coxph(form.1, tab)
extractAIC(mod.1)
extractAIC(mod.1, k=log(mod.1$nevent)) # BIC
form.2 <- Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + cluster(area) + factor(current_past_smoking)
mod.2 <- coxph(form.2, tab)
extractAIC(mod.2)
extractAIC(mod.2, k=log(mod.2$nevent)) # BIC


tab$rand.1 <- rnorm(nrow(tab))
tab$rand.2 <- rnorm(nrow(tab))
tab$rand.3 <- rnorm(nrow(tab))
form <- Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + cluster(area) + as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  factor(current_past_smoking)  + drink_five_per_day + physical_activity  + LBXTC + any_ht + rand.1 + rand.2 + rand.3
mod <- coxph(form, tab)

aic.mod <- stepAIC(mod, scope=list(lower=~ factor(current_past_smoking) + RIDAGEYR + male + cluster(area)), scale = 0,
        direction = 'both',
        trace = 1, keep = NULL, steps = 1000,
        k = 2)

aic.mod2 <- stepAIC(mod, scope=list(lower=~ factor(current_past_smoking) + RIDAGEYR + male + cluster(area)), scale = 0,
        direction = 'both',
        trace = 1, keep = NULL, steps = 1000,
        k = log(mod$nevent))



#AIC = -2*mod$loglik[2] + 2*24 (24=number of coefficients (incl. each categorical level))

aicCompare <- function(formstr) {
	mod.w <- coxph(as.formula(formstr), tab, weights=tab$WTMEC2YR)
	aic.w <- extractAIC(mod.w, k=log(mod.w$nevent))
	mod.u <- coxph(as.formula(formstr), tab)
	aic.u <- extractAIC(mod.u,k=log(mod.w$nevent))
	return(data.frame(aic.u=aic.u[2], aic.w=aic.w[2]))
}

termlabs <- attr(terms(adjusters), 'term.labels')
basestr <- 'Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + cluster(area)'
extstr <- basestr
aicTest <- data.frame()
for(ii in 1:length(termlabs)) {
	term <- termlabs[ii]
	extstr <- sprintf('%s + %s', extstr, term)
	simplestr <- sprintf('%s + %s', basestr, term)
	aicTest <- rbind(aicTest, aicCompare(extstr))
	aicTest <- rbind(aicTest, aicCompare(simplestr))
}

