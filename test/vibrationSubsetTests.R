source('vibration.R')
source('utility.R')
load('../bigTable_ultrasens_all.Rdata')



adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'

vars <- all.vars(as.formula(adjusters))
testVar <- 'LBXMC'
allvars <- c(testVar, vars, c('MORTSTAT', 'PERMTH_EXM', 'area', 'WTMEC2YR', 'RIDAGEYR', 'male'))
tab <- mainTab[complete.cases(mainTab[, allvars]), allvars]

### subset the data for each dataset
## LBXMC and LBXTC
tab <- codeCholesterol(tab)
#table(tab$MORTSTAT, tab$cholesterol)

### now need to run vibration for each strata of variable

kMax <- 5
subTab <- subset(tab, cholesterol == 1)
vib.1 <- conductVibration(Surv(PERMTH_EXM, MORTSTAT) ~ I(scale(log(LBXMC))) + RIDAGEYR + male +  cluster(area), subTab, as.formula(adjusters), kMin=0, kMax=2, family='cox', weights=subTab$WTMEC2YR)

#kMax <- 5
#subTab <- subset(tab, cholesterol == 2)
#vib.2 <- conductVibration(Surv(PERMTH_EXM, MORTSTAT) ~ scale(log(LBXMC)) + RIDAGEYR + male + cluster(area), subTab, adjusters, kMin=0, kMax=kMax, family='cox', weights=subTab$WTMEC2YR)


#subTab <- subset(tab, cholesterol == 3)
#vib.3 <- conductVibration(Surv(PERMTH_EXM, MORTSTAT) ~ I(scale(log(LBXMC))) + RIDAGEYR + male + cluster(area), subTab, as.formula(adjusters), kMin=13, kMax=13, family='cox', weights=subTab$WTMEC2YR)

