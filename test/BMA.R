## test out BMA on the dataset?
library(BMA)
load('../../data/nhanes9904_VoE.Rdata')
adjusters <- '~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht'
### vitamin D
'LBXVID'

### prepare data.
adjusting <- c('SES_LEVEL', 'education', 'RIDRETH1', 'bmi', 'any_cad', 'any_family_cad', 'any_diabetes', 'any_cancer_self_report', 'current_past_smoking', 'drink_five_per_day', 'physical_activity', 'LBXTC', 'any_ht')
dat <- mainTab[, c(adjusting, c('PERMTH_EXM', 'MORTSTAT', 'LBXVID'))]
dat <- dat[complete.cases(dat), ]
dat <- subset(dat, PERMTH_EXM > 0)
dat[, 'SES_LEVEL'] <- factor(dat[, 'SES_LEVEL'], levels=c(3, 1,2 ))
dat[, 'education'] <- factor(dat[, 'education'], levels=c(3, 1, 2))
dat[, 'RIDRETH1'] <- factor(dat[, 'RIDRETH1'], levels=c(3, 1, 2, 4, 5))
dat[, 'bmi'] <- factor(dat[, 'bmi'], levels=c(2, 1, 3, 4, 5))
dat[, 'LBXVID'] <- scale(log(dat$LBXVID))

vid.bic <- bic.surv(Surv(PERMTH_EXM, MORTSTAT) ~ ., data = dat, 
                         factor.type = TRUE)

summary(vid.bic, conditional=FALSE, digits=2)