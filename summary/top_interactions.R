# load('../bigTable_ultrasens_all.Rdata')
# mainTab <- mainTab[complete.cases(mainTab[, adjustby]), ]
## gets the top interactions surveyed
load('../vibration_data/interaction/interaction_all.Rdata')
#load('../vibration_data/interaction_anova/interaction_all.Rdata')
colnames(anovaTables) <- c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'varname', 'interacting_covariate')
#colnames(anovaTables) <- c('pvalue',  'Ftest', 'df', 'ddf', 'varname', 'interacting_covariate')
anovaTables[, 'term'] <- rownames(anovaTables)
recomputePvalue <- function(allData, zStatColName, pValColName) {
	### some pvalues estimated at 0 because test statistics so large; recompute their pvalues
	zeroPval <- !is.na(allData[,pValColName]) & (allData[,pValColName] == 0)
	allData[zeroPval, pValColName] <- pnorm(abs(allData[zeroPval, zStatColName]), lower.tail=F)*2 #two sided pvalue
	return(allData)
}

anovaTables <- recomputePvalue(anovaTables, 'z', 'pvalue')
load('../dictionary/dictionary.Rdata')
### sort by sample size?
interactionPvals <- merge(dictionary, anovaTables, by.x='varname', by.y='varname')
interactionPvals <- interactionPvals[order(-log10(interactionPvals$pvalue)*interactionPvals$N, decreasing=T), ]
interactionPvals.large <- subset(interactionPvals, N > 6500)

## ignore breast cancer and prostate cancer for now
interactionPvals.large <- interactionPvals.large[!(interactionPvals.large$varname %in% c('prostate_cancer_self_report', 'breast_cancer_self_report')), ]
##
## ignore ones that have small numbers in the case group:
interactionPvals.large <- interactionPvals.large[!(interactionPvals.large$varname %in% c('LBDHD', 'LBDHBG', 'LBDHCV', 'private_water_source', 'use_water_treatment')), ]
## then filter out small groups: RIDRETH5, RIDRETH12 (Other and Other Hispanic due to small sample)
interactionPvals.large <- interactionPvals.large[-grep('RIDRETH15', interactionPvals.large$term), ]
interactionPvals.large <- interactionPvals.large[-grep('RIDRETH12', interactionPvals.large$term), ]
## filter out ones that don't make sense like bmi/BMXBMI

nonSense <- which(interactionPvals.large$varname == 'BMXBMI' & interactionPvals.large$interacting_covariate == 'bmi')
interactionPvals.large <- interactionPvals.large[-nonSense, ]

interactionPvals.large <- interactionPvals.large[order(interactionPvals.large$pvalue), ]

save(interactionPvals.large, file='../vibration_data/interaction/top_interactions.Rdata')


