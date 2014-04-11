### gets data that overlaps with each other

source('~/EWAS/ewas_pipeline/bigVariateTable_join.R', chdir=T)
clustervars <- c('SEQN', 'WTMEC2YR', 'SDMVPSU', 'SDMVSTRA')
adjustby <- c('RIDAGEYR', 'male', 'SES_LEVEL', 'education', 'black', 'mexican', 'other_hispanic', 'other_eth','BMXBMI', 'cad', 'any_diabetes', 'any_cancer_self_report')
vars <- c('LBXVID', 'LBXBEC', 'LBXBCD', 'LBXCOT', 'physical_activity', 'drink_five_per_day', 'LBXLYC', 'LBXALC','SMD410', 'SMQ020')

con <- dbCon()
bt.a <- bigTable(con, 'a')
bt.b <- bigTable(con, 'b')
bt.c <- bigTable(con, 'c')

death.a <- dbGetQuery(con, 'select SEQN, MORTSTAT, PERMTH_EXM from nhanes.death_a')
death.b <- dbGetQuery(con, 'select SEQN, MORTSTAT, PERMTH_EXM from nhanes.death_b')
death.c <- dbGetQuery(con, 'select SEQN, MORTSTAT, PERMTH_EXM from nhanes.death_c')
death <- rbind(death.a, death.b, death.c)


allvars <- c(adjustby, vars, clustervars)
## now rbind them together with the variables

cols.a <- c(adjustby, c(clustervars, 'WTMEC4YR'), intersect(vars, colnames(bt.a)))
bt.a <- bt.a[, cols.a]
bt.a[, setdiff(allvars, colnames(bt.a))] <- NA

cols.b <- c(adjustby, c(clustervars, 'WTMEC4YR'), intersect(vars, colnames(bt.b)))
bt.b <- bt.b[, cols.b]
bt.b[, setdiff(allvars, colnames(bt.b))] <- NA

cols.c <- c(adjustby, c(clustervars), intersect(vars, colnames(bt.c)))
bt.c <- bt.c[, cols.c]
bt.c[, 'WTMEC4YR'] <- NA
bt.c[, setdiff(allvars, colnames(bt.c))] <- NA

bigTab <- rbind(bt.a, bt.b, bt.c)
bigTab <- merge(bigTab, death)
mainTab <- bigTab[complete.cases(bigTab), ]
done <- dbDisconnect(con)
#

codeArea <- function(dat) {
	### takes SDMVPSU and SDMVSTRA and codes a new area variable
	psu <- unique(dat$SDMVPSU)
	strata <- unique(dat$SDMVSTRA)
	dat[, 'area'] <- ''
	for(ii in 1:length(psu)) {
		for(jj in 1:length(strata)) {
			dat[dat$SDMVPSU == psu[ii] & dat$SDMVSTRA == strata[jj], 'area'] <- paste(psu[ii], strata[jj], sep="_")
		}
	}
	return(dat)
}

mainTab <- codeArea(mainTab)

save(mainTab, clustervars, vars, adjustby, file='~/EWAS/ultraadjust/bigTable_ultrasens.Rdata')
remove(list=ls())