### gets data that overlaps with each other
source('~/EWAS/ewas_pipeline/bigVariateTable_join.R', chdir=T)
source('utility.R')
clustervars <- c('SEQN', 'WTMEC2YR', 'SDMVPSU', 'SDMVSTRA', 'SDDSRVYR')

#adjustby <- c('RIDAGEYR', 'male', 'SES_LEVEL', 'education', 'black', 'mexican', 'other_hispanic', 'other_eth','BMXBMI', 'cad', 'any_diabetes', 'any_cancer_self_report')

adjustby <- c('RIDAGEYR', 'male', 'SES_LEVEL', 'education', 'RIDRETH1','BMXBMI', 'any_cad', 'any_family_cad', 'any_ht', 'any_diabetes', 'any_cancer_self_report', 'DMDMARTL')
adjustby <- c(adjustby, "LBXCOT", 'drink_five_per_day', 'physical_activity', 'DR1TKCAL', 'LBXTC', 'MSYS')
adjustby <- c(adjustby, "current_past_smoking")


### 
source('../xwas mortality/getVariables.R')
# load('../xwas mortality/xwas_mortality.Rdata')
# bigSamples <- subset(allData, N >= 1000 & nevent >= 100)
# ## remove pharmaceuticals
# ## remove food and supplement consumption
# ## remove food recall
# ## remove demographics
# ## occupation should be in here.
# bigSamples <- bigSamples[!(bigSamples$var_desc_ewas %in% c('food recall', 'demographics')), ]
# bigSamples <- bigSamples[-grep('^DRD', bigSamples$varname), ]
# vars <- setdiff(bigSamples$varname, adjustby)
# remove(allData)
# remove(bigSamples)
vars <- unique(setdiff(variables$varname, adjustby))
remove(variables)
###

con <- dbCon()
bt.a <- bigTable(con, 'a')
bt.b <- bigTable(con, 'b')
bt.c <- bigTable(con, 'c')

death.a <- dbGetQuery(con, 'select SEQN, MORTSTAT, PERMTH_EXM from nhanes.death_a')
death.b <- dbGetQuery(con, 'select SEQN, MORTSTAT, PERMTH_EXM from nhanes.death_b')
death.c <- dbGetQuery(con, 'select SEQN, MORTSTAT, PERMTH_EXM from nhanes.death_c')
death <- rbind(death.a, death.b, death.c)

ocq_a <- dbGetQuery(con, 'select SEQN, occupation from nhanes.occupation_a')
ocq_b <- dbGetQuery(con, 'select SEQN, occupation from nhanes.occupation_b')
ocq_c <- dbGetQuery(con, 'select SEQN, occupation from nhanes.occupation_c')
vars <- union(vars, c('occupation'))

bt.a <- merge(bt.a, ocq_a, all.x=T)
bt.b <- merge(bt.b, ocq_b, all.x=T)
bt.c <- merge(bt.c, ocq_c, all.x=T)

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
done <- dbDisconnect(con)

bigTab$SES_LEVEL <- bigTab$SES_LEVEL+1
bigTab$education <- bigTab$education+1
bigTab$current_past_smoking <- bigTab$current_past_smoking+1
mainTab <- codeArea(bigTab)
mainTab <- codeBMI(mainTab)

adjustby <- c(adjustby, 'bmi')

save(mainTab, clustervars, vars, adjustby, file='~/EWAS/ultraadjust/bigTable_ultrasens_all.Rdata')
remove(list=ls())