### create a data file for each strata of LBXTC

source('../code/utility.R')
load('../bigTable_ultrasens_all.Rdata')

mainTab.original <- codeCholesterol(mainTab)
mainTab <- subset(mainTab.original, cholesterol == 1)
save(mainTab, file='../bigTable_ultrasens_tc_1.Rdata')
mainTab <- subset(mainTab.original, cholesterol == 2)
save(mainTab, file='../bigTable_ultrasens_tc_2.Rdata')
mainTab <- subset(mainTab.original, cholesterol == 3)
save(mainTab, file='../bigTable_ultrasens_tc_3.Rdata')

### and any_family_cad

mainTab <- subset(mainTab.original, any_family_cad == 1)
save(mainTab, file='../bigTable_ultrasens_family_cad_1.Rdata')
mainTab <- subset(mainTab.original, any_family_cad == 0)
save(mainTab, file='../bigTable_ultrasens_family_cad_0.Rdata')