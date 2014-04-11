codeBMI <- function(mainTab) {
	mainTab[, 'bmi'] <- NA
	mainTab[which(mainTab$BMXBMI < 18.5), 'bmi'] <- 1
	mainTab[which(mainTab$BMXBMI >= 18.5 & mainTab$BMXBMI < 25), 'bmi'] <- 2
	mainTab[which(mainTab$BMXBMI >= 25 & mainTab$BMXBMI < 30), 'bmi'] <- 3
	mainTab[which(mainTab$BMXBMI >= 30 & mainTab$BMXBMI < 35), 'bmi'] <- 4
	mainTab[which(mainTab$BMXBMI >= 35), 'bmi'] <- 5
	return(mainTab)
}

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

codeCholesterol <- function(dat) {
	dat[, 'cholesterol'] <- NA
	dat[which(dat$LBXTC < 200), 'cholesterol'] <- 1
	dat[which(dat$LBXTC >= 200 & dat$LBXTC <= 239), 'cholesterol'] <- 2
	dat[which(dat$LBXTC > 239), 'cholesterol' ] <- 3
	return(dat)
}