library(getopt)
source('../vibration/vibration.R', chdir=T)

spec <- matrix(c('param_file', 'p', 1, 'character'), nrow=1, byrow=TRUE)
opt <- getopt(spec)
paramFile <- opt$param_file
load(paramFile)

load(param$inpath)

formula <- param$formula
adjusters <- param$adjusters
family <- param$family
outPath <- param$outpath
seriesName <- param$series
k <- param$k

### preprocess data.

seriesStr <- unlist(strsplit(param$series, ';'))

mainTab[, 'weight'] <- NA
if(length(seriesStr) == 3) {
	## 3 year weights
	#print('abc')
	mainTab[mainTab$SDDSRVYR==3, 'weight'] <- mainTab[mainTab$SDDSRVYR==3, 'WTMEC2YR']*(1/3)
	mainTab[mainTab$SDDSRVYR<3, 'weight'] <- mainTab[mainTab$SDDSRVYR<3, 'WTMEC2YR']*(2/3)
	
} else if(length(seriesStr) == 2) {
	### 1999-2002
	if(sum(seriesStr %in% c('1999-2000', '2001-2002'))==2) {
		#print('ab')
		mainTab[, 'weight'] <- mainTab[, 'WTMEC4YR']
	}
	## 1999-2000 2003-2004
	else if(sum(seriesStr %in% c('1999-2000', '2003-2004'))==2) {
		#print('ac')
		mainTab[, 'weight'] <- mainTab[, 'WTMEC2YR']*(1/2)
	}
	## 2001-2004
	else if(sum(seriesStr %in% c('2001-2002', '2003-2004'))==2) {
		#print('bc')
		mainTab[, 'weight'] <- mainTab[, 'WTMEC2YR']*(1/2)
	}
} else {
	mainTab[, 'weight'] <- mainTab[, 'WTMEC2YR']
}



vars <- c(all.vars(as.formula(formula)), all.vars(as.formula(adjusters)), 'weight')
mainTab <- mainTab[complete.cases(mainTab[, vars]), vars]
N <- nrow(mainTab)
timeElapsed <- system.time(
	vibration <- conductVibrationForK(as.formula(formula),mainTab,as.formula(adjusters),k=k,family=family, print_progress=T, weights=mainTab$weight)
	#vibration <- conductVibrationForK(as.formula(formula),mainTab,as.formula(adjusters),k=k,family=family, print_progress=T)
)

save(vibration, param, outPath, paramFile, k, N, timeElapsed, file=outPath)