addWeights <- function(mainTab, seriesStr) {
	
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
	return(mainTab)
}