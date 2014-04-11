library(getopt, quietly=T)
spec <- matrix(c('infile', 'i', 1, 'character'),nrow=1, byrow=TRUE)

opt <- getopt(spec)
infile <- opt$infile
load(infile)

vibFrame <- as.data.frame(vibFrame)
colnames(vibFrame) <- c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'k')
cat(infile, sep="\n")
totalNa <- sum(is.na(vibFrame$estimate))
cat(sprintf('%i\n', totalNa))