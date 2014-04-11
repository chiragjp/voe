## Belinda Burford
## 7/5/2013
## stiches .pdf files together

directory <- '../../plots/all_v4_small'

## Collect the names of the figures to be glued together
filenames <- dir(path = directory, pattern='*.pdf')
## The name of the pdf doc that will contain all the figures
outFileName <- "AllFigs.pdf"

setwd(directory)
system2(command = "pdftk", args = c(shQuote(filenames), "cat output", shQuote(outFileName)))


#pdfjam AllFigs.pdf --nup 5x8 --scale .7 --outfile ./Figure_S1_nature.pdf
#pdftk Figure_S1.pdf burst
#pdftk pg_0001.pdf stamp Fig1Header.pdf output pg1.pdf
#pdftk pg* cat output Figure_S1_b.pdf