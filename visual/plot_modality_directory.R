directory <- '../../vibration_data/all_v4/'
outdirec <- '../../plots/modality/'
#directory <- '../vibration_data/stratified/any_family_cad'
#outdirec <- '../plots/stratified/any_family_cad'
#plot_script <- 'plot_stratified_file.R'
plot_script <- 'plot_modality_file.R'
do_adjustments <- TRUE
dictionaryFile <- '../../data/dictionary/dictionary.Rdata'
adjustmentFile <- '../../data/dictionary/adjustments.csv'


filenames <- dir(directory, '*_gather.Rdata')
kMax <- 13
adjustments <- read.csv(adjustmentFile, comment.char='#')
if(do_adjustments == T) {
	filenames <- paste(adjustments$adjustment_name, '_vibration_gather.Rdata', sep="")
	kMax <- nrow(adjustments) - 1
}


contour <- T
for(ii in seq_along(filenames)) {
	varname <- unlist(strsplit(filenames[ii], '_vibration_gather.Rdata'))[1]
	filein <- file.path(directory, filenames[ii])
	#fileout <- file.path(outdirec, sprintf('%s.pdf', varname))
	if(!do_adjustments & sum(adjustments$adjustment_name==varname)) {
		next
	}
	for(k in 1:kMax) {
		cat(sprintf('Rscript %s -i %s -o %s -v %s -d %s -a %i -f %s\n', plot_script, filein, outdirec, varname, dictionaryFile, k, adjustmentFile))
	}
	
}
