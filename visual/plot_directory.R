# script to plot each variable
directory <- '../../vibration_data/all_v5/'
outdirec <- '../../plots/all_v4_small/'
#directory <- '../vibration_data/stratified/any_family_cad'
#outdirec <- '../plots/stratified/any_family_cad'
#plot_script <- 'plot_stratified_file.R'
#plot_script <- 'plot_contour_file.R'
plot_script <- 'plot_file.R'

dictionaryFile <- '../../data/dictionary/dictionary.Rdata'
filenames <- dir(directory, '*_gather.Rdata')

contour <- T
for(ii in seq_along(filenames)) {
	varname <- unlist(strsplit(filenames[ii], '_vibration_gather.Rdata'))[1]
	filein <- file.path(directory, filenames[ii])
	#fileout <- file.path(outdirec, sprintf('%s.pdf', varname))
	cat(sprintf('Rscript %s -i %s -o %s -v %s -d %s\n', plot_script, filein, outdirec, varname, dictionaryFile))
}

