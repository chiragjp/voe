### merges in the bic information in the main data stucture and resaves

directory <- '../vibration_data/all_v4/'
bicdirectory <- '../vibration_data/bic/gathered/'
outdirec <- '../vibration_data/all_v5/'

filenames <- dir(directory, '*_gather.Rdata')

for(ii in seq_along(filenames)) {
	#varname <- unlist(strsplit(filenames[ii], '_vibration_gather.Rdata'))[1]
	print(filenames[ii])
	filein <- file.path(directory, filenames[ii])
	bicFile <- file.path(bicdirectory, filenames[ii])
	fileout <- file.path(outdirec, filenames[ii])
	load(bicFile)
	remove(combinations, params, vibFrame)
	load(filein)
	save(combinations, params, vibFrame, bicFrame, file=fileout)
}