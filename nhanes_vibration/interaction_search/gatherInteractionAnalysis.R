directory <- '~/EWAS/ultraadjust/vibration_data/interaction/individual/'

files <- dir(directory, '_interaction.Rdata')


anovaTable.all <- c()
#interactionTerms.all <- c()
for(f in files) {
	print(f)
	load(file.path(directory, f))
	anovaTable.all <- rbind(anovaTable.all, anovaTables)
	#interactionTerms.all <- rbind(interactionTerms.all, interactionTerms)
}

anovaTables <- anovaTable.all
#interactionTerms <- interactionTerms.all

#save(anovaTables, interactionTerms, file=file.path(directory, 'interaction_all.Rdata'))

save(anovaTables, file=file.path(directory, 'interaction_all.Rdata'))