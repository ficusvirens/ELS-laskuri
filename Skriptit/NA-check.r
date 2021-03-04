# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi


# NACHECK: selvittää, miltä vuosilta dataa puuttuu tietylle muuttujalle
naCheck <- function(dataTable, area, variable) {

	NAyearsC <- integer()
	
	# käydään läpi data ja otetaan ylös vuodet, joilta dataa puuttuu
	for (i in 1:nrow(dataTable)) {
		if (is.na(dataTable[[variable]][i])) {
			print(paste("Ei dataa: ", variable, " ", toString(dataTable$pvm[i]), " asema ", toString(dataTable$asema[i]), sep = ""))
			if ((!(dataTable$vuosi[i] %in% NAyearsC))) NAyearsC <- c(NAyearsC, dataTable$vuosi[i])
			}
		}
		
	NAyearsC = sort(NAyearsC)
	
	# lisätään tulokset globaaliin listaan
	NAyears[[area]] <<- c(NAyears[[area]], list(as.character(NAyearsC)))
	
	return (NAyearsC)	
}


# NAFULLYEARS: selvittää, miltä vuosilta data puuttuu kokonaan
# meanTable = taulukko, jossa on vuosittaiset keskiarvot kaikille suureille
naFullYears <- function(meanTable, area, variable) {

	NAyearsF <- character()
	
	for (i in 1:nrow(meanTable)) {
		if (is.na(meanTable[[variable]][i]) || is.nan(meanTable[[variable]][i])) {
		NAyearsF <- c(NAyearsF, meanTable$vuosi[i])
		}
	}

	# lisätään globaaliin listaan tähdet niiden vuosien perään joilta vain osa datasta puuttuu
	if (variable == "chla" || variable == "secchi" || variable == "ntot" || variable == "ptot") {
		if (length(NAyears[[area]][[variable]] != 0)) {
			for (i in 1:length(NAyears[[area]][[variable]])) {
				temp = match(NAyears[[area]][[variable]][i], NAyearsF)
				if (is.na(temp)) {
					temp2 <- paste(NAyears[[area]][[variable]][i], "*", sep = "")
					NAyears[[area]][[variable]][i] <<- temp2
				}
			}
		}
	} else NAyears[[area]][[variable]] <<- NAyearsF

	return (NAyearsF)
}