# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# SCALED_ELS: muuntaa muuttujan arvon skaalatuksi ELS:ksi
# HUOM! ELS-rajataulukossa on oltava kuusi arvoa, jollei huonon alarajaa löydy niin sit NA loppuun.
scaled_ELS <- function(x, ELS_Class) { 
	if (is.na(x)) return (NA)

	# kun suuri arvo on hyvä
	if (ELS_Class[5] < ELS_Class[1]) { 
	
		# jos taulukossa ei ole huonon alarajaa, se asetetaan nollaksi 
		# !!HUOM!! tätä ei ole alkuperäisessä luokitteluohjeessa		
		if (is.na(ELS_Class[6])) ELS_Class[6] = 0 

		if (x <= ELS_Class[6]) return(0) 
		if (x <= ELS_Class[5]) return ((x - ELS_Class[6])/(ELS_Class[5] - ELS_Class[6]) * 0.2) 
		if (x <= ELS_Class[4]) return (0.2 + (x - ELS_Class[5])/(ELS_Class[4] - ELS_Class[5]) * 0.2)
		if (x <= ELS_Class[3]) return (0.4 + (x - ELS_Class[4])/(ELS_Class[3] - ELS_Class[4]) * 0.2)
		if (x <= ELS_Class[2]) return (0.6 + (x - ELS_Class[3])/(ELS_Class[2] - ELS_Class[3]) * 0.2)
		if (x <= ELS_Class[1]) return (0.8 + (x - ELS_Class[2])/(ELS_Class[1] - ELS_Class[2]) * 0.2)
		if (x > ELS_Class[1]) return (x/ELS_Class[1])
	}

	# kun pieni arvo on hyvä
	if (ELS_Class[5] > ELS_Class[1]) { 

		# jos taulukossa ei ole huonon alarajaa, olkoon se 2 * Valttava/huono-raja 
		# !!HUOM!! tätä ei ole alkuperäisessä luokitteluohjeessa
		if (is.na(ELS_Class[6])) ELS_Class[6] = ELS_Class[5] * 2
		
		if (x >= ELS_Class[6]) return(0) 
		if (x >= ELS_Class[5]) return ((x - ELS_Class[6])/(ELS_Class[5] - ELS_Class[6]) * 0.2) 
		if (x >= ELS_Class[4]) return (0.2 + (x - ELS_Class[5])/(ELS_Class[4] - ELS_Class[5]) * 0.2)
		if (x >= ELS_Class[3]) return (0.4 + (x - ELS_Class[4])/(ELS_Class[3] - ELS_Class[4]) * 0.2)
		if (x >= ELS_Class[2]) return (0.6 + (x - ELS_Class[3])/(ELS_Class[2] - ELS_Class[3]) * 0.2)
		if (x >= ELS_Class[1]) return (0.8 + (x - ELS_Class[2])/(ELS_Class[1] - ELS_Class[2]) * 0.2)
		if (x < ELS_Class[1]) return (ELS_Class[1]/x)
	}
}

# ELS_OVERVARIABLE: laskee ELS-arvot taulukon yhdestä muuttujasta
# dataTable = lähtödata taulukossa, variable = muuttuja, josta ELS-arvot lasketaan, ELS_Class = ELS-rajataulukko
ELS_OverVariable <- function(dataTable, variable, ELS_Class) { 
	ELS_Table <- sapply(dataTable[[variable]], FUN = function(x2) scaled_ELS(x2, ELS_Class[[variable]]))
	return (ELS_Table)

}

# ELS_ALL: laskee ELS-arvot taulukon kaikista muuttujista. HUOM! Muuttujat tarkistetaan ELS-rajataulukosta
# dataTable = lähtödata taulukossa, ELS_Class = ELS-rajataulukko
ELS_All <- function(dataTable, ELS_Class) {
	vars <- names(ELS_Class)

	temp = ELS_OverVariable(dataTable, vars[1], ELS_Class)
	ELS_Table <- round(temp, digits = 3)

	for (i in 2:length(vars)) {
		temp = ELS_OverVariable(dataTable, vars[i], ELS_Class)
		# pyöristetään kolmeen desimaaliin
		temp = round(temp, digits = 3)
		ELS_Table = data.frame(ELS_Table, temp)
	}
	
	# fiksataan taulukko vähän selkeämpään muotoon 
	ELS_Table = data.frame(dataTable$period, ELS_Table)
	colnames(ELS_Table) <- c("period", vars)
	
	return (ELS_Table)
}

# FYSCHEM: laskee taulukosta ELS-arvot fyskem-tilalle (keskiarvo näkösyvyydestä, Ntotista ja Ptotista)
fysChem <- function(dataTable) {
	fyskem <- subset(dataTable, select=c("secchi", "ntot", "ptot"))
	
	fyskem = round(rowMeans(fyskem, na.rm = TRUE), digits = 3)
	vars <- names(dataTable) %in% c("secchi", "ntot", "ptot")
	dataTable <- dataTable[!vars]
	dataTable = data.frame(dataTable, fyskem)
	return (dataTable)
}

# BIOL: laskee taulukosta ELS-arvot biologiselle tilalle (huonompi arvo a-klorofyllistä ja kasviplanktonin kokonaisbiomassasta, ja keskiarvo siitä ja BBI:stä)
biol <- function(dataTable) {
	# jos taulukossa on kokonaisbiomassadataa, sitä verrataan klorofylli a:n arvoon ja jätetään huonompi
	if ("biomassa" %in% colnames(dataTable)) {
		for (i in 1:nrow(dataTable)) {
			if (!is.na(dataTable$biomassa[i]) & !is.na(dataTable$chla[i])) {
				if (dataTable$biomassa[i] < dataTable$chla[i]) dataTable$chla[i] = dataTable$biomassa[i]
			}
		}
	}
	
	bioVars = intersect(c("chla", "BBI"), colnames(dataTable))
	
	bio <- subset(dataTable, select = bioVars)
	
	bio = round(rowMeans(bio, na.rm = TRUE), digits = 3)
	vars <- setdiff(colnames(dataTable), bioVars)
	dataTable <- subset(dataTable, select = vars)
	dataTable = data.frame(dataTable, bio)
	return (dataTable)
}

# ELS_TOWORDS: muuntaa ELS-arvon sanalliseksi arvioksi
ELS_ToWords <- function(x) {
	if (is.na(x)) return (NA)
	
	if (x > 0.8) return ("Erinomainen")
	if (x <= 0.8 & x > 0.6) return ("Hyva")
	if (x <= 0.6 & x > 0.4) return ("Tyydyttava")
	if (x <= 0.4 & x > 0.2) return ("Valttava")
	if (x <= 0.2) return ("Huono")
}

# ELS_TONUMBERS: muuntaa sanallisen ELS-arvon lukuarvoksi
ELS_ToNumbers <- function(x) {
	if (is.na(x)) return (NA)
	
	if (x == "Erinomainen") return (0.9)
	if (x == "Hyva") return (0.7)
	if (x == "Tyydyttava") return (0.5)
	if (x == "Valttava") return (0.3)
	if (x == "Huono") return (0.1)
}

# ELS_ALLWORDS: muuntaa ELS-arvot sanalliseksi arvioksi koko taulukosta
ELS_AllWords <- function(dataTable) {
	period <- dataTable$period
	dataTable$period <- NULL
	
	variables <- names(dataTable)
	
	ELS_Table <- sapply(dataTable[[variables[1]]], FUN = ELS_ToWords)
	for (i in 2:length(variables)) {
		ELS_Table <- data.frame(ELS_Table, sapply(dataTable[[variables[i]]], FUN = ELS_ToWords))
	}
	
	ELS_Table = data.frame(period, ELS_Table)
	colnames(ELS_Table) <- c("period", variables)
	return (ELS_Table)
}