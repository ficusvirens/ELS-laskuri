# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi


# FINDSENSITIVITY: hakee muuttujan herkkyysarvon
findSensitivity <- function(x, sensTable) {
	
	# etsitään laji herkkyystaulukosta
	matchNumber = match(x, sensitivityTable$taksoni)
	# jos ei löydy sellaisenaan, katsotaan onko matcheja pelkälle suvun nimelle
	if (is.na(matchNumber)) matchNumber = match(sub(" .*", "", x), sensitivityTable$taksoni)
	
	if (!is.na(matchNumber)) {
		# jos laji on kiellettyjen listalla
		if (sensTable$ES50[matchNumber] == 999) {
			return (NA) 
		} else return (sensTable$ES50[matchNumber])
	} else {
		 print(paste("Ei herkkyysarvoa:", x))
		return (NA)
	}
}

# DEEPCHECK: tarkistaa onko aseman syvyys alle vai yli 10 m
deepCheck <- function(x, depTable) {
	index = match(x, depTable$asema)
	if (depTable$syvyys[index] > 10) {
		return (TRUE)
	} else return (FALSE)
}


# CHECKAREA: tarkistaa, mihin vesimuodostumaan havaintoasema kuuluu
# x = havaintoasema character-muodossa
checkArea <- function(x) {
	for (i in 1:length(areas)) {
		if (x %in% areas[[i]]) return(names(areas)[i])
	}
}


# H: laskee Shannon-Wienerin indeksin 
H <- function(species) { # species = vektori, jossa eri lajien yksilömäärät
	speciesSum = 0

	for(i in 1:length(species)) {
		if (!is.na(species[i])) speciesSum = speciesSum + (species[i]/sum(species)) * log2(species[i]/sum(species))
	}

	return (-speciesSum)
}

BQI <- function(species, sensitivity) { # sensitivity = vektori, jossa eri lajien herkkyysarvot samassa järjestyksessä kuin species-vektorissa
	sensitivitySum = 0
	for(i in 1:length(species)) {
		if (!is.na(sensitivity[i])) sensitivitySum = sensitivitySum + (species[i]/sum(species) * sensitivity[i])
	}
	return (sensitivitySum * log10(length(species)+1))
}

# BBI: laskee pohjaeläinindeksin yhdelle näytteelle
BBI <- function(species, sensitivity, BQImax, Hmax) { 
	myH = H(species)
	myBQI = BQI(species, sensitivity)

	return ((myBQI/BQImax + myH/Hmax)/2 * ((1 - 1/sum(species)) + (1 - 1/length(species)))/2)
}

# BBI_ALL: laskee pohjaeläinindeksit koko datalle
BBI_all <- function(benthosTable, sensitivityTable, taxonTable, stationDepths, BBI_Inner, BBI_Outer) {

	# jos taulukko on tyhjä, ei lasketa mitään
	if (nrow(benthosTable) == 0) {
		colnames(benthosTable) <- c("asema", "pvm", "BBI")
		return (benthosTable)
	}
	
	stats = unique(benthosTable$asema)

	# tähän tulevat tulokset: BBI-arvo kullekin yksittäiselle havaintopäivälle ja -asemalle
	BBI_table = data.frame()

	for (i in 1:length(stats)) {
		statBenthos <- subset(benthosTable, asema == stats[i])
		dates = unique(statBenthos$pvm)
	
		# mikä vesimuodostuma kyseessä?
		thisArea = checkArea(stats[i])
	
		if (innerCheck(thisArea)) {
			BBI_this = BBI_Inner
		} else BBI_this = BBI_Outer
	
		if (deepCheck(stats[i], stationDepths)) {
			depth10 = "yli10m"
		} else depth10 = "alle10m"
	
		BBI_temp = numeric()
		
		merges = c("Oligochaeta", "Ostracoda", "Chironomidae")

		for (j in 1:length(dates)) {
			# saman päivän havainnot yhteen
			datBenthos <- subset(statBenthos, pvm == dates[j])
		
			for (k in 1:nrow(datBenthos)) {
				taxonMatch = match(datBenthos$nimi[k], taxonTable$laji)
				if (!is.na(taxonMatch)) datBenthos$nimi[k] = taxonTable$vastaavuus[taxonMatch]
			}
		
			# yhdistetään oligochaetat, ostracodat ja chironomidaet summaksi 
			for (k in 1:nrow(datBenthos)) {
				if (datBenthos$nimi[k] %in% merges) {
					temp = sum(subset(datBenthos, (nimi == datBenthos$nimi[k]))$lukumäärä, na.rm = TRUE)
				}
			}
			datBenthos <- datBenthos[!duplicated(datBenthos$nimi),]
			
			# poistetaan Ostracodat
			datBenthos <- subset(datBenthos, nimi != "Ostracoda")
		
			sens <- numeric()
			for (k in 1:nrow(datBenthos)) {
				# sens = vektori, jossa on kunkin havainnoista löytyvän pohjaeläimen herkkyysarvo samassa järjestyksessä
				sens = c(sens, findSensitivity(datBenthos$nimi[k], sensitivityTable))
			}
		
			BBI_temp = c(BBI_temp, BBI(datBenthos$lukumäärä, sens, BBI_this["BQImax", depth10], BBI_this["H'max", depth10]))
		}
	
		BBI_table_temp = data.frame(rep(stats[i], length(dates)), dates, BBI_temp)
		BBI_table = rbind(BBI_table, BBI_table_temp)
	
	}
	colnames(BBI_table) <- c("asema", "pvm", "BBI")
	
	return (BBI_table)

}
