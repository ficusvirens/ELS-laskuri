# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# COUNTTOTMASS: laskee kullekin havaintopäivälle kokonaisbiomassan
# dataTable = taulukko, jossa oleelliset muuttujat ja päivämäärät; palauttaa samanmuotoisen taulukon
countTotMass <- function(dataTable) {

	# jos dataa ei ole, ei lasketa mitään
	if (nrow(dataTable) == 0) {
		newTable = data.frame(asema = numeric(), pvm = numeric(), paiva = numeric(), kk = numeric(), vuosi = numeric(), biomassa = numeric()) 
		return (newTable)
	}

	# tästä asemat vektorina
	stations = 0
	stations <- unique(dataTable$asema) 
	
	newTable = numeric()
	
	for (i in 1:length(stations)) {
	
		tempTable <- subset(dataTable, asema == stations[i])
		# tästä duplikaattipäivämäärät vektorina
		dups <- sort(unique(tempTable[duplicated(tempTable$pvm),]$pvm))
	
		# yhdistetään saman päivän eri havainnot summaksi 
		for (j in 1:nrow(tempTable)) {
			if (tempTable$pvm[j] %in% dups) {
				temp = sum(subset(tempTable, (pvm == tempTable$pvm[j]))$biomassa, na.rm = TRUE)
				# muunto milligrammoiksi
				temp = temp/1000
				if (!is.nan(temp)) {
					tempTable$biomassa[j] = temp
				} else tempTable$biomassa[j] = tempTable$biomassa[j]/1000
			} else tempTable$biomassa[j] = tempTable$biomassa[j]/1000
		}
	
		# poistetaan duplikaattipäivämäärälliset havainnot, vain ensimmäinen (nyt summa) jää
		tempTable <- tempTable[!duplicated(tempTable$pvm),]
		newTable <- rbind(newTable, tempTable)
	}
	
	return (newTable)
}

