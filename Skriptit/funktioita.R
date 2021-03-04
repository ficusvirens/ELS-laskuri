# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# CONVERT: muuttaa halutun muuttujan taulukon perusteella toiseen muotoon
# x = yksittäinen muuttuja, convertTable = taulukko, josta luetaan muutokset
convert <- function (x, convertTable) {

	for (i in 1:nrow(convertTable)) {
		if (x == convertTable[i, 1]) x = convertTable[i, 2]
	}
	
	return (x)
}


# INNERCHECK: jos annettu vesimuodostuma on tyyppiä "sisäsaaristo", palauttaa TRUE, muuten FALSE
innerCheck <- function(area) {
	temp = match(area, areaList$lyhenne)
	if (areaList$sisa[temp]) {
		return (TRUE)
	} else return (FALSE)
}


# INNERCHECKSTATION: jos annettu havaintoasema on sisäsaariston alueella, palauttaa TRUE, muuten FALSE
innerCheckStation <- function(x) {
	# tehdään asemista helpommin luettava lista
	areasLong = melt(areas)
	areasLong$value <- as.character(areasLong$value)
	
	# mihin vesistömuodostumaan asema kuuluu?
	myArea <- convert(x, areasLong)

	return (innerCheck(myArea))
}


# AREATYPE: tarkistaa vesialuetyypin, johon annettu vesimuodostuma kuuluu ja palauttaa sen lyhenteen
areaType <- function(area) {
	temp = match(area, areaList$lyhenne)
	return (areaList$tyyppi[temp])
}

# AREATYPESTATION: tarkistaa vesialuetyypin, johon annettu havaintoasema kuuluu ja palauttaa sen lyhenteen
areaTypeStation <- function(x) {
	# tehdään asemista helpommin luettava lista
	areasLong = melt(areas)
	areasLong$value <- as.character(areasLong$value)
	
	# mihin vesistömuodostumaan asema kuuluu?
	myArea <- convert(x, areasLong)

	return(areaType(myArea))
}

# REMOVEDUPLICATES: siivoaa datan niin että kullekin päivälle on vain yksi havainto
# dataTable = taulukko, jossa oleelliset muuttujat ja päivämäärät; palauttaa samanmuotoisen taulukon
removeDuplicates <- function(dataTable) {

	# tästä asemat vektorina
	stats = 0
	stats <- unique(dataTable$asema) 
	newTable = numeric()
	
	for (i in 1:length(stats)) {
	
		tempTable <- subset(dataTable, asema == stats[i])
		# tästä duplikaattipäivämäärät vektorina
		dups <- sort(unique(tempTable[duplicated(tempTable$pvm),]$pvm))
	
		# yhdistetään saman päivän eri havainnot keskiarvolla 
		for (j in 1:nrow(tempTable)) {
			# jos kyseessä on duplikaattipäivämäärä...
			if (tempTable$pvm[j] %in% dups) {
				# käydään läpi kaikki muuttujat
				for (k in 1:length(variables)) {
					# jos muuttuja on näkösyvyys, Ntot tai Ptot, poistetaan havainnot jotka ovat yli 1 m syvyydestä
					if (variables[k] == "secchi" | variables[k] == "ntot" | variables[k] == "ptot" ) {
						temp = mean(subset(tempTable, (pvm == tempTable$pvm[j] & syvyys <= 1))[[variables[k]]], na.rm = TRUE)
					} else temp = mean(subset(tempTable, (pvm == tempTable$pvm[j]))[[variables[k]]], na.rm = TRUE)
					if (!is.nan(temp)) tempTable[[variables[k]]][j] = temp
				}
			}
		}
	
		# poistetaan duplikaattipäivämäärälliset havainnot, vain ensimmäinen (nyt keskiarvo) jää
		tempTable <- tempTable[!duplicated(tempTable$pvm),]
		newTable <- rbind(newTable, tempTable)
	}
	
	return (newTable)
}

# SORTDATA: laittaa datan järjestykseen aseman ja päivämäärän mukaan 
# dataTable = taulukko, jossa oleelliset muuttujat ja päivämäärät; palauttaa samanmuotoisen taulukon
sortData <- function(dataTable) { 

	# taulukko järjestykseen
	sortTable <- dataTable[order(dataTable$asema, dataTable$pvm) , ]
	
	# poistetaan duplikaattipäivämäärät
	newTable <- removeDuplicates(sortTable)

	return (newTable)
}

# COUNTMEDIANS: laskee vuosittaiset mediaanit eri havaintoasemille
# dataTable = taulukko, jossa oleelliset muuttujat ja päivämäärät; palauttaa vektorin, jossa on pyydetyn aseman ja muuttujan vuosittaiset mediaanit aikajärjestyksessä
countMedians <- function(dataTable, station, variable) {

	stationMedians <- numeric()

	for (i in 1:length(years)) {
		stationMedians <- append(stationMedians, median(dataTable[[variable]][dataTable$vuosi == years[i] & dataTable$asema == station], na.rm = TRUE))
	}
	return (stationMedians)
}


# COUNTMEANOFMEDIANS: laskee vuosittaiset keskiarvot eri vesimuodostumille
# palauttaa vektorin
countMeanOfMedians <- function(dataTable, variable) {

	# jos dataa ei ole, palautetaan pelkkää NA:ta
	if (nrow(dataTable) == 0) {
		areaMeans = 1:length(years)
		areaMeans[1:length(years)] = NA
		return(areaMeans)
	}
	
	
	stats = 0
	stats <- unique(dataTable$asema) # tehdään asemista vektori

	# lasketaan staMedians-vektoriin havaintoasemien vuosittaiset mediaanit
	staMedians = numeric() 
	for (i in 1:length(stats)) {
		staMedians = cbind(staMedians, countMedians(dataTable, stats[i], variable))
	}

	# tehdään taulukosta nätimpi
	colnames(staMedians) <- stats
	rownames(staMedians) <- years

	# lasketaan vesimuodostumakohtaiset keskiarvot havaintoasemien mediaaneista
	areaMeans <- rowMeans(staMedians, na.rm = TRUE)

	return (areaMeans)
}

# MEANSTOTABLE: laskee ja niputtaa taulukkoon vuosittaiset keskiarvot kaikille muuttujille yhdestä taulukosta
# palauttaa taulukon
meansToTable <- function(dataTable) {

	meanTable <- numeric()

	for (i in 1:length(variables)) meanTable = cbind(meanTable, countMeanOfMedians(dataTable, variables[i]))

	meanTable = data.frame(years, meanTable)
	colnames(meanTable) <- c("vuosi", variables)

	return (meanTable)
}

# THROUGHTOMEANS: laskee alkuperäisdatasta vuosikeskiarvoihin asti
throughToMeans <- function(dataTable, area) {
	# halutun vesimuodostuman datat vain
	areaTable <- subset(dataTable, (asema %in% areas[[area]]))
	
	# jos dataa ei ole, palautetaan taulukollinen NA:ta
	if (nrow(areaTable) == 0) {
		secchi = 1:length(years)
		secchi[1:length(years)] = NA
		ntot = 1:length(years)
		ntot[1:length(years)] = NA
		ptot = 1:length(years)
		ptot[1:length(years)] = NA
		chla = 1:length(years)
		chla[1:length(years)] = NA
		vuosi = years
		meanTable = cbind(vuosi, secchi, ntot, ptot, chla)
		return (meanTable)	
	}
	
	# järjestykseen
	sortedTable = sortData(areaTable)
	
	# tässä välissä NA-check yksittäisille päiville
	for (i in 1:length(variables)) naCheck(sortedTable, area, variables[i])
	
	meanTable = meansToTable(sortedTable)
	
	return (meanTable)

}

# THROUGHTOMEANSSTAT: laskee alkuperäisdatasta vuosikeskiarvoihin asti havaintoasemalle
throughToMeansStat <- function(dataTable, stat) {
	# jos asemasta ei ole dataa, palautetaan taulukollinen NA:ta
	if (is.na(match(stat, dataTable$asema))) {
		secchi = 1:length(years)
		secchi[1:length(years)] = NA
		ntot = 1:length(years)
		ntot[1:length(years)] = NA
		ptot = 1:length(years)
		ptot[1:length(years)] = NA
		chla = 1:length(years)
		chla[1:length(years)] = NA
		vuosi = years
		meanTable = cbind(vuosi, secchi, ntot, ptot, chla)
		return (meanTable)
	}

	# halutun vesimuodostuman datat vain
	areaTable <- subset(dataTable, (asema == stat))
	# järjestykseen
	sortedTable = sortData(areaTable)
	
	meanTable = meansToTable(sortedTable)
	
	return (meanTable)

}

# CYCLE4: laskee neljän vuoden keskiarvon keskiarvoille
# meanTable = taulukko, jossa on vuosikeskiarvot kaikille muuttujille; palauttaa vektorin, jossa on yhden nelivuotissyklin keskiarvot kaikille muuttujille
cycle4 <- function(meanTable, startYear) {

	means <- subset(meanTable, (vuosi >= startYear & vuosi < (startYear+4)))
	means$vuosi <- NULL
	means = colMeans(means, na.rm = TRUE)
	return (means)
}

# CYCLE4ALL: laskee neljän vuoden keskiarvot koko aineiston yli
# meanTable = taulukko, jossa on vuosikeskiarvot kaikille muuttujille; palauttaa taulukon jossa rivit ovat nelivuotiskausia ja sarakkeet suureita
cycle4All <- function(meanTable) {

	cycle4Table = cycle4(meanTable, years[1])

	for (i in 2:length(periods)) { # pitää lopettaa ajoissa ettei nelivuotissyklit putoa laidan yli
		cycle4Table = data.frame(cycle4Table, cycle4(meanTable, years[i]))
	}

	cycle4Table <- as.data.frame(t(cycle4Table)) 

	rownames(cycle4Table) <- NULL
	cycle4Table = data.frame(periods, cycle4Table)
	return (cycle4Table)
}

