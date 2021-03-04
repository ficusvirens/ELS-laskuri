# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

{
source("Skriptit/globaalit_muuttujat.r")
source("Skriptit/kartta.r")

# kysytään käyttäjältä painotus
bioPercents = readline(prompt = "Anna biologisen datan painotus prosenteissa (0-100): ")
bioPercents <- as.numeric(bioPercents)

for (i in 1:length(areas)) {
	area = names(areas)[i]

	# luetaan sisään bio-fyskem-taulukko, jonka malli on aiemmin tuottanut
	filename = paste("Output/ELS/ELS-arvot bio-fyskem ", area, ".txt", sep = "")
	myData = read.table(file = filename, header = TRUE)

	# lasketaan painotettu keskiarvo
	myData$painotettu_ka = ((bioPercents * myData$bio) + ((100-bioPercents) * myData$fyskem))/100

	finalData = data.frame(myData$period, myData$painotettu_ka)
	colnames(finalData) <- c("period", "painotettu_ka")
	filename2 = paste("Output/ELS/ELS-arvot painotetut ", area, ".txt", sep = "")
	write.table(finalData, file = filename2)
	
	results[[area]] <- finalData$painotettu_ka
}

# synkataan karttakuva ja karttaan tuleva data
mapData <- data.frame(results)
rownames(mapData) <- periods
mapData <- t(mapData)

areaList <- read.table(file = "Kayttotiedostot/vesimuodostumat_lyhenteet.txt", header = TRUE, sep = "\t")
areaList$lyhenne <- as.character(areaList$lyhenne)
areaList$alue <- as.character(areaList$alue)

mapAreas = character()
for (i in 1:nrow(mapData)) mapAreas = c(mapAreas, convert(rownames(mapData)[i], areaList))
mapData = data.frame(mapData, alue = mapAreas, check.names = FALSE) 

# piirretään oma kartta kustakin nelivuotissyklistä
for (i in 1:length(periods)) {
	selc = c(periods[i], "alue")
	tempMapData = subset(mapData, select = selc)
	colnames(tempMapData)[1] <- "ELS"
	tempMapData <- fixMapData(tempMapData)
	
	drawMap(tempMapData, helMap, periods[i])
}

# ------->>>    VIIVADIAGRAMMI    <<<-------

dat = data.frame(results)
filename = "Output/ELS-plot.png"
lineGraph(dat, years, filename)

}