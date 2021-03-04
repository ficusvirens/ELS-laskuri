# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi


source("Skriptit/globaalit_muuttujat.r")
source("Skriptit/kartta.r")
source("Skriptit/ELS.r")

for (i in 1:length(names(areas))) {
	area = names(areas)[i]

	# luetaan sisään tiedot
	filename = paste("Output/ELS/ELS-keskiarvo sanallinen ", area, ".txt", sep = "")
	myData = read.table(file = filename, header = TRUE)
	myData$ELS <- sapply(myData$ELS, FUN = ELS_ToNumbers)
	results[[area]] <- myData$ELS
}

# synkataan karttakuva ja karttaan tuleva data
periods <- myData$period

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
	tempMapData = subset(mapData, select = c(periods[i], alue))
	colnames(tempMapData)[1] <- "ELS"
	tempMapData <- fixMapData(tempMapData)
	
	drawMap(tempMapData, helMap, periods[i])
}

