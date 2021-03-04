# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

library("tmap")

# FIXMAP: muokkaa kartan drawMap-funktiolle sopivaan muotoon
# map =  shapefile-objekti, jossa vesistömuodostumien nimet ovat otsakkeen "Nimi" alla
fixMap <- function(map) {
	# muutetaan vesistömuodostumien nimet character-muotoon
	map@data$Nimi <- as.character(map@data$Nimi)

	# järjestetään aakkosjärjestykseen
	map <- map[order(map@data$Nimi),]
	
	return (map)
}

# FIXMAPDATA: muokkaa karttadatan drawMap-funktiolle sopivaan muotoon
# mapData = taulukko, jossa vesistömuodostumat ovat nimellä "alue" ja ELS-arvo nimellä "ELS"
fixMapData <- function(mapData) {
	
	# muutetaan vesistömuodostumien nimet character-muotoon
	mapData$alue <- as.character(mapData$alue)
	
	# järjestetään aakkosjärjestykseen
	mapData <- mapData[order(mapData$alue),]

	# muutetaan ELS-arvot ELS-luokiksi
	mapData$ELS <- sapply(mapData$ELS, FUN = ELS_ToWords)
	
	# muutetaan ELS-luokat faktoreiksi
	levs = c("Erinomainen", "Hyva", "Tyydyttava", "Valttava", "Huono")
	mapData$ELS <- factor(mapData$ELS, levels = levs, ordered = TRUE)
	
	return (mapData)
}

# DRAWMAP: piirtää kartan, jossa vesistömuodostumat on värikoodattu ELS-luokan mukaan
# map = shapefile-objekti, jossa vesistömuodostumien nimet ovat otsakkeen "Nimi" alla
# mapData = taulukko, jossa vesistömuodostumat ovat nimellä "alue" ja ELS-luokka nimellä "ELS"
drawMap <- function(mapData, map, period) {

	# tehdään uusi kartta
	newMap <- append_data(map, mapData, key.shp = "Nimi", key.data = "alue")
	
	mapTitle = as.character(period)

	# tässä asetetaan kartan muotoilut. 
	finalMap <- tm_shape(newMap) +
	tm_layout(title = mapTitle) +
	tm_fill("ELS", title = "ELS", style = "cat", palette = "-RdYlGn", colorNA = "grey") +
	tm_borders(alpha = .5)+
	tm_text("alue", size = 0.8, col = "#000000", bg.color = "#FFFFFF")
	
	# tallennetaan kartta jpg-muotoon
	filename = paste("Output/ELS-kartat/ELS-kartta ", period, ".jpg", sep = "")
	save_tmap(finalMap, file = filename)
}

