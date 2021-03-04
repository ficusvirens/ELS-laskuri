# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

#Intensiivi-asemat vuodesta 2014 eteenpäin: 4 39 57 114 117 123 4 148 149 168
#Lähivedet-asemat vuodesta 2014 eteenpäin: 87, 94, 68, 18, 20, 110, 111, 113, 181, 166
#Töölönlahti-asemat: 188, 191, 179
#Asemien luokittelu tehty vesipuitedirektiivin vesimuodostumaluokittelun mukaan
#Luokittelussa käytetään kaikkia saatavilla olevia asemia - Lisää Lahtiohjelman asemat

#Määritysrajat - Muuttuja, yksikkö, vuosi jolloin tullut voimaan, määritysrajan alapuolella olevat arvot korvattu luvulla 0

#Asemat
 #Kruunuvuorenselän asemat: 4, 18, 188 (188, lisätty 25.2.2015)
 #Seurasaaren asemat: 94, 87, 191 (191, lisätty 25.2.2015)
 #Sipoonselän asemat: 111, 113, 181
 #Suvisaariston ja Lauttasaaren asemat: 68, 117
 #Porvoo-Helsinki asemat: 39, 114, 166
 #Helsinki-Porkkala asemat: 125, 168, 149, 57, 147, 148, 123
 #Villingin asemat: 25, 110

 
{
#setwd("Y:/YSO/VESI/VESISTÖ/YHTEISET/Laatuluokitus/HKI_luokittelu") # asetetaan työskentelykansio
#setwd("Y:/YSO/VESI/Henkilöt/Airola/R") # asetetaan työskentelykansio

library("reshape")


sink("Output/Output.txt") # tänne tungetaan kaikki viestit koodin etenemisestä

source("Skriptit/globaalit_muuttujat.r")
source("Skriptit/NA-check.r") 
source("Skriptit/funktioita.r") 
source("Skriptit/biomassa.r")
source("Skriptit/ELS.r")
source("Skriptit/kartta.r") 
source("Skriptit/uloskirjoitus.r") 
source("Skriptit/BBI.r")
source("Skriptit/heatmap.r")
source("Skriptit/kuvat.r")

source("Skriptit/herkkyysanalyysit.r")


# pyydetään käyttäjältä aloitus- ja lopetusvuosi
startYear <- readline(prompt = "Anna aloitusvuosi: ")
startYear <- as.numeric(startYear)
endYear <- readline(prompt = "Anna lopetusvuosi (yhteensä vähintään viiden vuoden pätkä): ")
endYear <- as.numeric(endYear)

# luodaan globaali muuttuja "years", joka on vektori halutuista vuosista aikajärjestyksessä	
assign("years", startYear:endYear, envir = .GlobalEnv)

# luodaan globaali muuttuja "periods", joka on vektori halutuista nelivuotissykleistä aikajärjestyksessä
sYear <- years[1]
eYear <- years[1]+3
period <- paste(toString(sYear), "-", toString(eYear), sep = "")

for (i in 2:(length(years)-3)) { # pitää lopettaa ajoissa ettei nelivuotissyklit putoa laidan yli
	sYear <- years[i]
	eYear <- years[i]+3
	period = c(period, paste(toString(sYear), "-", toString(eYear), sep = ""))
}

assign("periods", period, envir = .GlobalEnv)


# ------->>>    SECCHI, NTOT, PTOT, CHLA    <<<-------

mydata <- read.table("Input/fysikaaliskemiallisetmuuttujat.txt", header = TRUE) # luetaan data taulukkoon
mydata$pvm <- as.Date(mydata$pvm) # asetetaan päivämäärät date-muotoisiksi

# jollei päivämääriä ole kirjoitettu auki, tehdään se
if(!("vuosi" %in% colnames(mydata))) mydata$vuosi <- as.numeric(format(mydata$pvm, format = "%Y"))
if(!("kk" %in% colnames(mydata))) mydata$kk <- as.numeric(format(mydata$pvm, format = "%m"))
if(!("paiva" %in% colnames(mydata))) mydata$paiva <- as.numeric(format(mydata$pvm, format = "%d"))

# valikoidaan tarvittavat muuttujat ja kesäkausi sekä halutut vuodet
mydata.l <- subset(mydata, kk > 6 & (kk < 9 | (kk == 9 & paiva < 15)) & syvyys < 6 & (vuosi >= startYear & vuosi <= endYear), select = c(
 "asema", 
 "pvm",
 "paiva",
 "kk",
 "vuosi",
 "syvyys",
 "secchi",
 "ntot",
 "ptot",
 "chla")) 
 
 
# ------->>>    KASVIPLANKTONIN KOKONAISBIOMASSA    <<<-------

bmData <- read.table("Input/kasviplankton_biomassa.txt", header = TRUE, sep = "\t", fill = TRUE) # luetaan biomassadata taulukkoon
bmData$pvm <- as.Date(bmData$pvm) # asetetaan päivämäärät date-muotoisiksi

# kirjoitetaan päivämäärät auki omiin sarakkeisiinsa
bmData$vuosi <- as.numeric(format(bmData$pvm, format = "%Y"))
bmData$kk <- as.numeric(format(bmData$pvm, format = "%m"))
bmData$paiva <- as.numeric(format(bmData$pvm, format = "%d"))

# valikoidaan tarvittavat muuttujat ja kesäkausi sekä halutut vuodet
bmData.l <- subset(bmData, kk > 6 & (kk < 9 | (kk == 9 & paiva < 15)) & (vuosi >= startYear & vuosi <= endYear), select = c(
 "asema", 
 "pvm",
 "paiva",
 "kk",
 "vuosi",
 "biomassa")) 

# järjestetään mittausaseman ja päiväyksen mukaan varmuuden vuoksi
bmData.l.s <- bmData.l[order(bmData.l$asema, bmData.l$pvm) , ]

# tähän tulee taulukko jossa on kokonaisbiomassat
totBio <- countTotMass(bmData.l.s)


# ------->>>    BBI    <<<-------

# luetaan pohjisdata taulukkoon
benthosTable <- read.table(file = "Input/pohjaelaimet.txt", header = TRUE, sep = "\t", , strip.white = TRUE, stringsAsFactors = TRUE, quote = "")
benthosTable$pvm <- as.Date(benthosTable$pvm) 
benthosTable$asema <- as.character(benthosTable$asema)
benthosTable$nimi <- as.character(benthosTable$nimi)
benthosTable$vuosi <- as.numeric(format(benthosTable$pvm, format = "%Y"))

# valikoidaan tarvittavat muuttujat sekä halutut vuodet
benthosTable.l <- subset(benthosTable, vuosi >= startYear & vuosi <= endYear, select = c(
 "asema", 
 "pvm",
 "vuosi", 
 "nimi", 
 "lukumäärä"))

# luetaan herkkyysarvot taulukkoon
sensitivityTable <- read.table(file = "Kayttotiedostot/herkkyysluokitukset.txt", header = TRUE, sep = "\t", stringsAsFactors = TRUE, quote = "")
sensitivityTable$taksoni <- as.character(sensitivityTable$taksoni)

# luetaan pohjisten vastaavuudet taulukkoon
taxonTable <- read.table(file = "Kayttotiedostot/pohjaelaimet_vastaavuuksia.txt", header = TRUE, sep = "\t", stringsAsFactors = TRUE, quote = "")
taxonTable$laji <- as.character(taxonTable$laji)
taxonTable$vastaavuus <- as.character(taxonTable$vastaavuus)

# luetaan BBI:n laskentaan tarvittavat vakiot taulukkoon
BBI_Inner <- read.table("Kayttotiedostot/BBI-vakiot_sisa.txt", header = TRUE, row.names = 1) 
BBI_Outer <- read.table("Kayttotiedostot/BBI-vakiot_ulko.txt", header = TRUE, row.names = 1) 

# lasketaan BBI
BBI_table = BBI_all(benthosTable.l, sensitivityTable, taxonTable, stationData, BBI_Inner, BBI_Outer)
BBI_table$vuosi <- as.numeric(format(BBI_table$pvm, format = "%Y"))


# ------->>>    ELS VESIMUODOSTUMITTAIN   <<<-------

# mitä vesialuetyyppejä aineistossa on?
types <- unique(areaList$tyyppi)

# luetaan ELS-raja-arvot taulukkoon
ELS_Classes <- list()
for (i in 1:length(types)) {
	tempTitle = paste("Kayttotiedostot/ELS-rajat_", types[i], ".txt", sep = "")
	ELS_Classes[[types[i]]] <- read.table(tempTitle, header = TRUE, row.names = 1) 
}
 
# lasketaan kustakin vesimuodostumasta neljän vuoden keskiarvot, ELS-arvot ja ELS-luokat ja kirjoitetaan ne tiedostoihin
for (i in 1:length(areas)) {
	name = names(areas)[i]

	type = areaType(name)

	ELS_table = ELS_Classes[[type]]
	
	# secchi, ntot, ptot, chla
	variables = c("secchi", "ntot", "ptot", "chla")
	temp = throughToMeans(mydata.l, name)
	
	# biomassa 
	if (!innerCheck(name)) {
		variables = c(variables, "biomassa")
		totBioArea = subset(totBio, (asema %in% areas[[name]]))
		temp2 <- countMeanOfMedians(totBioArea, "biomassa")
	}
	
	# BBI 
	variables = c(variables, "BBI")
	BBI_area = subset(BBI_table, (asema %in% areas[[name]]))
	temp3 <- countMeanOfMedians(BBI_area, "BBI")
	
	# yhdistetään
	if (!innerCheck(name)) {
		meanTable = data.frame(temp, temp2, temp3)
	} else meanTable = data.frame(temp, temp3) 
	

	colnames(meanTable) <- c("vuosi", variables)
	
	# otsakkeet NA-vuosilistaan
	names(NAyears[[name]]) <- variables[1:4]
	
	# tässä välissä NA-check kokonaisille vuosille
	for (i in 1:length(variables)) naFullYears(meanTable, name, variables[i])
	
	cycledTable = cycle4All(meanTable)
	writeAll(cycledTable, name, ELS_table)
	
	cTables[[name]] <- cycledTable
}

# ------->>>    ELS HAVAINTOASEMITTAIN   <<<-------

#for (i in 1:length(stations)) {
#	stat = stations[i]
#	type = areaTypeStation(stat)

#	ELS_table = ELS_Classes[[type]]
	
	# secchi, ntot, ptot, chla
#	variables = c("secchi", "ntot", "ptot", "chla")
#	temp = throughToMeansStat(mydata.l, stat)
	
	# biomassa 
#	if (!innerCheckStation(stat)) {
#		variables = c(variables, "biomassa")
#		temp2 <- countMedians(totBio, stat, "biomassa")
#		names(temp2) <- years
#	}
	
	# BBI 
#	variables = c(variables, "BBI")
#	temp3 <- countMedians(BBI_table, stat, "BBI")
#	names(temp3) <- years
	
	# yhdistetään
#	if (!innerCheckStation(stat)) {
#		meanTable = data.frame(temp, temp2, temp3)
#	} else meanTable = data.frame(temp, temp3) 
	
#	colnames(meanTable) <- c("vuosi", variables)
	
#	cycledTable = cycle4All(meanTable)
#	writeAll(cycledTable, stat, ELS_table)

#}

# ------->>>    HERKKYYSANALYYSIT VESIMUODOSTUMITTAIN  <<<-------

variablesD = c("secchi", "ntot", "ptot", "chla", "biomassa", "BBI", "chla & biomassa", "ntot & ptot")

# tähän laitetaan tulokset, jotka on laskettu datasta josta puuttuu yksi muuttuja
droppedResults <- rep(list(data.frame()), length(areas))
names(droppedResults) <- names(areas)

# tässä lasketaan eri vesimuodostumien ELS-arvot ilman kutakin muuttujaa ja piirretään niistä graafi
for (j in 1:length(areas)) {
	temp3 = as.character(periods)
	name = names(areas)[j]

	for (i in 1:length(variablesD)) {
		# biomassan laskenta jätetään sisäsaaristoalueilta pois, muuten lasketaan kaikki
		if (!(innerCheck(name) & (variablesD[i] == "biomassa" | variablesD[i] == "chla & biomassa"))) {
			type = areaType(name)
			ELS_table = ELS_Classes[[type]]
			
			if (i < 7) { # lasketaan ensin yksittäiset muuttujat, sitten vasta parit
				# tässä poistetaan yhden muuttujan data
				temp = removeVariable(cTables[[name]], variablesD[i])
			} else if (variablesD[i] == "ntot & ptot") {
				# tässä poistetaan kahden muuttujan data
				temp = removeVariable(cTables[[name]], "ntot")
				temp = removeVariable(temp, "ptot")
			} else if (variablesD[i] == "chla & biomassa") {
				# tässä poistetaan kahden muuttujan data
				temp = removeVariable(cTables[[name]], "chla")
				temp = removeVariable(temp, "biomassa")					
			}
			# ja sitten lasketaan ELS-keskiarvo
			temp2 = toELS_final(temp, ELS_table)
			temp3 = data.frame(temp3, temp2)
		}
	}
	
	# sisäsaaristossa muuttujia on vähemmän
	if (innerCheck(name)) {
		outerVariables = c("biomassa", "chla & biomassa")
		colnames(temp3) <- c("period", variablesD[!variablesD %in% outerVariables])
	} else colnames(temp3) <- c("period", variablesD)
	droppedResults[[name]] <- temp3
	
	# kirjoitetaan herkkyysanalyysitulokset tiedostoon
	filename = paste("Output/Herkkyysanalyysit/Datoja puuttuu ", name, ".txt", sep = "") 
	write.table(droppedResults[[name]], file = filename, sep = "\t", quote = FALSE, row.names = FALSE)
	droppedResults[[name]]$period <- NULL
	
	filename2 = paste("Output/Herkkyysanalyysit/Datoja puuttuu ", name, ".png", sep = "")
	myTitle = paste("ELS-arvon muutos, kun dataa jätetään pois,", convert(name, areaList))

	# tässä on nyt muutos suhteessa kaikella datalla saatuun tulokseen
	change <- droppedResults[[name]] - results[[name]]
	
	# piirretään kuva ELS-arvon muutoksesta, kun jättää yhden muuttujan huomiotta
	changeGraph(change, years, filename2, myTitle)

}


# ------->>>    HERKKYYSANALYYSIT PARAMETREITTAIN  <<<-------

variables = c("secchi", "ntot", "ptot", "chla", "biomassa", "BBI")

# tähän laitetaan tulokset, jotka on laskettu datasta josta puuttuu yksi muuttuja
droppedResults <- rep(list(data.frame()), length(variables))
names(droppedResults) <- variables

# tässä lasketaan eri vesimuodostumien ELS-arvot ilman kutakin muuttujaa ja piirretään niistä graafi
for (i in 1:length(variables)) {
	temp3 = as.character(periods)
	for (j in 1:length(areas)) {
		name = names(areas)[j]
		
		# biomassan laskenta jätetään sisäsaaristoalueilta pois, muuten lasketaan kaikki
		if (!(innerCheck(name) & variables[i] == "biomassa")) {
			type = areaType(name)
			ELS_table = ELS_Classes[[type]]
			
			# tässä poistetaan yhden muuttujan data
			temp = removeVariable(cTables[[name]], variables[i])
			# ja sitten lasketaan vesimuodostumittain ELS-keskiarvo
			temp2 = toELS_final(temp, ELS_table)
			temp3 = data.frame(temp3, temp2)
		}
	}
	
	if (variables[i] == "biomassa") {
		# kerätään yhteen vektoriin ulko- ja välisaariston vesimuodostumat 
		outers = character()
		for (k in 1:length(areas)) {
			if (!innerCheck(names(areas[k]))) outers = c(outers, names(areas[k]))
		}
		colnames(temp3) <- c("period", outers)
	} else colnames(temp3) <- c("period", names(areas))
	droppedResults[[variables[i]]] <- temp3
	
	# kirjoitetaan herkkyysanalyysitulokset tiedostoon
	filename = paste("Output/Herkkyysanalyysit/Ilman ", variables[i], "-dataa.txt", sep = "") 
	write.table(droppedResults[[variables[i]]], file = filename, sep = "\t", quote = FALSE, row.names = FALSE)
	droppedResults[[variables[i]]]$period <- NULL
	
	filename2 = paste("Output/Herkkyysanalyysit/Ilman ", variables[i], "-dataa.png", sep = "")
	myTitle = paste("ELS-arvon muutos, kun", variables[i], "jätetään pois")

	# tässä on nyt muutos suhteessa kaikella datalla saatuun tulokseen
	if (variables[i] == "biomassa") {
		change <- droppedResults[[variables[i]]] - subset(data.frame(results), select = outers)
	} else change <- droppedResults[[variables[i]]] - data.frame(results)
	
	# piirretään kuva ELS-arvon muutoksesta, kun jättää yhden muuttujan huomiotta
	changeGraph(change, years, filename2, myTitle)

}


# ------->>>    BIO-FYSKEM    <<<------

for (i in 1:length(areas)) {
	area = names(areas)[i]
	filename = paste("Output/Herkkyysanalyysit/Bio-fyskem ", area, ".png", sep = "")
	gTitle = paste("ELS-arvon ajallinen muutos,", convert(area, areaList))
	lineGraph(fc_bio[[area]], years, filename, gTitle)
}

# ------->>>    HEATMAP    <<<------

variables = c("secchi", "ntot", "ptot", "chla", "biomassa", "BBI")

for (i in 1:length(variables)) { 
	# kirjoitetaan heatmapia varten data tiedostoon
	writeHeatTable(variables[i])

	# luetaan heatmapdata tiedostosta
	filename = paste("Output/Heatmap/Heatmapdata ", variables[i], ".txt", sep = "")
	dat <- read.table(filename, header = TRUE, sep = "\t", check.names = FALSE)
	colnames(dat) <- c("Vesimuodostuma", years)
	dat$Vesimuodostuma <- as.character(dat$Vesimuodostuma)
	for (j in 1:nrow(dat)) dat$Vesimuodostuma[j] = convert(dat$Vesimuodostuma[j], areaList)

	# piirretään heatmap
	drawHeatmap(dat, variables[i])
}


# ------->>>    KARTAT    <<<-------

# luetaan kartta sisään
helMap <- read_shape(file = "Kayttotiedostot/Vesimuodostumat_region_region.shp")
helMap <- fixMap(helMap)

# synkataan karttakuva ja karttaan tuleva data
mapData <- data.frame(results)
rownames(mapData) <- as.character(periods)
mapData <- t(mapData)

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

# piirretään ELS-arvojen ajallisesta muutoksesta graafi
dat = data.frame(results)
filename = "Output/ELS-plot.png"
gTitle = "ELS-arvon ajallinen muutos"
lineGraph(dat, years, filename, gTitle)


sink() # suljetaan Output.txt-tiedosto 

}