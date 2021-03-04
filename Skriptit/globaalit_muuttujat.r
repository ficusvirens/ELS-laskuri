# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# luodaan globaali muuttuja "areas", joka on lista vesimuodostumista ja kunkin havaintoasemista
stats <- as.list(read.table("Kayttotiedostot/asemat.txt", header = TRUE, sep = "\t", fill = TRUE))
stats <- lapply(stats, function(x)x[!is.na(x)])
stats <- lapply(stats, function(x)x[x!=""])
stats <- lapply(stats, function(x)as.character(x))
assign("areas", stats, envir = .GlobalEnv)

# luodaan globaali muuttuja "stations", joka on lista havaintoasemista
stationsTemp <- as.character(melt(areas)$value)
assign("stations", stationsTemp, envir = .GlobalEnv)

# luodaan globaali muuttuja "areaList", jossa on tietoja vesimuodostumista
arList <- read.table(file = "Kayttotiedostot/vesimuodostumat_lyhenteet.txt", header = TRUE, sep = "\t")
arList$lyhenne <- as.character(arList$lyhenne)
arList$alue <- as.character(arList$alue)
arList$tyyppi <- as.character(arList$tyyppi)
assign("areaList", arList, envir = .GlobalEnv)

# luodaan globaali muuttuja "NAyears", joka pitää kirjaa vuosista, joilta puuttuu dataa
tempList <- replicate(length(areaList$lyhenne), list())
names(tempList) <- areaList$lyhenne
assign("NAyears", tempList, envir = .GlobalEnv)

# luodaan globaali muuttuja "variables", joka on vektori kaikista muuttujista 
assign("variables", c("secchi", "ntot", "ptot", "chla", "biomassa"), envir = .GlobalEnv)

# luodaan globaali muuttuja "stationData", jossa on asemien syvyydet
# nämä tiedot tarvitaan pohjaeläinasemista!
statData <- read.table("Kayttotiedostot/asemat_tiedot.txt", header = TRUE)
statData$asema <- as.character(statData$asema)
assign("stationData", statData, envir = .GlobalEnv)

# luodaan globaali muuttuja "results", joka on listojen lista ELS-arvoista kaikissa vesistömuodostumissa ja vuosisykleissä
res <- rep(list(list()), length(names(areas)))
names(res) <- names(areas)
assign("results", res, envir = .GlobalEnv) 

# luodaan globaali muuttuja "cTables", joka on data frame -lista ELS-arvoista kaikissa vesistömuodostumissa ja vuosisykleissä ja toimii apuna herkkyysanalyysien laskennassa
cTab <- rep(list(data.frame()), length(names(areas)))
names(cTab) <- names(areas)
assign("cTables", cTab, envir = .GlobalEnv) 

# luodaan globaali muuttuja, listojen lista "fc_bio", johon talletetaan fysikaalis-kemialliset ja biologiset ELS-tulokset erikseen
fb <- rep(list(data.frame()), length(names(areas)))
names(fb) <- names(areas)
assign("fc_bio", fb, envir = .GlobalEnv) 
