# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# WRITENA: kirjoittaa tiedostoon nimeltä "Dataa puuttuu", miltä vuosilta dataa puuttuu
writeNA <- function(NAlist, area) {
	filename = paste("Output/Dataa puuttuu/Dataa puuttuu ", area, ".txt", sep = "")

	temp = "* = vain osa havainnoista puuttuu \n"

	# jos tämänniminen tiedosto on jo, se poistetaan ja aloitetaan tyhjästä
	if (file.exists(filename)) file.remove(filename)
	
	cat(temp, "\n", file = filename, append = TRUE)
	z <- deparse(substitute(NAlist[[area]]))
    nams = names(NAlist[[area]]) 
    
	for (i in seq_along(NAlist[[area]])){ 
		cat(nams[i], "\t",  NAlist[[area]][[i]], "\n", file = filename, append = TRUE) 
	}
}


# WRITEALL: kirjoittaa tiedostoon neljän vuoden keskiarvot, ELS-arvot muuttujille ja ELS-luokat sekä huomautukset puuttuvasta datasta
# area = vesimuodostuman lyhenne, ELS_Class = ELS-raja-arvotaulukko, dataTable = taulukko, jossa on neljän vuoden keskiarvot kaikille muuttujille
writeAll <- function(dataTable, area, ELS_Class) {
	filename = paste("Output/4 v keskiarvot/4 v keskiarvot ", area, ".txt", sep = "")
	write.table(dataTable, file = filename, sep = "\t", quote = FALSE, row.names = FALSE)
	
	temp1 = ELS_All(dataTable, ELS_Class)
	
	filename2 = paste("Output/ELS/ELS-arvot ", area, ".txt", sep = "")	
	write.table(temp1, file = filename2, sep = "\t", quote = FALSE, row.names = FALSE)
	
	temp2 = fysChem(temp1)
	temp3 = biol(temp1)
	temp4 = cbind(temp2[c("period", "fyskem")], temp3$bio)
	colnames(temp4) <- c("period", "fyskem", "bio")
	fc_bio[[area]] <<- temp4
	fc_bio[[area]]$period <<- NULL
	
	# ELS-keskiarvo
	ELS_final = rowMeans(cbind(temp2$fyskem, temp3$bio), na.rm = TRUE)
	results[[area]] <<- ELS_final
	
	temp5 = cbind(temp2["period"], ELS_final)
	filename3 = paste("Output/ELS/ELS-keskiarvo ", area, ".txt", sep = "")
	write.table(temp5, file = filename3, sep = "\t", quote = FALSE, row.names = FALSE)
	
	filename4 = paste("Output/ELS/ELS-keskiarvo sanallinen ", area, ".txt", sep = "")
	temp6 = cbind(temp2["period"], "ELS" = sapply(temp5$ELS_final, FUN = ELS_ToWords))
	write.table(temp6, file = filename4, sep = "\t", quote = FALSE, row.names = FALSE)
	
	filename5 = paste("Output/ELS/ELS-arvot bio-fyskem ", area, ".txt", sep = "")
	write.table(temp4, file = filename5, sep = "\t", quote = FALSE, row.names = FALSE)
	
	filename6 = paste("Output/ELS/ELS-luokat ", area, ".txt", sep = "")
	write.table(ELS_AllWords(temp4), file = filename6, sep = "\t", quote = FALSE, row.names = FALSE)
	
	writeNA(NAyears, area)

}

# TOELS_FINAL: ymppää taulukosta ELS-keskiarvot yhteen vektoriin
toELS_final <- function(dataTable, ELS_Class) {
	temp1 = ELS_All(dataTable, ELS_Class)
	temp2 = fysChem(temp1)
	temp3 = biol(temp1)
	temp4 = cbind(temp2[c("period", "fyskem")], temp3$bio)
	colnames(temp4) <- c("period", "fyskem", "bio")
	
	# ELS-keskiarvo
	ELS_final = rowMeans(cbind(temp2$fyskem, temp3$bio), na.rm = TRUE)

	return (ELS_final)

}
