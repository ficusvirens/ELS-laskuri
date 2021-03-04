# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

library("ggplot2")

myBreaks <- function(myMin, myMax) {
	breaks = myMin
	i = myMin+2
	while(i <= myMax) {
		breaks = c(breaks, i)
		i = i + 2
	}
	
	return (breaks)
}

# DRAWHEATMAP: piirtää heatmapin datan olemassaolosta
drawHeatmap <- function(x, variable) {

	filename = paste("Output/Heatmap/Heatmap ", variable, ".png", sep = "")
	myTitle = paste("Puuttuva data", variable)
	
	# muokataan data heatmapille sopivaan muotoon
	dat3 <- melt(x, id.var = "Vesimuodostuma")
	colnames(dat3) <- c("Vesimuodostuma", "Vuosi", "Onko_dataa")
	dat3$Vuosi = as.numeric(as.character(dat3$Vuosi))

	
	# tässä määritellään heatmapin muotoilut
	ggplot(dat3, aes(Vuosi, Vesimuodostuma)) + 
	  geom_tile(aes(fill = Onko_dataa), colour = "grey") +
	  ggtitle(myTitle) +
	  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "red")) +
	  coord_fixed(ratio = 3)
	
	# tallennetaan
	ggsave(filename, width = 8, height = 3, dpi = 120)

}

# WRITEHEATTABLE: kirjoittaa tekstitiedostoon heatmapiin tarvittavan datan muuttujakohtaisesti
writeHeatTable <- function(variable) {
	
	myAreas = character()
	
	# kasviplanktonin kokonaisbiomassadataa ei käytetä sisäsaaristotyypeille, koska vertailuarvoja ei ole; poimitaan siis vain ulkosaariston vesimuodostumat
	if (variable == "biomassa") {
		for (i in 1:length(names(areas))) {
			if (!innerCheck(names(areas)[i])) myAreas = c(myAreas, names(areas[i]))
		}
	} else myAreas = names(areas)

	heatTable <- matrix(ncol = length(years), nrow = length(myAreas))
	rownames(heatTable) <- myAreas
	colnames(heatTable) <- years

	for (j in 1:length(myAreas)) {
		if (variable %in% names(NAyears[[myAreas[j]]])) {
			for (i in 1:length(years)) {
				if (is.na(match(years[i], as.numeric(NAyears[[myAreas[j]]][[variable]])))) {
					heatTable[myAreas[j], as.character(years[i])] = TRUE
				} else heatTable[myAreas[j], as.character(years[i])] = FALSE
			}
		}
	}
	
	write.table(heatTable, file = paste("Output/Heatmap/Heatmapdata ", variable, ".txt", sep = ""), sep = "\t", col.names = NA)
}
