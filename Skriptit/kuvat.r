# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# LINEGRAPH: piirtää annetusta tulostaulukosta viivadiagrammin, jossa näkyy ELS:n muutos kussakin vesimuodostumassa
lineGraph <- function(dat, years, filename, graphTitle) {

	# tästä vuodet kuvaajan x-akselille
	myEnd = length(years)-2
	graphYears = years[2:myEnd]

	dat$vuosi <- graphYears
	dat$vuosi <- as.character(dat$vuosi)

	# käppyröiden nimet ihan kokopitkiksi vesimuodostumien nimiksi
	titles = sapply(colnames(dat), FUN = function(x2) convert(x2, areaList))
	colnames(dat) <- titles

	# muokataan data sopivaan muotoon
	dat2 = melt(dat)
	dat2$vuosi <- as.numeric(dat2$vuosi)
	colnames(dat2) <- c("Vuosi", "Vesimuodostuma", "ELS")

	# tässä on graafin muotoilut
	ggplot(data = dat2, aes(x = Vuosi, y = ELS, group = Vesimuodostuma, colour = Vesimuodostuma))+
	  geom_line(size = 1)+
	  expand_limits(y = c(0, 1))+
	  ggtitle(graphTitle)+
	  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), minor_breaks = NULL)+
	  theme(legend.title = element_blank())+
	  coord_fixed(ratio = 30)

	# tallennetaan
	ggsave(filename, width = 10, height = 6, dpi = 120)
	
}


# CHANGEGRAPH: piirtää annetusta tulostaulukosta viivadiagrammin, jossa näkyy ELS:n muutos kussakin vesimuodostumassa
changeGraph <- function(dat, years, filename, myTitle) {

	myEnd = length(years)-2
	graphYears = years[2:myEnd]

	dat$vuosi <- graphYears
	dat$vuosi <- as.character(dat$vuosi)


	titles = sapply(colnames(dat), FUN = function(x2) convert(x2, areaList))
	colnames(dat) <- titles

	dat2 = melt(dat)
	dat2$vuosi <- as.numeric(dat2$vuosi)
	colnames(dat2) <- c("Vuosi", "Parametri", "ELS")

	ggplot(data = dat2, aes(x = Vuosi, y = ELS, group = Parametri, colour = Parametri))+
	  geom_line(size = 1)+
	  expand_limits(y = c(-0.4, 0.4))+
	  ggtitle(myTitle)+
	  scale_y_continuous(breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), minor_breaks = NULL)+
	  coord_fixed(ratio = 38)
	 
	ggsave(filename, width = 10, height = 6, dpi = 120)
}