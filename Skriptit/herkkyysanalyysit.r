# ELS-laskuri
# Sofia Airola 2016
# sofia.airola@hespartto.fi

# REMOVEVARIABLE: muuttaa halutun muuttujan kaikki arvot NA:ksi
removeVariable <- function(dataTable, variable) {
	dataTable[[variable]] <- sapply(dataTable[[variable]], FUN = function(x2) x2 = NA)

	return(dataTable)
}