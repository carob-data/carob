
process_cip_lbvars <- function(filename, addvars=NULL) {      

	sheets <- readxl::excel_sheets(filename)
	minimal <- carobiner::read.excel(filename, sheet="Minimal")
	installation <- carobiner::read.excel(filename, sheet="Installation")

	if ("Fieldbook" %in% sheets) {
		r <- carobiner::read.excel(filename, sheet="Fieldbook")		  
		n <- as.list(installation$Value)
	} else if ("F4_Harvest_Mother" %in% sheets) {
		r <- carobiner::read.excel(filename, sheet="F4_Harvest_Mother")
		n <- as.list(installation$Mother)
	} else if ("F4_harvest_mother" %in% sheets) {
		r <- carobiner::read.excel(filename, sheet="F4_harvest_mother")
		n <- as.list(installation$Mother)
	} else if ("F4_ Harvest_Mother" %in% sheets) {
		r <- carobiner::read.excel(filename, sheet="F4_ Harvest_Mother")
		n <- as.list(installation$Mother)
	} else {
		stop("not clear which sheet should be used")
	}
	m <- as.list(minimal$Value)
	names(m) <- gsub(" ", "_", minimal$Factor)	
	names(n) <- gsub(" ", "_", installation$Factor)
	
	for (v in addvars) if (is.null(r[[v]])) r[v] <- NA

	plot_size <- as.numeric(n$`Plot_size_(m2)`)
	plot_adj <- 10000 / plot_size
     
  	if (grepl("/", m$Begin_date)) {
		i <- is.na(m$Begin_date)
		m$Begin_date <- as.character(as.Date(m$Begin_date,format = "%d/%m/%Y"))
		j <- is.na(m$Begin_date)
		if (sum(j) > sum(i)) warning("NAs introduced in Begin_date")
  	}
	if (grepl("/", m$End_date)) {
		if (grepl("-", m$End_date)) {
			# taking the last one, should perhaps take the average (doi_10.21223_KNC22E.R)
			m$End_date <- strsplit(m$End_date, "-")[[1]][2]
		}
		i <- is.na(m$End_date)
		m$End_date <- as.character(as.Date(m$End_date,format = "%d/%m/%Y"))
		j <- is.na(m$End_date)
		if (sum(j) > sum(i)) warning("NAs introduced in End_date")
	} 
	d <- data.frame(
		rep = as.integer(r$REP),
		variety = r$INSTN,
		yield = as.numeric(r$TTWP) * plot_adj,
		yield_marketable = as.numeric(r$MTWP) * plot_adj,
		country = m$Country,
		adm1 = m$Admin1,
		adm2 = m$Admin2,
		adm3 = m$Admin3,
		location = m$Locality,
		longitude = as.numeric(m$Longitude),
		latitude = as.numeric(m$Latitude),
		geo_from_source = TRUE,
		elevation = as.numeric(m$Elevation),
		planting_date = m$Begin_date,
		harvest_date = m$End_date,
		plant_density = as.numeric(n$`Planting_density_(plants/Ha)`),
		row_spacing = as.numeric(n$`Distance_between_rows_(m)`) * 100,
		plant_spacing = as.numeric(n$`Distance_between_plants_(m)`) * 100,
		trial_id = gsub(".xls|.xlsx", "", basename(filename))
	)
	
	if (!is.null(r$AUDPC)) {
		d$AUDPC <- as.numeric(r$AUDPC) / 100
	    d$pathogen <- "Phytophthora infestans"
	}
	if (!is.null(r$AUDPC)) {
		d$rAUDPC <- as.numeric(r$rAUDPC)
	    d$pathogen <- "Phytophthora infestans"
	}
	if (!is.null(r$plot)) {
		d$plot_id = as.character(r$plot)
	} else {
		d$plot_id = as.character(NA)	
	}
	if (!is.null(r$block)) {
		d$block_id = as.character(r$block)
	} else {
		d$block_id = as.character(NA)
	}

	if ("Soil_analysis" %in% sheets) {
		soil <- carobiner::read.excel(filename, sheet="Soil_analysis")
		names(soil) <- gsub(" ", "_", tolower(names(soil)))
		d$soil_pH <- soil$soil_ph
		d$soil_SOM <- soil$organic_matter
	}
	
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$crop <- "potato"
    d$yield_part <- "tubers"
    d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d
}


