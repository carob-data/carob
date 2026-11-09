
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
	names(m) <- tolower(gsub(" ", "_", trimws(minimal$Factor)))
	names(n) <- tolower(gsub(" ", "_", trimws(installation$Factor)))
	names(r) <- trimws(tolower(names(r)))
	
	if (!is.null(addvars)) {
		addvars <- tolower(addvars)
		for (v in addvars) if (is.null(r[[v]])) r[v] <- NA
	}
	
	plot_size <- as.numeric(n$`plot_size_(m2)`)
	plot_adj <- 10000 / plot_size
     
  	if (grepl("/", m$begin_date)) {
		i <- is.na(m$begin_date)
		m$begin_date <- as.character(as.Date(m$begin_date,format = "%d/%m/%Y"))
		j <- is.na(m$begin_date)
		if (sum(j) > sum(i)) warning("NAs introduced in begin_date")
  	}
	if (grepl("/", m$end_date)) {
		if (grepl("-", m$end_date)) {
			# taking the last one, should perhaps take the average (doi_10.21223_KNC22E.R)
			m$end_date <- strsplit(m$end_date, "-")[[1]][2]
		}
		i <- is.na(m$end_date)
		m$end_date <- as.character(as.Date(m$end_date,format = "%d/%m/%Y"))
		j <- is.na(m$end_date)
		if (sum(j) > sum(i)) warning("NAs introduced in End_date")
	}
	d <- data.frame(
		rep = as.integer(r$rep),
		variety = r$instn,
		yield = as.numeric(r$ttwp) * plot_adj,
		yield_marketable = as.numeric(r$mtwp) * plot_adj,
		#we record the number of plants planted and harvested instead
		#yield_adjusted = as.numeric(r$ttya) * plot_adj,
		country = m$country,
		adm1 = m$admin1,
		adm2 = m$admin2,
		adm3 = m$admin3,
		location = m$locality,
		longitude = as.numeric(m$longitude),
		latitude = as.numeric(m$latitude),
		geo_from_source = TRUE,
		elevation = as.numeric(m$elevation),
		planting_date = m$begin_date,
		harvest_date = m$end_date,
		seed_density = as.numeric(n$`planting_density_(plants/ha)`),
		row_spacing = as.numeric(n$`distance_between_rows_(m)`) * 100,
		plant_spacing = as.numeric(n$`distance_between_plants_(m)`) * 100,
		trial_id = trimws(gsub(".xls|.xlsx", "", basename(filename)))
	)
	if (!is.null(r$npe)) {
		# number of plants emerged
		d$plant_density <- as.numeric(r$npe) * plot_adj
	} else if (!is.null(r$pph)) {
		# PPH (CO_330:0000290) is the proportion of plants harvested,
		# in percent (NPH * 100 / NTP)
		d$plant_density <- as.numeric(n$`planting_density_(plants/ha)`) * as.numeric(r$pph) / 100
	}
	if (!is.null(r$tntp)) {
		d$tuber_density <- as.numeric(r$tntp) * plot_adj	
	}

	if (!is.null(r$avdm)) {
		avdm <- r$avdm
		# "-" is used for missing values in some fieldbooks
		avdm[avdm %in% c("-", "")] <- NA
		d$yield_moisture <- 100 - as.numeric(avdm)
	} else {
		d$yield_moisture <- NA
	}
	
	if (!is.null(r$audpc)) {
		d$AUDPC <- as.numeric(r$audpc) / 100
	    d$pathogen <- "Phytophthora infestans"
	}
	if (!is.null(r$raudpc)) {
		d$rAUDPC <- as.numeric(r$raudpc)
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

	i <- match("soil_analysis", tolower(sheets))
	if (!is.na(i)) {
		soil <- carobiner::read.excel(filename, sheet=i)
		names(soil) <- gsub(" ", "_", trimws(tolower(names(soil))))
		d$soil_pH <- soil$soil_ph
		d$soil_SOM <- soil$organic_matter
	}
	

	fm <- list.files(dirname(filename), pattern="material_list", ignore.case=TRUE, full.names=TRUE)
	if (length(fm) > 1) {
		stop("multiple material files")
	} else if (length(fm) == 1) {
		if (grepl("\\.xlsx?$", fm, ignore.case=TRUE)) {
			m <- carobiner::read.excel(fm, na="")
			m[] <- lapply(m, function(x) trimws(as.character(x)))
			# "Female code" -> "Female.code" as with read.delim
			names(m) <- make.names(names(m))
		} else {
			m <- utils::read.delim(fm, colClasses="character", na.strings="", strip.white=TRUE)
		}
		names(m) <- tolower(names(m))
		fem <- ifelse(is.na(m$female.code), m$female.institutional.number, m$female.code)
		mal <- ifelse(is.na(m$male.code), m$male.institutional.number, m$male.code)
		i <- match(d$variety, m$institutional.number)
		if (!(all(is.na(i)))) {
			j <- !is.na(i)
			k <- i[j]
			d$female_parent[j] <- fem[k]
			d$male_parent[j] <- mal[k]
			d$variety_pedigree[j] <- paste(fem[k], mal[k], sep=" x ")
			nm <- m$clone.or.variety.name[i]
			d$variety_code <- d$variety
			d$variety[!is.na(nm)] <- nm[!is.na(nm)]
		}
	}
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$crop <- "potato"
    d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d
}


