
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

	parse_hidap_date <- function(x, label) {
		if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NA_character_)
		x <- x[1]
		if (inherits(x, "POSIXt") || inherits(x, "Date")) {
			return(as.character(as.Date(x)))
		}
		x <- trimws(as.character(x))
		if (!nzchar(x)) return(NA_character_)
		if (grepl("/", x)) {
			if (grepl("-", x)) {
				# taking the last one, should perhaps take the average (doi_10.21223_KNC22E.R)
				x <- strsplit(x, "-")[[1]][2]
			}
			dval <- as.character(as.Date(x, format = "%d/%m/%Y"))
			if (is.na(dval)) warning(paste("NAs introduced in", label))
			return(dval)
		}
		if (grepl("^[0-9]{5}(\\.0+)?$", x)) {
			return(as.character(as.Date(as.numeric(x), origin = "1899-12-30")))
		}
		x
	}
	m$begin_date <- parse_hidap_date(m$begin_date, "begin_date")
	m$end_date <- parse_hidap_date(m$end_date, "End_date")

	d <- data.frame(
		rep = as.integer(r$rep),
		variety = trimws(as.character(r$instn)),
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
	if (!is.na(plot_size)) {
		d$plot_area <- plot_size
	}
	if (!is.null(r$ttwp)) {
		d$yield <- as.numeric(r$ttwp) * plot_adj
	}
	if (!is.null(r$mtwp)) {
		d$yield_marketable <- as.numeric(r$mtwp) * plot_adj
	}
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
		if (!is.null(soil$soil_ph)) d$soil_pH <- soil$soil_ph
		if (!is.null(soil$organic_matter)) d$soil_SOM <- soil$organic_matter
		if (is.null(soil$soil_ph) && all(c("abbreviature", "data1") %in% names(soil))) {
			sget <- function(nm) {
				hit <- soil$data1[tolower(trimws(as.character(soil$abbreviature))) == tolower(nm)]
				if (length(hit) == 0) return(NA_real_)
				val <- trimws(as.character(hit[1]))
				if (!is.na(val) && grepl("^-?[0-9]+(\\.[0-9]+)?$", val)) as.numeric(val) else NA_real_
			}
			sv <- list(
				soil_pH = sget("Soil PH"),
				soil_SOM = sget("Organic matter"),
				soil_P = sget("Phosphorus"),
				soil_K = sget("Potassium"),
				soil_sand = sget("Sand"),
				soil_silt = sget("Silt"),
				soil_clay = sget("Clay")
			)
			for (k in names(sv)) if (!is.na(sv[[k]])) d[[k]] <- sv[[k]]
		}
	}

	fm <- list.files(dirname(filename), pattern="material_list", ignore.case=TRUE, full.names=TRUE)
	ml <- NULL
	if (length(fm) > 1) {
		stop("multiple material files")
	} else if (length(fm) == 1) {
		if (grepl("\\.xlsx?$", fm, ignore.case=TRUE)) {
			ml <- carobiner::read.excel(fm, na="", col_types="text")
			ml[] <- lapply(ml, function(x) trimws(as.character(x)))
			# "Female code" -> "Female.code" as with read.delim
			names(ml) <- make.names(names(ml))
		} else {
			ml <- utils::read.delim(fm, colClasses="character", na.strings="", strip.white=TRUE)
		}
	} else {
		ms <- which(tolower(gsub(" ", "_", trimws(sheets))) == "material_list")
		if (length(ms) == 1) {
			ml <- carobiner::read.excel(filename, sheet=ms, na=c("", "NA"), col_types="text")
			ml[] <- lapply(ml, function(x) {
				x <- trimws(as.character(x))
				x[!nzchar(x)] <- NA_character_
				x
			})
			names(ml) <- make.names(names(ml))
		}
	}
	if (!is.null(ml)) {
		names(ml) <- tolower(names(ml))
		pick_col <- function(cands) {
			hit <- intersect(cands, names(ml))
			if (length(hit) == 0) rep(NA_character_, nrow(ml)) else ml[[hit[1]]]
		}
		key <- pick_col(c("institutional.number", "accession_number", "accession.number"))
		vname <- pick_col(c("clone.or.variety.name", "accession_name", "accession.name"))
		vcode <- pick_col(c("accession_code", "accession.code", "code.of.clone"))
		fc <- pick_col(c("female.code", "female_code"))
		fi <- pick_col(c("female.institutional.number", "female_accenumb", "female.accenumb"))
		mc <- pick_col(c("male.code", "male_code"))
		mi <- pick_col(c("male.institutional.number", "male_accenumb", "male.accenumb"))
		fem <- ifelse(is.na(fc), fi, fc)
		mal <- ifelse(is.na(mc), mi, mc)
		i <- match(d$variety, key)
		if (!(all(is.na(i)))) {
			j <- !is.na(i)
			k <- i[j]
			if (any(!is.na(fem[k]))) d$female_parent[j] <- fem[k]
			if (any(!is.na(mal[k]))) d$male_parent[j] <- mal[k]
			if (any(!is.na(fem[k]) & !is.na(mal[k]))) {
				ped <- ifelse(!is.na(fem[k]) & !is.na(mal[k]), paste(fem[k], mal[k], sep=" x "), NA_character_)
				d$variety_pedigree[j] <- ped
			}
			nm <- vname[i]
			nm[is.na(nm)] <- vcode[i][is.na(nm)]
			d$variety_code <- d$variety
			d$variety[!is.na(nm)] <- nm[!is.na(nm)]
		}
	}
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "potato"
	if (!is.null(d$yield)) {
		d$yield_part <- "tubers"
		d$yield_isfresh <- TRUE
	}
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d
}


