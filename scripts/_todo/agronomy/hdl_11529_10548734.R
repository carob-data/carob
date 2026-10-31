# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm and station wheat nitrogen-rate trials in Guanajuato for a GreenSeeker calibration, seasons 2009-10 through 2012-13. The title names Guanajuato.
# N at planting is kg N/ha. A trailing N on codes such as 0N is the nutrient label. 0N is 0 kg N/ha.
# Yield is already kg/ha. The 2009-2012 sheets are at 12% moisture and the 2012-2013 sheet is at 14%.
# Latitude and longitude in the file are integer degrees. Longitude is unsigned; Guanajuato is west of Greenwich, so it is stored negative.
# Conservation tillage is stored as reduced tillage. Direct planting is stored as none (no tillage).
# NDVI readings are repeated through the season and kept in the long table.

## ISSUES
# Several plots have no yield or no coordinates. Sites share the same integer-degree coordinate.
# Phosphorus and potassium are not in the workbook.
# write_files: missing variables: K_fertilizer, P_fertilizer. duplicate coordinates: fewer coordinates than locations. NA detected: yield, latitude, longitude.

## NOT PROCESSED
# algorithm Wheat-Guanajuato.xlsx and the Curated data sheet are the calibration calculations, not the plot records.
# Phenology codes next to each NDVI date. Many cells are a dash, and the stage list is not defined.
# Planting method and the pre-plant irrigation date, other than using that date to mark the crop as irrigated.
# Meta data sheet.


carob_script <- function(path) {

"
wheat experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Guanajuato.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Guanajuato.
"

	uri <- "hdl:11529/10548734"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "GreenSeeker",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;NDVI",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		out <- rep(NA_character_, length(x))
		dash <- !is.na(x) & grepl("-", x, fixed=TRUE)
		out[dash] <- substr(x[dash], 1, 10)
		rest <- !is.na(x) & !dash
		if (any(rest)) out[rest] <- as.character(as.Date(as.numeric(x[rest]), origin="1899-12-30"))
		out
	}
	clean_num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		x <- sub("N$", "", x)
		as.numeric(x)
	}
	clean_chr <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		x
	}

	f <- ff[basename(ff) == "GreenSeeker Guanajuato2009-2013.xlsx"]
	sheets <- c("2009-2010", "2010-2011", "2011-2012", "2012-2013")
	wide <- list()
	long <- list()
	for (s in sheets) {
		raw <- carobiner::read.excel(f, sheet=s, col_names=FALSE)
		h1 <- trimws(as.character(unlist(raw[1, ])))
		h2 <- trimws(as.character(unlist(raw[2, ])))
		h1[is.na(h1)] <- ""
		h2[is.na(h2)] <- ""
		body <- raw[-(1:2), ]
		col <- function(label) {
			j <- which(h1 == label)
			if (length(j) == 0) return(rep(NA, nrow(body)))
			body[[j[1]]]
		}
		jN <- which(grepl("N at Planting", h1))
		jY <- which(grepl("^Yield", h1))
		jP <- which(grepl("^Plot size", h1))
		jD <- which(h1 == "planting density (Kg/ha.)")
		jPl <- which(h1 == "Plants/m2")
		N <- clean_num(body[[jN[1]]])
		loc <- clean_chr(col("Locality"))
		muni <- clean_chr(col("Municipality"))
		keep <- !is.na(N) & !is.na(loc)
		body <- body[keep, ]
		N <- N[keep]
		loc <- loc[keep]
		muni <- muni[keep]
		lon <- clean_num(body[[which(h1 == "Longitude")[1]]])
		# The sheet stores longitude as a positive degree. Guanajuato is west of Greenwich.
		pos <- !is.na(lon) & lon > 0
		lon[pos] <- -lon[pos]
		planted <- as_date(body[[which(h1 == "Planting Date")[1]]])
		moist <- 12
		if (grepl("14", h1[jY[1]])) moist <- 14
		till <- tolower(clean_chr(body[[which(h1 == "Tillage")[1]]]))
		till[till == "direct planting"] <- "none"
		till[till == "conservation"] <- "reduced tillage"
		variety <- clean_chr(body[[which(h1 == "Hibrid")[1]]])
		seed <- rep(NA_real_, nrow(body))
		if (length(jD)) seed <- clean_num(body[[jD[1]]])
		plants <- rep(NA_real_, nrow(body))
		if (length(jPl)) plants <- clean_num(body[[jPl[1]]])
		area <- rep(NA_real_, nrow(body))
		if (length(jP)) area <- clean_num(body[[jP[1]]])
		irr_date <- as_date(body[[which(grepl("sowing irrigation", h1))[1]]])
		d1 <- data.frame(
			country = "Mexico",
			adm1 = "Guanajuato",
			location = loc,
			site = muni,
			trial_id = paste(s, loc, sep="_"),
			rep = as.integer(clean_num(body[[which(h1 == "REP")[1]]])),
			treatment = paste0(N, " kg/ha N"),
			N_fertilizer = N,
			crop = "wheat",
			variety = variety,
			yield_part = "grain",
			yield = clean_num(body[[jY[1]]]),
			yield_moisture = moist,
			planting_date = planted,
			harvest_date = as_date(body[[which(grepl("^Harvest date", h1))[1]]]),
			seed_rate = seed,
			plant_density = plants * 10000,
			plot_area = area,
			land_prep_method = till,
			latitude = clean_num(body[[which(h1 == "Latitude")[1]]]),
			longitude = lon,
			geo_from_source = TRUE,
			irrigated = !is.na(irr_date),
			on_farm = !muni %in% c("Campo experimental", "Estación Pénjamo"),
			is_survey = FALSE
		)
		d1$plot_id <- paste(d1$trial_id, d1$rep, d1$N_fertilizer, sep="_")
		date_cols <- which(h2 == "Date")
		lng <- lapply(date_cols, function(j) {
			data.frame(
				plot_id = d1$plot_id,
				trial_id = d1$trial_id,
				date = as_date(body[[j]]),
				NDVI = clean_num(body[[j + 1]])
			)
		})
		lng <- do.call(rbind, lng)
		lng <- lng[!is.na(lng$NDVI) | !is.na(lng$date), ]
		wide[[s]] <- d1
		long[[s]] <- lng
	}
	d <- do.call(rbind, wide)
	lng <- do.call(rbind, long)

	carobiner::write_files(path, meta, d, long=lng)
}
