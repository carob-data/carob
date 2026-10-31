# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yield is already kg/ha at 14% moisture. Nitrogen is the planting rate in the plot table.
# Each Data sheet is one site. Planting date, variety, tillage, and irrigation come from the matching column of Variable Description.
# The irrigation cell says Rainfull, so the trials are not irrigated.
# Tillage is Conventional Tillage.
# Harvest population is plants/m2. It is stored as plants/ha. In 2019 the numbers are already about 50,000 while the unit cell still says plants/m2, so those values are stored as plants/ha without a further conversion.
# NDVI header cells are Excel serials or calendar dates. 9/26/2016 is month/day because 26 cannot be a month.
# Country and coordinates are not in the sheets or in geographicCoverage.

## ISSUES
# Country and coordinates are not in the sheets or in geographicCoverage.
# Elemental phosphorus is not stored. The variable description gives product rates, not a P2O5 percentage.
# write_files: missing variables: country, latitude, longitude, K_fertilizer, P_fertilizer, harvest_date. contributor:

## NOT PROCESSED
# Map sheet and Wheather sheet.
# Phenology stage names.
# Phosphorus rows name a product and a kg/ha rate (triple superphosphate and diammonium phosphate) but not the P2O5 percentage, so elemental P is not calculated.
# Potassium is not in the variable description.


carob_script <- function(path) {

"
Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Campeche.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Campeche.
"

	uri <- "hdl:11529/10548684"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;NDVI",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		kept <- gsub("[^0-9.]", "", x)
		bad <- !is.na(x) & (kept != x | !grepl("[0-9]", x) | grepl("\\..*\\.", x))
		x[bad] <- NA_character_
		as.numeric(x)
	}

	parse_date <- function(x, dmy) {
		x <- trimws(gsub("[\r\n]", "", as.character(x)))
		out <- rep(NA_character_, length(x))
		for (i in seq_along(x)) {
			v <- x[i]
			if (is.na(v) || !nzchar(v) || v == "NA") next
			whole <- sub("\\.0+$", "", v)
			digits <- gsub("[^0-9]", "", whole)
			if (whole == digits && nchar(whole) >= 5) {
				out[i] <- as.character(as.Date(as.numeric(whole), origin="1899-12-30"))
				next
			}
			if (grepl("[A-Za-z]", v)) {
				p <- strsplit(v, "-")[[1]]
				if (length(p) != 3) next
				mon <- match(tolower(substr(p[2], 1, 3)), c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
				if (is.na(mon)) next
				day <- as.numeric(p[1])
				yr <- as.numeric(p[3])
				if (is.na(day) || is.na(yr)) next
				if (yr < 100) yr <- yr + 2000
				out[i] <- sprintf("%04d-%02d-%02d", yr, mon, day)
				next
			}
			p <- strsplit(v, "/")[[1]]
			if (length(p) != 3) next
			a <- as.numeric(p[1])
			b <- as.numeric(p[2])
			yr <- as.numeric(p[3])
			if (any(is.na(c(a, b, yr)))) next
			if (yr < 100) yr <- yr + 2000
			if (yr < 1990 || yr > 2030) next
			if (a > 12) {
				day <- a
				mon <- b
			} else if (b > 12) {
				day <- b
				mon <- a
			} else if (isTRUE(dmy)) {
				day <- a
				mon <- b
			} else if (isFALSE(dmy)) {
				day <- b
				mon <- a
			} else {
				next
			}
			if (mon < 1 || mon > 12 || day < 1 || day > 31) next
			out[i] <- sprintf("%04d-%02d-%02d", yr, mon, day)
		}
		out
	}

	day_order <- function(x) {
		a <- c()
		b <- c()
		for (v in x) {
			p <- strsplit(v, "/")[[1]]
			if (length(p) != 3) next
			a <- c(a, as.numeric(p[1]))
			b <- c(b, as.numeric(p[2]))
		}
		if (any(a > 12, na.rm=TRUE) && !any(b > 12, na.rm=TRUE)) return(TRUE)
		if (any(b > 12, na.rm=TRUE) && !any(a > 12, na.rm=TRUE)) return(FALSE)
		NA
	}

	site_info <- function(f) {
		info <- carobiner::read.excel(f, sheet="Variable Description", col_names=FALSE)
		labs <- as.character(info[[2]])
		getv <- function(col, label) {
			i <- which(labs == label)
			if (length(i) == 0) return(NA_character_)
			paste(as.character(info[i, col]), collapse=" | ")
		}
		name_at <- which(!is.na(as.character(unlist(info[1, ]))))
		out <- list()
		for (j in name_at) {
			site <- trimws(as.character(info[1, j]))
			if (!nzchar(site)) next
			irr_lab <- labs[grep("irrigation", labs, ignore.case=TRUE)]
			irr_lab <- if (length(irr_lab) == 0) "Prepalanting/ sowing irrigation" else irr_lab[1]
			out[[site]] <- list(
				planting = getv(j + 2, "Planting Date"),
				variety = getv(j + 2, "Hibrid/Company"),
				tillage = getv(j + 2, "Tillage"),
				irrigation = getv(j + 2, irr_lab)
			)
		}
		out
	}

	site_key <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x <- sub(" 20[0-9]{2}$", "", x)
		x <- sub("^nuevo ", "", x)
		x <- sub("^el ", "", x)
		x <- gsub(" el ", " ", x)
		gsub("[^a-z0-9]", "", x)
	}

	lookup_site <- function(loc, info) {
		keys <- names(info)
		if (loc %in% keys) return(info[[loc]])
		want <- site_key(loc)
		hit <- keys[vapply(keys, site_key, character(1)) == want]
		if (length(hit) == 1) return(info[[hit]])
		NULL
	}

	books <- list(
		"GreenSeeker Campeche 2016.xlsx" = c("Data-Hopelchén", "Data-Calkiní ", "Data-Hopelchén-Nuevo Progreso", "Data-Pocyaxum"),
		"GreenSeeker Campeche 2017.xlsx" = c("Data-Hopelchén", "Data-Nuevo Progreso (Hopelchén)", "Data-Nohacal ", "Data-Pocyaxum"),
		"GreenSeeker Campeche 2019.xlsx" = "Data-Hopelchén"
	)

	d_list <- list()
	long_list <- list()
	id0 <- 0L
	for (bn in names(books)) {
		f <- ff[basename(ff) == bn]
		year <- sub("^.*([0-9]{4}).*$", "\\1", bn)
		info <- site_info(f)
		sheets <- books[[bn]]
		headers <- unlist(lapply(sheets, function(s) {
			raw <- carobiner::read.excel(f, sheet=s, col_names=FALSE)
			hdr <- as.character(unlist(raw[1, ]))
			unit <- as.character(unlist(raw[2, ]))
			unit[hdr == "NDVI"]
		}))
		dmy <- day_order(headers)
		for (s in sheets) {
			raw <- carobiner::read.excel(f, sheet=s, col_names=FALSE)
			hdr <- trimws(as.character(unlist(raw[1, ])))
			unit <- as.character(unlist(raw[2, ]))
			r <- raw[-(1:2), ]
			r <- r[!is.na(r[[1]]), ]
			if (nrow(r) == 0) next
			loc <- trimws(sub("^Data[- ]*", "", s))
			meta_s <- lookup_site(loc, info)
			n_col <- if ("Total N" %in% hdr) which(hdr == "Total N")[1] else which(hdr == "N at Planting")[1]
			y_col <- which(hdr == "Yield at 14% hum")[1]
			p_col <- which(hdr == "Plot size")
			h_col <- which(hdr == "Harvest Pop.")
			n_fert <- num(r[[n_col]])
			pop <- if (length(h_col) == 0) rep(NA_real_, nrow(r)) else num(r[[h_col[1]]])
			if (length(h_col) >= 1 && isTRUE(grepl("m", unit[h_col[1]])) && is.finite(stats::median(pop, na.rm=TRUE)) && stats::median(pop, na.rm=TRUE) < 100) {
				pop <- pop * 10000
			}
			till <- if (is.null(meta_s)) NA_character_ else meta_s$tillage
			land <- NA_character_
			if (!is.na(till) && grepl("conventional", tolower(till))) land <- "conventional"
			irr_txt <- if (is.null(meta_s)) NA_character_ else meta_s$irrigation
			if (length(irr_txt) != 1) irr_txt <- NA_character_
			irrigated <- NA
			irrigation_dates <- NA_character_
			if (!is.na(irr_txt) && grepl("rain", tolower(irr_txt))) {
				irrigated <- FALSE
			} else if (!is.na(irr_txt)) {
				irrigation_dates <- parse_date(irr_txt, dmy)
				if (length(irrigation_dates) == 1 && !is.na(irrigation_dates)) irrigated <- TRUE
			}
			d <- data.frame(
				crop = "maize",
				location = loc,
				trial_id = paste(loc, year, sep="_"),
				plot_id = as.character(r[[which(hdr == "Plot")[1]]]),
				rep = as.integer(num(r[[which(hdr == "Rep")[1]]])),
				treatment = as.character(n_fert),
				N_fertilizer = n_fert,
				plot_area = if (length(p_col) == 0) NA_real_ else num(r[[p_col[1]]]),
				plant_density = pop,
				yield = num(r[[y_col]]),
				yield_moisture = 14,
				yield_part = "grain",
				variety = if (is.null(meta_s)) NA_character_ else meta_s$variety,
				land_prep_method = land,
				planting_date = if (is.null(meta_s)) NA_character_ else parse_date(meta_s$planting, dmy),
				irrigated = irrigated,
				irrigation_dates = irrigation_dates,
				on_farm = NA,
				is_survey = FALSE,
				geo_from_source = FALSE
			)
			d$record_id <- id0 + seq_len(nrow(d))
			id0 <- id0 + nrow(d)
			ndvi_cols <- which(hdr == "NDVI")
			ndvi_dates <- parse_date(unit[ndvi_cols], dmy)
			long <- NULL
			for (i in seq_along(ndvi_cols)) {
				x <- data.frame(
					record_id = d$record_id,
					date = ndvi_dates[i],
					NDVI = num(r[[ndvi_cols[i]]])
				)
				long <- if (is.null(long)) x else rbind(long, x)
			}
			long <- long[!is.na(long$NDVI) & !is.na(long$date), ]
			d_list[[length(d_list) + 1]] <- d
			long_list[[length(long_list) + 1]] <- long
		}
	}

	d <- do.call(rbind, d_list)
	long <- do.call(rbind, long_list)
	carobiner::write_files(path, meta, d, long=long)
}
