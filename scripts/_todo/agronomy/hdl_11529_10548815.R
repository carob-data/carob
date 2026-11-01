# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot sheets report yield at 14 percent moisture in kg/ha. The 2008 to 2012 sheet labels yield as 16 percent and the numbers are tonnes/ha, so they are multiplied by 1000. The variable-description cell on that sheet also says kg/ha and 14 percent.
# Coordinates are degrees, minutes, and seconds. O means west. Unsigned longitudes above 90 degrees are stored west, matching the O mark on the other sheets at the same sites.
# The title says Guanajuato and the description says Sinaloa. The sheets name Guasave and Culiacan. No sheet states a country.
# NDVI readings are in the long table. Water is recorded as irrigation on every sheet.
# Design on the sheets is RCBD.
# Conservation agriculture is not a land-preparation term, so those three sheets have no land_prep_method.
# Sheet O.I. 15-16 (HB1) has the variable dictionary pasted over the first plots. Rows without a nitrogen rate are left out.

## ISSUES
# write_files: missing variables: on_farm, country, K_fertilizer, P_fertilizer, harvest_date. location: variable 'country' is missing. NA detected: yield, latitude, longitude.
# Eight plots have no yield. The 2008 to 2012 sheet has no coordinates and no treatment number.

## NOT PROCESSED
# Variable Description sheet.
# Growing degree days.
# SPAD readings on the 2018-19 sheets.
# Treatment numbers that share one nitrogen rate. The 2008 to 2012 sheet has no treatment number.


carob_script <- function(path) {

"
Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Guanajuato.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Sinaloa.
"

	uri <- "hdl:11529/10548815"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;NDVI",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.8
	)

	num <- function(x) {
		y <- trimws(as.character(x))
		y[is.na(y) | y %in% c("", "NA", ".", "-")] <- NA_character_
		y[!is.na(y) & grepl("[A-Za-z]", y)] <- NA_character_
		as.numeric(y)
	}
	excel_date <- function(x) {
		as.character(as.Date(num(x), origin="1899-12-30"))
	}
	as_date <- function(x) {
		y <- trimws(as.character(x))
		y[is.na(y) | y %in% c("", "NA")] <- NA_character_
		out <- rep(NA_character_, length(y))
		for (i in seq_along(y)) {
			if (is.na(y[i])) next
			if (grepl("/", y[i])) {
				p <- strsplit(y[i], "/")[[1]]
				if (length(p) < 3) next
				a <- as.numeric(p[1])
				b <- as.numeric(p[2])
				fmt <- "%d/%m/%Y"
				if (!is.na(a) && a <= 12 && !is.na(b) && b > 12) fmt <- "%m/%d/%Y"
				if (nchar(p[3]) == 2) fmt <- sub("%Y", "%y", fmt)
				out[i] <- as.character(as.Date(y[i], format=fmt))
			} else {
				out[i] <- excel_date(y[i])
			}
		}
		out
	}
	prep <- function(x) {
		y <- tolower(trimws(as.character(x)))
		if (length(y) != 1 || is.na(y) || !nzchar(y)) return(NA_character_)
		if (grepl("conventional", y)) "conventional" else NA_character_
	}
	dms <- function(x) {
		y <- as.character(x)
		y[is.na(y)] <- ""
		y <- gsub("’|‘|´|′|`", "'", y)
		y <- gsub("”|“|″", "\"", y)
		dec <- rep(NA_real_, length(y))
		hemi <- rep(NA_character_, length(y))
		for (i in seq_along(y)) {
			if (!nzchar(y[i])) next
			n <- as.numeric(unlist(regmatches(y[i], gregexpr("[0-9]+\\.?[0-9]*", y[i]))))
			if (!length(n) || all(is.na(n))) next
			v <- n[1]
			if (length(n) > 1) v <- v + n[2] / 60
			if (length(n) > 2) v <- v + n[3] / 3600
			dec[i] <- v
			hemi[i] <- toupper(gsub("[0-9.'\"°º[:space:]]", "", y[i]))
		}
		neg <- hemi %in% c("S", "W", "O")
		dec[neg] <- -abs(dec[neg])
		# Unsigned longitudes above 90 degrees on these sheets are west.
		dec
	}

	f <- ff[grepl("\\.xlsx$", basename(ff))][1]
	sheets <- c(
		"2008 to 2012", "O.I13-14", "PV14",
		"O.I. 15-16 (HA2)", "O.I 15-16 (HA1)", "O.I. 15-16 (HB1)", "O.I. 15-16 (HB2)",
		"O.I. 16-17 (PAC1)", "O.I. 16-17 (PAC2)", "O.I. 16-17 (PAC3)", "O.I. 16-17 (PAC4)",
		"O.I. 17-18 (PAC1)", "O.I. 17-18 (PAC2)", "O.I. 17-18 (PAC3)", "O.I. 17-18 (PAC4)",
		"O.I. 18-19 (PAC1)", "O.I. 18-19 (PAC2)", "O.I. 18-19 (PAC3)", "O.I. 18-19 (PAC4)",
		"O.I. 18-19 (PAC5)", "O.I. 18-19 (PAC6)", "O.I. 18-19 (PAC7)", "O.I. 18-19 (PAC8)"
	)

	read_plots <- function(sheet) {
		raw <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE, col_types="text")
		lab <- trimws(as.character(raw[[1]]))
		lab[is.na(lab)] <- ""
		val <- trimws(as.character(raw[[2]]))
		pick <- function(label) {
			i <- which(lab == label)
			if (!length(i)) NA_character_ else val[i[1]]
		}
		h <- NA_integer_
		for (i in seq_len(nrow(raw))) {
			row <- trimws(as.character(unlist(raw[i, ])))
			row[is.na(row)] <- ""
			if (any(row %in% c("Plot", "No. Parcela")) && any(grepl("^N at Planting", row))) h <- i
		}
		hdr <- trimws(as.character(unlist(raw[h, ])))
		unit <- trimws(as.character(unlist(raw[h + 1, ])))
		hdr[is.na(hdr)] <- ""
		unit[is.na(unit)] <- ""
		sub <- raw[(h + 2):nrow(raw), ]
		keep <- !is.na(num(sub[[1]]))
		sub <- sub[keep, ]
		n <- nrow(sub)
		col <- function(pat) {
			i <- grep(pat, hdr, ignore.case=TRUE)
			if (!length(i)) return(NA_integer_)
			i[1]
		}
		pull <- function(i) {
			if (is.na(i)) return(rep(NA_real_, n))
			num(sub[[i]])
		}
		lat <- dms(pick("Latitude"))
		lon <- dms(pick("Longitude"))
		if (!is.na(lon) && lon > 90) lon <- -lon
		dens <- num(gsub("seed/ha", "", pick("Density")))
		yld <- col("^Yield")
		scale <- 1
		if (!is.na(yld) && grepl("16", hdr[yld])) scale <- 1000
		moist <- if (!is.na(yld) && grepl("16", hdr[yld])) 16 else 14
		d <- data.frame(
			trial_id = paste(sheet, pick("Localidad"), pick("Cycle")),
			location = pick("Localidad"),
			adm2 = pick("Municipio"),
			latitude = lat,
			longitude = lon,
			elevation = num(gsub("msnm", "", pick("Altitude"))),
			planting_date = excel_date(pick("Planting Date")),
			variety = pick("Hibrid/Company"),
			crop = "maize",
			plot_id = trimws(as.character(sub[[grep("^Plot$|Parcela", hdr)[1]]])),
			treatment = trimws(as.character(sub[[col("^TRT$")]])),
			rep = as.integer(pull(col("^Rep$"))),
			N_fertilizer = pull(col("^N at Planting")),
			plot_area = pull(col("Plot size")),
			seed_density = dens,
			yield = pull(yld) * scale,
			yield_moisture = moist,
			yield_part = "grain",
			land_prep_method = prep(pick("Tillage")),
			irrigated = grepl("irrigation", tolower(pick("Water"))),
			is_survey = FALSE,
			geo_from_source = !is.na(lat) & !is.na(lon)
		)
		ndvi_cols <- which(hdr == "NDVI")
		long <- NULL
		for (j in ndvi_cols) {
			x <- data.frame(
				trial_id = d$trial_id,
				plot_id = d$plot_id,
				date = as_date(unit[j]),
				NDVI = num(sub[[j]])
			)
			long <- if (is.null(long)) x else rbind(long, x)
		}
		list(d=d, long=long)
	}

	read_shifted <- function(sheet) {
		raw <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE, col_types="text")
		lab <- trimws(as.character(raw[[1]]))
		lab[is.na(lab)] <- ""
		val <- trimws(as.character(raw[[2]]))
		pick <- function(label) {
			i <- which(lab == label)
			if (!length(i)) NA_character_ else val[i[1]]
		}
		ndvi_n <- apply(raw, 1, function(r) sum(trimws(as.character(r)) == "NDVI", na.rm=TRUE))
		h <- which(ndvi_n >= 3)[1]
		hdr <- trimws(as.character(unlist(raw[h, ])))
		unit <- trimws(as.character(unlist(raw[h + 1, ])))
		hdr[is.na(hdr)] <- ""
		unit[is.na(unit)] <- ""
		sub <- raw[(h + 2):nrow(raw), ]
		keep <- !is.na(num(sub[[4]])) & !is.na(num(sub[[6]]))
		sub <- sub[keep, ]
		n <- nrow(sub)
		lat <- dms(pick("Latitude"))
		lon <- dms(pick("Longitude"))
		if (!is.na(lon) && lon > 90) lon <- -lon
		rep <- as.integer(num(sub[[2]]))
		trt <- trimws(as.character(sub[[3]]))
		pid <- trimws(as.character(sub[[1]]))
		pid[is.na(num(pid))] <- paste0("r", rep[is.na(num(pid))], "t", trt[is.na(num(pid))])
		d <- data.frame(
			trial_id = paste(sheet, pick("Localidad"), pick("Cycle")),
			location = pick("Localidad"),
			adm2 = pick("Municipio"),
			latitude = lat,
			longitude = lon,
			elevation = num(gsub("msnm", "", pick("Altitude"))),
			planting_date = excel_date(pick("Planting Date")),
			variety = pick("Hibrid/Company"),
			crop = "maize",
			plot_id = pid,
			treatment = trt,
			rep = rep,
			N_fertilizer = num(sub[[4]]),
			plot_area = num(sub[[5]]),
			seed_density = num(gsub("seed/ha", "", pick("Density"))),
			yield = num(sub[[6]]),
			yield_moisture = 14,
			yield_part = "grain",
			land_prep_method = prep(pick("Tillage")),
			irrigated = grepl("irrigation", tolower(pick("Water"))),
			is_survey = FALSE,
			geo_from_source = !is.na(lat) & !is.na(lon)
		)
		ndvi_cols <- which(hdr == "NDVI")
		long <- NULL
		for (j in ndvi_cols) {
			x <- data.frame(
				trial_id = d$trial_id,
				plot_id = d$plot_id,
				date = as_date(unit[j]),
				NDVI = num(sub[[j]])
			)
			long <- if (is.null(long)) x else rbind(long, x)
		}
		list(d=d, long=long)
	}

	read_early <- function(sheet) {
		raw <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE, col_types="text")
		c1 <- trimws(as.character(raw[[1]]))
		c1[is.na(c1)] <- ""
		val <- trimws(as.character(raw[[2]]))
		pick <- function(label) {
			i <- which(c1 == label)
			if (!length(i)) NA_character_ else val[i[1]]
		}
		c3 <- trimws(as.character(raw[[3]]))
		c3[is.na(c3)] <- ""
		hloc <- which(c1 == "Loc" & c3 == "Locality")[1]
		hdat <- which(c1 == "Loc" & grepl("N at Planting", as.character(raw[[3]])))[1]
		loc <- raw[(hloc + 1):(hdat - 1), ]
		loc <- loc[!is.na(num(loc[[1]])), ]
		sites <- data.frame(
			loc = as.integer(num(loc[[1]])),
			cycle = trimws(as.character(loc[[2]])),
			location = trimws(as.character(loc[[3]])),
			planting_date = excel_date(loc[[4]]),
			dap6 = num(loc[[5]]),
			dap8 = num(loc[[6]]),
			dap12 = num(loc[[7]])
		)
		sub <- raw[(hdat + 1):nrow(raw), ]
		sub <- sub[!is.na(num(sub[[1]])) & !is.na(num(sub[[3]])), ]
		n <- nrow(sub)
		key <- paste(as.integer(num(sub[[1]])), trimws(as.character(sub[[2]])))
		sk <- paste(sites$loc, sites$cycle)
		hit <- match(key, sk)
		d <- data.frame(
			trial_id = paste(sheet, sites$location[hit], sites$cycle[hit]),
			location = sites$location[hit],
			adm2 = rep(NA_character_, n),
			latitude = rep(NA_real_, n),
			longitude = rep(NA_real_, n),
			elevation = rep(NA_real_, n),
			planting_date = sites$planting_date[hit],
			variety = rep(NA_character_, n),
			crop = "maize",
			plot_id = as.character(seq_len(n)),
			treatment = rep(NA_character_, n),
			rep = rep(NA_integer_, n),
			N_fertilizer = num(sub[[3]]),
			plot_area = rep(NA_real_, n),
			seed_density = rep(NA_real_, n),
			yield = num(sub[[7]]) * 1000,
			yield_moisture = 16,
			yield_part = "grain",
			land_prep_method = prep(pick("Tillage")),
			irrigated = grepl("irrigation", tolower(pick("Water"))),
			is_survey = FALSE,
			geo_from_source = FALSE
		)
		long <- NULL
		for (k in 1:3) {
			dap <- sites[[paste0("dap", c(6, 8, 12)[k])]][hit]
			date <- as.character(as.Date(d$planting_date) + dap)
			x <- data.frame(
				trial_id = d$trial_id,
				plot_id = d$plot_id,
				date = date,
				NDVI = num(sub[[3 + k]])
			)
			long <- if (is.null(long)) x else rbind(long, x)
		}
		list(d=d, long=long)
	}

	parts <- list()
	for (s in sheets) {
		if (s == "2008 to 2012") parts[[s]] <- read_early(s)
		else if (s == "O.I. 15-16 (HB1)") parts[[s]] <- read_shifted(s)
		else parts[[s]] <- read_plots(s)
	}
	d <- do.call(rbind, lapply(parts, function(x) x$d))
	long <- do.call(rbind, lapply(parts, function(x) x$long))
	ok <- !is.na(d$location) & nzchar(d$location)
	if (any(ok)) d$location[ok] <- carobiner::fix_name(d$location[ok], "title")
	ok <- !is.na(d$adm2) & nzchar(d$adm2)
	if (any(ok)) d$adm2[ok] <- carobiner::fix_name(d$adm2[ok], "title")
	d <- d[!is.na(d$N_fertilizer), ]
	d$record_id <- as.integer(seq_len(nrow(d)))
	long <- merge(long, d[, c("trial_id", "plot_id", "record_id")], by=c("trial_id", "plot_id"))
	long <- long[!is.na(long$NDVI) & !is.na(long$date), ]
	long$trial_id <- NULL
	long$plot_id <- NULL
	if (all(is.na(d$variety))) d$variety <- NULL
	if (all(is.na(d$seed_density))) d$seed_density <- NULL
	if (all(is.na(d$plot_area))) d$plot_area <- NULL
	if (all(is.na(d$adm2))) d$adm2 <- NULL
	if (all(is.na(d$elevation))) d$elevation <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
