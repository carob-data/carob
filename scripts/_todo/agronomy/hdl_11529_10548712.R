# R script for "carob"
# license: GPL (>=3)

## NOTES
# Maize nitrogen-rate trials for a GreenSeeker calibration in Querétaro. The title names the state. Country is Mexico. The workbooks do not give coordinates.
# Yield is already kg/ha at 14% moisture.
# Nitrogen is Total N when that column is present, otherwise N at planting. A trailing N on values such as 0N is removed. A zero rate stays 0.
# Triple superphosphate product rates use 19.23% elemental P from the fertilizer vocabulary. A dash in the rate cell is 0.
# K-Mag is not in that vocabulary, so those potassium rates are not converted.
# NDVI columns are repeated dates and are kept in the long table. Daily maximum and minimum temperature, where a weather sheet is present, are passed as weather.
# The variable description names one site. Variety, planting date, tillage, and fertilizer rates from that sheet are applied only when the site name matches the data sheet.

## ISSUES
# No coordinates. K-Mag rates are not converted. Some plots have no yield. A recorded plant density of 0 is below the vocabulary minimum. One NDVI cell was written 0,893 and is stored as 0.893.
# The 2016 Pedro Escobedo harvest population is already plants per hectare, so it is not multiplied by 10000.
# write_files: missing variables: latitude, longitude, K_fertilizer, harvest_date. out of bounds: plant_density (0, 120000). NA detected: yield.

## NOT PROCESSED
# Map sheets.
# Phenology stage dates in the variable description.
# Potassium rates whose product is K-Mag.


carob_script <- function(path) {

"
Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Querétaro.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Querétaro.
"

	uri <- "hdl:11529/10548712"
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
		carob_completion = 65,
		carob_effort = 0.7
	)

	num <- function(z) {
		if (is.null(z)) return(NA_real_)
		z <- trimws(as.character(z))
		z[z %in% c("", "-", "–", "NA", "N/A", "n/a", "#N/A", ".", "NULL")] <- NA_character_
		z <- sub("[Nn]$", "", z)
		z <- trimws(z)
		z[z %in% c("", "-")] <- NA_character_
		z[!is.na(z) & grepl("[[:alpha:]]", z)] <- NA_character_
		dec <- !is.na(z) & grepl(",", z) & !grepl("\\.", z)
		z[dec] <- sub(",", ".", z[dec])
		as.numeric(z)
	}
	excel_date <- function(z) {
		if (inherits(z, "POSIXt") || inherits(z, "Date")) return(as.character(as.Date(z)))
		z <- trimws(as.character(z))
		z[is.na(z) | z %in% c("", "-", "–", "NA")] <- NA_character_
		bad <- !is.na(z) & (grepl("/", z) | grepl("[[:alpha:]]", z))
		z[bad] <- NA_character_
		iso <- !is.na(z) & nchar(z) >= 10 & substr(z, 5, 5) == "-"
		out <- rep(NA_character_, length(z))
		out[iso] <- substr(z[iso], 1, 10)
		rest <- !iso & !is.na(z)
		out[rest] <- as.character(as.Date(as.numeric(z[rest]), origin="1899-12-30"))
		out
	}
	norm <- function(z) tolower(gsub("[^a-z0-9]", "", z))
	p_rate <- function(x) {
		if (is.na(x)) return(NA_real_)
		x <- trimws(as.character(x))
		if (!nzchar(x)) return(NA_real_)
		if (x %in% c("-", "–")) return(0)
		low <- tolower(x)
		piece <- trimws(sub("[Kk]g.*", "", x))
		rate <- as.numeric(piece)
		if (grepl("triple|super fosfato", low)) return(rate * 19.23 / 100)
		NA_real_
	}
	k_rate <- function(x) {
		if (is.na(x)) return(NA_real_)
		x <- trimws(as.character(x))
		if (!nzchar(x)) return(NA_real_)
		if (x %in% c("-", "–")) return(0)
		NA_real_
	}
	xlsx_sheets <- function(f) {
		td <- tempfile("sheets")
		dir.create(td)
		unzip(f, files="xl/workbook.xml", exdir=td)
		xml <- paste(readLines(file.path(td, "xl", "workbook.xml"), warn=FALSE), collapse="")
		hit <- regmatches(xml, gregexpr("name=\"[^\"]*\"", xml))[[1]]
		gsub("^name=\"|\"$", "", hit)
	}

	read_info <- function(f, sheets) {
		s <- sheets[grepl("^Variable Description", sheets)]
		if (!length(s)) return(NULL)
		info <- carobiner::read.excel(f, sheet=s[1], col_names=FALSE)
		lab <- trimws(as.character(info[[2]]))
		val <- trimws(as.character(info[[3]]))
		site <- trimws(as.character(info[[1]][1]))
		pick <- function(pat) {
			i <- grep(pat, lab, ignore.case=TRUE)
			if (!length(i)) NA_character_ else val[i[1]]
		}
		till <- tolower(pick("^Tillage$"))
		prep <- NA_character_
		if (!is.na(till) && grepl("conventional", till)) prep <- "conventional"
		if (!is.na(till) && grepl("zero", till)) prep <- "none"
		ir <- val[grepl("irrigation", lab, ignore.case=TRUE)]
		ir <- excel_date(ir)
		ir <- ir[!is.na(ir)]
		list(
			site = site,
			variety = pick("Hibrid/Company"),
			planting_date = excel_date(pick("Planting Date")),
			land_prep_method = prep,
			P_fertilizer = p_rate(pick("Phosphorus fertilizer")),
			K_fertilizer = k_rate(pick("^potassium")),
			irrigation_dates = if (length(ir)) paste(ir, collapse="; ") else NA_character_,
			irrigated = if (length(ir)) TRUE else NA
		)
	}

	read_wth <- function(f, sheets) {
		s <- sheets[trimws(tolower(sheets)) %in% c("wheather", "weather")]
		if (!length(s)) return(NULL)
		raw <- carobiner::read.excel(f, sheet=s[1], col_names=FALSE)
		hdr_row <- NA_integer_
		for (i in seq_len(nrow(raw))) {
			cells <- tolower(trimws(as.character(unlist(raw[i, ]))))
			if (any(cells == "date", na.rm=TRUE)) hdr_row <- i
		}
		if (is.na(hdr_row)) return(NULL)
		hdr <- tolower(trimws(as.character(unlist(raw[hdr_row, ]))))
		id <- which(hdr == "date")[1]
		ix <- which(grepl("max", hdr))[1]
		in_ <- which(grepl("min", hdr))[1]
		if (is.na(id) || is.na(ix) || is.na(in_)) return(NULL)
		body <- raw[(hdr_row + 1):nrow(raw), ]
		date <- excel_date(body[[id]])
		ok <- !is.na(date)
		station <- trimws(as.character(raw[[1]][1]))
		if (is.na(station) || !nzchar(station) || tolower(station) == "date") station <- NA_character_
		data.frame(
			station_name = station,
			date = date[ok],
			tmax = num(body[[ix]][ok]),
			tmin = num(body[[in_]][ok])
		)
	}

	read_sheet <- function(f, sheet, info) {
		raw <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		hdr <- trimws(as.character(unlist(raw[1, ])))
		unit <- as.character(unlist(raw[2, ]))
		body <- raw[-(1:2), ]
		find <- function(pat) {
			i <- grep(pat, hdr, ignore.case=TRUE)
			if (!length(i)) NA_integer_ else i[1]
		}
		i_n <- find("^Total N")
		if (is.na(i_n)) i_n <- find("^N at Planting")
		i_y <- find("^Yield")
		i_plot <- find("^Plot$")
		if (is.na(i_plot)) i_plot <- find("^Plot ")
		i_rep <- find("^Rep$")
		i_area <- find("^Plot size")
		i_pop <- find("^Harvest Pop")
		if (is.na(i_y) || is.na(i_n)) return(NULL)
		yield <- num(body[[i_y]])
		nrate <- num(body[[i_n]])
		keep <- !is.na(nrate)
		if (!any(keep)) return(NULL)
		body <- body[keep, ]
		yield <- yield[keep]
		nrate <- nrate[keep]
		pop <- if (is.na(i_pop)) rep(NA_real_, length(nrate)) else num(body[[i_pop]])
		if (!all(is.na(pop)) && median(pop, na.rm=TRUE) < 100) pop <- pop * 10000
		loc <- trimws(sub("^Data[- ]*", "", trimws(sheet)))
		loc <- carobiner::fix_name(loc, "title")
		year <- sub(".*([0-9]{4}).*", "\\1", basename(f))
		use <- !is.null(info) && nzchar(info$site) && grepl(norm(info$site), norm(loc), fixed=TRUE)
		d <- data.frame(
			country = "Mexico",
			location = loc,
			trial_id = paste(loc, year),
			plot_id = if (is.na(i_plot)) NA_character_ else as.character(body[[i_plot]]),
			rep = if (is.na(i_rep)) NA_integer_ else as.integer(num(body[[i_rep]])),
			crop = "maize",
			treatment = as.character(nrate),
			N_fertilizer = nrate,
			plot_area = if (is.na(i_area)) NA_real_ else num(body[[i_area]]),
			plant_density = pop,
			yield = yield,
			yield_moisture = 14,
			yield_part = "grain",
			variety = if (use) info$variety else NA_character_,
			planting_date = if (use) info$planting_date else NA_character_,
			land_prep_method = if (use) info$land_prep_method else NA_character_,
			P_fertilizer = if (use) info$P_fertilizer else NA_real_,
			K_fertilizer = if (use) info$K_fertilizer else NA_real_,
			irrigation_dates = if (use) info$irrigation_dates else NA_character_,
			irrigated = if (use) info$irrigated else NA,
			on_farm = NA,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
		ndvi_cols <- which(hdr == "NDVI")
		long <- NULL
		if (length(ndvi_cols)) {
			dates <- excel_date(unit[ndvi_cols])
			for (i in seq_along(ndvi_cols)) {
				val <- num(body[[ndvi_cols[i]]])
				val[!is.na(val) & val > 1] <- NA_real_
				part <- data.frame(row = seq_along(val), date = dates[i], NDVI = val)
				long <- if (is.null(long)) part else rbind(long, part)
			}
			long <- long[!is.na(long$NDVI), ]
		}
		list(d=d, long=long, keep=which(keep))
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- NULL
	long <- NULL
	wth <- NULL
	for (f in ff) {
		sheets <- xlsx_sheets(f)
		info <- read_info(f, sheets)
		data_sheets <- sheets[grepl("^Data", sheets)]
		for (s in data_sheets) {
			got <- read_sheet(f, s, info)
			if (is.null(got)) next
			n0 <- if (is.null(d)) 0 else nrow(d)
			d <- if (is.null(d)) got$d else rbind(d, got$d)
			if (!is.null(got$long) && nrow(got$long)) {
				got$long$row <- got$long$row + n0
				long <- if (is.null(long)) got$long else rbind(long, got$long)
			}
		}
		w <- read_wth(f, sheets)
		if (!is.null(w) && nrow(w)) wth <- if (is.null(wth)) w else rbind(wth, w)
	}
	if (all(is.na(d$K_fertilizer))) d$K_fertilizer <- NULL
	if (all(is.na(d$P_fertilizer))) d$P_fertilizer <- NULL
	d$record_id <- as.integer(seq_len(nrow(d)))
	if (!is.null(long) && nrow(long)) {
		long$record_id <- as.integer(long$row)
		long$row <- NULL
	} else {
		long <- NULL
	}
	if (is.null(long) && is.null(wth)) {
		carobiner::write_files(path, meta, d)
	} else if (is.null(wth)) {
		carobiner::write_files(path, meta, d, long=long)
	} else if (is.null(long)) {
		carobiner::write_files(path, meta, d, wth=wth)
	} else {
		carobiner::write_files(path, meta, d, long=long, wth=wth)
	}
}
