# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yield is already kg/ha at 14% moisture. Nitrogen is total N when that column is present, otherwise the planting rate.
# Each Data sheet is one site. Planting date, variety, tillage, and irrigation come from the matching column of Variable Description.
# A rainfed irrigation cell is stored as not irrigated. A date in that cell is an irrigation date. Several irrigation dates are joined with "; ".
# Tillage is stored as conventional when the description says conventional.
# Harvest population in plants/m2 is stored as plants/ha when the site median is below 100.
# NDVI header cells are Excel serials or calendar dates. 15-Aug style dates are read as day, month, year.
# The title names Edo. México, so the country is Mexico. The sheets and geographicCoverage have no coordinates.
# El Batán 2014 states 40 kg/ha of P2O5. That rate is stored as elemental P. The same cell for potassium is empty.
# The 2014 preplant cell is "41780 after rain fed", so that site is stored as not irrigated.
# The 2015 planting cell is "May 28 and 29 , 2015", so no single planting date is stored.
# Texcoco 2012 tillage is conservation agriculture. That is not a land-preparation term, so it is left blank.
# The variable description spells Tejupilco as Tejupilgo.

## ISSUES
# No coordinates in the sheets or in geographicCoverage. Country is Mexico because the title names Edo. México.
# Product rates of triple superphosphate and potassium chloride are not converted. Elemental P is stored only for El Batán 2014, from 40 kg/ha of P2O5.
# One NDVI cell is a period and two NDVI cells are dates. Those are not stored.
# write_files: missing variables: latitude, longitude, K_fertilizer, harvest_date. contributor:

## NOT PROCESSED
# Map sheet and Wheather sheet.
# Phenology stage names.
# Phosphorus and potassium product rates, except the 2014 P2O5 rate.
# Texcoco 2012 conservation agriculture.


carob_script <- function(path) {

"
Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Edo. México.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Edo. México.
"

	uri <- "hdl:11529/10548710"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
		x[x %in% c("", "NA", "-", ".")] <- NA_character_
		bad <- !is.na(x) & (grepl("[A-Za-z]", x) | grepl("/", x))
		x[bad] <- NA_character_
		as.numeric(x)
	}
	n_rate <- function(x) {
		x0 <- trimws(as.character(x))
		trip <- !is.na(x0) & grepl("^[0-9]+(\\s*-\\s*[0-9]+){2}$", x0)
		x0[trip] <- sub("\\s*-.*", "", x0[trip])
		num(x0)
	}

	parse_one <- function(v, dmy) {
		v <- trimws(gsub("[\r\n]", " ", as.character(v)))
		if (is.na(v) || !nzchar(v) || v %in% c("NA", "-")) return(NA_character_)
		lead <- substr(v, 1, 5)
		if (nchar(v) >= 5 && gsub("[^0-9]", "", lead) == lead) {
			return(as.character(as.Date(as.numeric(lead), origin="1899-12-30")))
		}
		if (grepl("[A-Za-z]", v)) {
			p <- strsplit(v, "-")[[1]]
			if (length(p) != 3) return(NA_character_)
			mon <- match(tolower(substr(p[2], 1, 3)), c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
			if (is.na(mon)) return(NA_character_)
			day <- as.numeric(p[1])
			yr <- as.numeric(p[3])
			if (is.na(day) || is.na(yr)) return(NA_character_)
			if (yr < 100) yr <- yr + 2000
			return(sprintf("%04d-%02d-%02d", yr, mon, day))
		}
		p <- strsplit(v, "/")[[1]]
		if (length(p) != 3) return(NA_character_)
		a <- as.numeric(p[1])
		b <- as.numeric(p[2])
		yr <- as.numeric(p[3])
		if (any(is.na(c(a, b, yr)))) return(NA_character_)
		if (yr < 100) yr <- yr + 2000
		if (yr < 1990 || yr > 2030) return(NA_character_)
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
			return(NA_character_)
		}
		if (mon < 1 || mon > 12 || day < 1 || day > 31) return(NA_character_)
		sprintf("%04d-%02d-%02d", yr, mon, day)
	}

	parse_date <- function(x, dmy) {
		vapply(x, parse_one, character(1), dmy=dmy, USE.NAMES=FALSE)
	}

	day_order <- function(x) {
		a <- c()
		b <- c()
		for (v in x) {
			p <- strsplit(as.character(v), "/")[[1]]
			if (length(p) != 3) next
			a <- c(a, as.numeric(p[1]))
			b <- c(b, as.numeric(p[2]))
		}
		if (length(a) == 0) return(NA)
		if (any(a > 12, na.rm=TRUE) && !any(b > 12, na.rm=TRUE)) return(TRUE)
		if (any(b > 12, na.rm=TRUE) && !any(a > 12, na.rm=TRUE)) return(FALSE)
		NA
	}

	site_key <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x <- chartr("áéíóúñü", "aeiounu", x)
		x <- sub(" 20[0-9]{2}$", "", x)
		x <- sub("^data[- ]*", "", x)
		x <- sub("^el ", "", x)
		gsub("[^a-z0-9]", "", x)
	}

	oxide_kg <- function(txt, oxide, div) {
		txt <- txt[1]
		if (is.na(txt) || !nzchar(txt) || txt == "-") return(NA_real_)
		if (!grepl(oxide, txt, ignore.case=TRUE)) return(NA_real_)
		if (grepl("treatment", txt, ignore.case=TRUE)) return(NA_real_)
		hit <- regexpr("[0-9]+(\\.[0-9]+)?", txt)
		if (hit < 1) return(NA_real_)
		as.numeric(substr(txt, hit, hit + attr(hit, "match.length") - 1)) / div
	}

	site_info <- function(f) {
		info <- carobiner::read.excel(f, sheet="Variable Description", col_names=FALSE)
		name_at <- which(!is.na(as.character(unlist(info[1, ]))))
		out <- list()
		for (j in name_at) {
			site <- trimws(as.character(info[1, j]))
			if (!nzchar(site) || site == "NA") next
			if (j + 2 > ncol(info)) next
			labs <- as.character(info[[j + 1]])
			vals <- trimws(as.character(info[[j + 2]]))
			getv <- function(label) {
				i <- which(labs == label)
				if (length(i) == 0) return(NA_character_)
				paste(vals[i], collapse=" | ")
			}
			irr_i <- grep("irrigation", labs, ignore.case=TRUE)
			k_i <- grep("^potassium", labs, ignore.case=TRUE)
			out[[site]] <- list(
				planting = getv("Planting Date"),
				variety = getv("Hibrid/Company"),
				tillage = getv("Tillage"),
				irrigation = if (length(irr_i) == 0) NA_character_ else vals[irr_i],
				p_txt = getv("Phosphorus fertilizer rate"),
				k_txt = if (length(k_i) == 0) NA_character_ else paste(vals[k_i], collapse=" | ")
			)
		}
		out
	}

	lookup_site <- function(loc, info) {
		keys <- names(info)
		if (length(keys) == 1) return(info[[keys]])
		want <- site_key(loc)
		keyed <- vapply(keys, site_key, character(1))
		hit <- keys[keyed == want]
		if (length(hit) == 1) return(info[[hit]])
		inside <- vapply(keyed, function(k) {
			nzchar(want) && nzchar(k) && (grepl(want, k, fixed=TRUE) || grepl(k, want, fixed=TRUE))
		}, logical(1))
		hit <- keys[inside]
		if (length(hit) == 1) return(info[[hit]])
		pref <- substr(want, 1, 7)
		hit <- keys[substr(keyed, 1, 7) == pref]
		if (length(hit) == 1) return(info[[hit]])
		NULL
	}

	books <- list(
		"GreenSeeker Edo. México 2011-2012.xlsx" = "Data-Tejupilco ",
		"GreenSeeker Edo. México 2012.xlsx" = c("Data-Texcoco (Chapingo)", "Data-San Miguel (Tejupilco)", "Data-Metepec", "Data-Cocotitlán (Las Cruces)"),
		"GreenSeeker Edo. México 2013.xlsx" = c("Data-Huamantla, Tlax", "Data-Juchitepec (Los Cedros)", "Data-Temantla (Los Reyes)"),
		"GreenSeeker Edo. México 2014.xlsx" = "Data-El Batan (Texcoco)",
		"GreenSeeker Edo. México 2015.xlsx" = "Data-El Batan (Texcoco)"
	)

	d_list <- list()
	long_list <- list()
	id0 <- 0L
	for (bn in names(books)) {
		f <- ff[basename(ff) == bn]
		yrs <- regmatches(bn, gregexpr("20[0-9]{2}", bn))[[1]]
		year <- paste(yrs, collapse="-")
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
			if (length(n_col) == 0 || is.na(n_col) || length(y_col) == 0 || is.na(y_col)) next
			n_fert <- n_rate(r[[n_col]])
			keep <- !is.na(n_fert)
			r <- r[keep, ]
			n_fert <- n_fert[keep]
			if (nrow(r) == 0) next
			pop <- if (length(h_col) == 0) rep(NA_real_, nrow(r)) else num(r[[h_col[1]]])
			if (length(h_col) >= 1 && isTRUE(grepl("m", unit[h_col[1]])) && is.finite(stats::median(pop, na.rm=TRUE)) && stats::median(pop, na.rm=TRUE) < 100) {
				pop <- pop * 10000
			}
			till <- if (is.null(meta_s)) NA_character_ else meta_s$tillage
			land <- rep(NA_character_, nrow(r))
			if (!is.na(till) && grepl("conventional", tolower(till))) land[] <- "conventional"
			irr_txt <- if (is.null(meta_s)) NA_character_ else meta_s$irrigation
			saw_rain <- FALSE
			dates <- character()
			if (length(irr_txt) > 0) {
				for (cell in irr_txt) {
					if (is.na(cell) || !nzchar(cell) || cell %in% c("-", "NA")) next
					if (grepl("rain", cell, ignore.case=TRUE)) {
						saw_rain <- TRUE
						next
					}
					dt <- parse_one(cell, dmy)
					if (!is.na(dt)) dates <- c(dates, dt)
				}
			}
			irrigated <- NA
			if (length(dates) > 0) irrigated <- TRUE
			else if (saw_rain) irrigated <- FALSE
			irrigation_dates <- if (length(dates) == 0) NA_character_ else paste(unique(dates), collapse="; ")
			p_rate <- if (is.null(meta_s)) NA_real_ else oxide_kg(meta_s$p_txt, "P2O5", 2.29)
			k_rate <- if (is.null(meta_s)) NA_real_ else oxide_kg(meta_s$k_txt, "K2O", 1.2051)
			d <- data.frame(
				crop = "maize",
				country = "Mexico",
				location = loc,
				trial_id = paste(loc, year, sep="_"),
				plot_id = as.character(r[[which(hdr == "Plot")[1]]]),
				rep = as.integer(num(r[[which(hdr == "Rep")[1]]])),
				treatment = as.character(n_fert),
				N_fertilizer = n_fert,
				P_fertilizer = p_rate,
				K_fertilizer = k_rate,
				plot_area = if (length(p_col) == 0) NA_real_ else num(r[[p_col[1]]]),
				plant_density = pop,
				yield = num(r[[y_col]]),
				yield_moisture = 14,
				yield_part = "grain",
				variety = if (is.null(meta_s)) NA_character_ else meta_s$variety,
				land_prep_method = land,
				planting_date = if (is.null(meta_s)) NA_character_ else parse_one(meta_s$planting, dmy),
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
			if (!is.null(long)) long <- long[!is.na(long$NDVI) & !is.na(long$date), ]
			d_list[[length(d_list) + 1]] <- d
			long_list[[length(long_list) + 1]] <- long
		}
	}

	d <- do.call(rbind, d_list)
	if (all(is.na(d$P_fertilizer))) d$P_fertilizer <- NULL
	if (all(is.na(d$K_fertilizer))) d$K_fertilizer <- NULL
	if (all(is.na(d$land_prep_method))) d$land_prep_method <- NULL
	if (all(is.na(d$irrigation_dates))) d$irrigation_dates <- NULL
	if (all(is.na(d$planting_date))) d$planting_date <- NULL
	long <- do.call(rbind, long_list)
	if (is.null(long) || nrow(long) == 0) {
		d$record_id <- NULL
		carobiner::write_files(path, meta, d)
	} else {
		carobiner::write_files(path, meta, d, long=long)
	}
}
