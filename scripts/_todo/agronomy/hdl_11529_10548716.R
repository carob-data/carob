# R script for "carob"
# license: GPL (>=3)

## NOTES
# Maize nitrogen-rate trials for a GreenSeeker calibration in Guerrero. The title names the state. Country is Mexico.
# The map sheet says a randomized complete block design.
# Yield is already kg/ha at 14% moisture. Nitrogen is Total N when that column exists, otherwise N at planting. Text such as 0 Kg N is 0.
# DAP is 20.1% elemental P and triple superphosphate is 19.23% elemental P, from the fertilizer vocabulary. Potassium chloride is 49.8% elemental K. A dash in the potassium cell is 0. A phosphorus rate with no product name is not converted.
# DAP also contains 18% N. That nitrogen is not added to the N treatment rate.
# Rainfed irrigation cells are stored as not irrigated. A date in an irrigation cell is an irrigation date. Drip irrigation is irrigated, with no date.
# Harvest population in plants/m2 is stored as plants/ha when the median is below 100. A plants/ha unit is left as printed.
# NDVI header cells are Excel serials or day/month/year dates. They are kept in the long table.
# The variable description does not give coordinates. The potassium rate is a dash, stored as 0.

## ISSUES
# write_files: missing variables: latitude, longitude, harvest_date. contributor:

## NOT PROCESSED
# Map sheets.
# Phenology notes in the variable description.
# Phosphorus rates that name no product.


carob_script <- function(path) {

"
Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Guerrero.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Guerrero.
"

	state <- "Guerrero"
	uri <- "hdl:11529/10548716"
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
	n_num <- function(z) {
		z <- trimws(as.character(z))
		z <- sub(" *[Kk]g.*$", "", z)
		num(z)
	}
	to_date <- function(z) {
		if (inherits(z, "Date") || inherits(z, "POSIXt")) return(as.character(as.Date(z)))
		ch <- trimws(as.character(z))
		ch[is.na(ch) | ch %in% c("", "-", "–", "NA")] <- NA_character_
		out <- rep(NA_character_, length(ch))
		iso <- !is.na(ch) & nchar(ch) >= 10 & substr(ch, 5, 5) == "-"
		out[iso] <- substr(ch[iso], 1, 10)
		sl <- !is.na(ch) & grepl("/", ch)
		plain <- !is.na(ch) & !iso & !sl & !grepl("[[:alpha:]]", ch)
		n <- rep(NA_real_, length(ch))
		n[plain] <- as.numeric(ch[plain])
		unix <- plain & !is.na(n) & n > 1e9 & n < 2e10
		excel <- plain & !is.na(n) & n > 20000 & n < 80000
		out[unix] <- as.character(as.Date(as.POSIXct(n[unix], origin="1970-01-01", tz="UTC")))
		out[excel] <- as.character(as.Date(n[excel], origin="1899-12-30"))
		if (any(sl)) {
			first <- rep(NA_real_, length(ch))
			for (i in which(sl)) {
				p <- strsplit(ch[i], "/")[[1]]
				if (length(p) != 3) next
				a <- as.numeric(p[1])
				b <- as.numeric(p[2])
				yr <- as.numeric(p[3])
				if (any(is.na(c(a, b, yr)))) next
				if (yr < 100) yr <- yr + 2000
				day <- a
				mon <- b
				if (a <= 12 && b > 12) {
					day <- b
					mon <- a
				}
				if (mon >= 1 && mon <= 12 && day >= 1 && day <= 31) {
					out[i] <- sprintf("%04d-%02d-%02d", yr, mon, day)
				}
				first[i] <- a
			}
			if (any(first > 12, na.rm=TRUE)) {
				for (i in which(sl)) {
					p <- strsplit(ch[i], "/")[[1]]
					if (length(p) != 3) next
					a <- as.numeric(p[1])
					b <- as.numeric(p[2])
					yr <- as.numeric(p[3])
					if (any(is.na(c(a, b, yr)))) next
					if (yr < 100) yr <- yr + 2000
					if (b >= 1 && b <= 12 && a >= 1 && a <= 31) out[i] <- sprintf("%04d-%02d-%02d", yr, b, a)
				}
			}
		}
		out
	}
	norm <- function(z) {
		z <- tolower(z)
		z <- chartr("áéíóúüñ", "aeiouun", z)
		gsub("[^a-z0-9]", "", z)
	}
	dms <- function(x) {
		x <- as.character(x)
		x[is.na(x)] <- ""
		nums <- regmatches(x, gregexpr("[0-9]+\\.?[0-9]*", x))
		out <- rep(NA_real_, length(x))
		for (i in seq_along(nums)) {
			p <- as.numeric(nums[[i]])
			if (!length(p)) next
			if (length(p) == 1) out[i] <- p[1]
			else if (length(p) == 2) out[i] <- p[1] + p[2] / 60
			else out[i] <- p[1] + p[2] / 60 + p[3] / 3600
		}
		neg <- grepl("S|W", x, ignore.case=TRUE)
		out[neg] <- -abs(out[neg])
		out
	}
	product_p <- function(x) {
		if (length(x) != 1 || is.na(x)) return(list(p=NA_real_, type=NA_character_))
		x <- trimws(as.character(x))
		if (!nzchar(x)) return(list(p=NA_real_, type=NA_character_))
		if (x %in% c("-", "–")) return(list(p=0, type=NA_character_))
		low <- tolower(x)
		rate <- as.numeric(trimws(sub("[Kk]g.*", "", x)))
		if (is.na(rate)) return(list(p=NA_real_, type=NA_character_))
		if (grepl("dap", low)) return(list(p=rate * 20.1 / 100, type="DAP"))
		if (grepl("triple|super fosfato|s\\.f\\.c", low)) return(list(p=rate * 19.23 / 100, type="TSP"))
		list(p=NA_real_, type=NA_character_)
	}
	product_k <- function(x) {
		if (length(x) != 1 || is.na(x)) return(list(k=NA_real_, type=NA_character_))
		x <- trimws(as.character(x))
		if (!nzchar(x)) return(list(k=NA_real_, type=NA_character_))
		if (x %in% c("-", "–")) return(list(k=0, type=NA_character_))
		low <- tolower(x)
		rate <- as.numeric(trimws(sub("[Kk]g.*", "", x)))
		if (is.na(rate)) return(list(k=NA_real_, type=NA_character_))
		if (grepl("cloruro de potasio|\\bkcl\\b", low)) return(list(k=rate * 49.8 / 100, type="KCl"))
		list(k=NA_real_, type=NA_character_)
	}
	xlsx_sheets <- function(f) {
		td <- tempfile("sheets")
		dir.create(td)
		unzip(f, files="xl/workbook.xml", exdir=td)
		xml <- paste(readLines(file.path(td, "xl", "workbook.xml"), warn=FALSE), collapse="")
		hit <- regmatches(xml, gregexpr("name=\"[^\"]*\"", xml))[[1]]
		gsub("^name=\"|\"$", "", hit)
	}
	read_blocks <- function(f, sheets) {
		s <- sheets[grepl("^Variable Description", sheets)]
		if (!length(s)) return(list())
		info <- carobiner::read.excel(f, sheet=s[1], col_names=FALSE)
		lab_cols <- c()
		for (j in seq_len(ncol(info))) {
			col <- trimws(as.character(info[[j]]))
			if (any(col == "Planting Date", na.rm=TRUE)) lab_cols <- c(lab_cols, j)
		}
		out <- list()
		for (j in lab_cols) {
			lab <- trimws(as.character(info[[j]]))
			val <- trimws(as.character(info[[j + 1]]))
			pick <- function(pat) {
				i <- grep(pat, lab, ignore.case=TRUE)
				if (!length(i)) NA_character_ else val[i[1]]
			}
			site <- NA_character_
			if (j > 1) {
				site <- trimws(as.character(info[[j - 1]][1]))
				if (is.na(site) || !nzchar(site) || grepl("^cycle$|^variable$", site, ignore.case=TRUE)) site <- NA_character_
			}
			till <- tolower(pick("^Tillage$"))
			prep <- NA_character_
			if (!is.na(till) && grepl("conventional", till)) prep <- "conventional"
			if (!is.na(till) && grepl("conservation", till)) prep <- "conservation agriculture"
			if (!is.na(till) && grepl("zero", till)) prep <- "none"
			ir_lab <- grepl("irrigation", lab, ignore.case=TRUE)
			ir_val <- val[ir_lab]
			ir_val <- ir_val[!is.na(ir_val) & nzchar(ir_val)]
			ir_dates <- to_date(ir_val)
			ir_dates <- ir_dates[!is.na(ir_dates)]
			irrigated <- NA
			if (length(ir_val) && any(grepl("rain", tolower(ir_val)))) irrigated <- FALSE
			else if (length(ir_dates) > 0) irrigated <- TRUE
			else if (length(ir_val) && any(grepl("drip", tolower(ir_val)))) irrigated <- TRUE
			pp <- product_p(pick("Phosphorus fertilizer"))
			kk <- product_k(pick("^potassium"))
			types <- c(pp$type, kk$type)
			types <- types[!is.na(types)]
			ft <- if (length(types)) paste(unique(types), collapse="; ") else NA_character_
			lat <- dms(pick("^Latitude$"))
			lon <- dms(pick("^Longitude$"))
			if (length(lon) == 1 && !is.na(lon) && lon > 0) {
				rawlon <- pick("^Longitude$")
				if (is.na(rawlon) || !grepl("E", rawlon)) lon <- -abs(lon)
			}
			out[[length(out) + 1]] <- list(
				site = site,
				variety = pick("Hibrid/Company"),
				planting_date = to_date(pick("Planting Date")),
				land_prep_method = prep,
				P_fertilizer = pp$p,
				K_fertilizer = kk$k,
				fertilizer_type = ft,
				irrigation_dates = if (length(ir_dates)) paste(ir_dates, collapse="; ") else NA_character_,
				irrigated = irrigated,
				latitude = if (length(lat)) lat[1] else NA_real_,
				longitude = if (length(lon)) lon[1] else NA_real_
			)
		}
		out
	}
	match_block <- function(loc, blocks) {
		if (!length(blocks)) return(NULL)
		if (length(blocks) == 1 && (is.na(blocks[[1]]$site) || !nzchar(blocks[[1]]$site))) return(blocks[[1]])
		nl <- norm(loc)
		best <- NULL
		best_n <- 0
		for (b in blocks) {
			ns <- norm(b$site)
			if (!nzchar(ns)) next
			if (grepl(ns, nl, fixed=TRUE) || grepl(nl, ns, fixed=TRUE)) {
				if (nchar(ns) > best_n) {
					best <- b
					best_n <- nchar(ns)
				}
			}
		}
		best
	}
	read_wth <- function(f, sheets) {
		s <- sheets[trimws(tolower(sheets)) %in% c("wheather", "weather")]
		if (!length(s)) return(NULL)
		raw <- carobiner::read.excel(f, sheet=s[1], col_names=FALSE)
		w <- NULL
		for (i in seq_len(nrow(raw))) {
			cells <- tolower(trimws(as.character(unlist(raw[i, ]))))
			cells[is.na(cells)] <- ""
			at <- which(cells %in% c("fecha", "date") | grepl("^temp", cells))
			if (!length(at)) next
			date_cols <- which(cells %in% c("fecha", "date"))
			if (!length(date_cols) && any(grepl("^temp", cells))) date_cols <- 1
			for (id in date_cols) {
				right <- cells[id:length(cells)]
				ix <- id - 1 + which(grepl("max", right))[1]
				in_ <- id - 1 + which(grepl("min", right))[1]
				if (is.na(ix) || is.na(in_)) next
				if (i == nrow(raw)) next
				body <- raw[(i + 1):nrow(raw), ]
				date <- to_date(body[[id]])
				ok <- !is.na(date)
				if (!any(ok)) next
				station <- NA_character_
				if (i > 1) {
					above <- trimws(as.character(raw[[id]][seq_len(i - 1)]))
					above <- above[!is.na(above) & nzchar(above)]
					if (length(above)) station <- above[length(above)]
				}
				part <- data.frame(
					station_name = if (is.na(station)) state else station,
					date = date[ok],
					tmax = num(body[[ix]][ok]),
					tmin = num(body[[in_]][ok])
				)
				w <- if (is.null(w)) part else rbind(w, part)
			}
			if (any(cells %in% c("fecha", "date"))) break
		}
		w
	}

	read_sheet <- function(f, sheet, blocks, year) {
		raw <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		hdr <- trimws(as.character(unlist(raw[1, ])))
		hdr[is.na(hdr)] <- ""
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
		nrate <- n_num(body[[i_n]])
		keep <- !is.na(nrate)
		if (!any(keep)) return(NULL)
		body <- body[keep, ]
		nrate <- nrate[keep]
		yield <- num(body[[i_y]])
		pop <- if (is.na(i_pop)) rep(NA_real_, length(nrate)) else num(body[[i_pop]])
		upop <- if (is.na(i_pop)) "" else unit[i_pop]
		if (!is.na(upop) && grepl("ha", upop, ignore.case=TRUE)) {
			pop <- pop
		} else if (length(pop) && is.finite(stats::median(pop, na.rm=TRUE)) && stats::median(pop, na.rm=TRUE) < 100) {
			pop <- pop * 10000
		}
		loc <- trimws(sub("^Data[- ]*", "", trimws(sheet)))
		if (!nzchar(loc)) loc <- state
		loc <- carobiner::fix_name(loc, "title")
		info <- match_block(loc, blocks)
		use <- !is.null(info)
		lat <- if (use) info$latitude else NA_real_
		lon <- if (use) info$longitude else NA_real_
		if (length(lat) != 1) lat <- NA_real_
		if (length(lon) != 1) lon <- NA_real_
		d <- data.frame(
			country = "Mexico",
			adm1 = state,
			location = loc,
			trial_id = paste(loc, year),
			plot_id = if (is.na(i_plot)) NA_character_ else trimws(as.character(body[[i_plot]])),
			rep = if (is.na(i_rep)) NA_integer_ else as.integer(num(body[[i_rep]])),
			crop = "maize",
			treatment = as.character(nrate),
			N_fertilizer = nrate,
			P_fertilizer = if (use) info$P_fertilizer else NA_real_,
			K_fertilizer = if (use) info$K_fertilizer else NA_real_,
			fertilizer_type = if (use) info$fertilizer_type else NA_character_,
			plot_area = if (is.na(i_area)) NA_real_ else num(body[[i_area]]),
			plant_density = pop,
			yield = yield,
			yield_moisture = 14,
			yield_part = "grain",
			variety = if (use) info$variety else NA_character_,
			planting_date = if (use) info$planting_date else NA_character_,
			land_prep_method = if (use) info$land_prep_method else NA_character_,
			irrigation_dates = if (use) info$irrigation_dates else NA_character_,
			irrigated = if (use) info$irrigated else NA,
			latitude = lat,
			longitude = lon,
			on_farm = NA,
			is_survey = FALSE,
			geo_from_source = !is.na(lat) & !is.na(lon)
		)
		ndvi_cols <- which(hdr == "NDVI")
		long <- NULL
		if (length(ndvi_cols)) {
			dates <- to_date(unit[ndvi_cols])
			for (i in seq_along(ndvi_cols)) {
				val <- num(body[[ndvi_cols[i]]])
				part <- data.frame(row = seq_along(val), date = dates[i], NDVI = val)
				long <- if (is.null(long)) part else rbind(long, part)
			}
			long <- long[!is.na(long$NDVI) & !is.na(long$date), ]
		}
		list(d=d, long=long)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- NULL
	long <- NULL
	wth <- NULL
	for (f in ff) {
		sheets <- xlsx_sheets(f)
		if (!any(grepl("^Data", sheets))) next
		blocks <- read_blocks(f, sheets)
		yrs <- regmatches(basename(f), gregexpr("[0-9]{4}", basename(f)))[[1]]
		year <- if (length(yrs) >= 2) paste(yrs[1], yrs[2], sep="-") else yrs[1]
		data_sheets <- sheets[grepl("^Data", sheets)]
		for (s in data_sheets) {
			got <- read_sheet(f, s, blocks, year)
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
	if (all(is.na(d$fertilizer_type))) d$fertilizer_type <- NULL
	if (all(is.na(d$latitude)) || all(is.na(d$longitude))) {
		d$latitude <- NULL
		d$longitude <- NULL
		d$geo_from_source <- FALSE
	}
	d$record_id <- as.integer(seq_len(nrow(d)))
	if (!is.null(long) && nrow(long)) {
		long$record_id <- as.integer(long$row)
		long$row <- NULL
	} else {
		long <- NULL
	}
	if (is.null(long) && is.null(wth)) carobiner::write_files(path, meta, d)
	else if (is.null(wth)) carobiner::write_files(path, meta, d, long=long)
	else if (is.null(long)) carobiner::write_files(path, meta, d, wth=wth)
	else carobiner::write_files(path, meta, d, long=long, wth=wth)
}
