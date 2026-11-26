# R script for "carob"
# license: GPL (>=3)

## NOTES
# Variety means are in the "-1" workbooks. Grain yield is already kg/ha.
# The matching workbook without "-1" is an empty fieldbook and is not used.
# 05CHTTW6-1.xls (Agua Fria) has no yield column. The locations sheet says "Without Grain Yield".
# Coordinates are degrees and minutes. South and west are negative. Excel dates use origin 1899-12-30.
# Philipines in the locations sheet is stored as Philippines.
# Disease and pest scores are in the long table. A 1-5 unit is severity. A percent unit is incidence.

## ISSUES
# write_files: missing variables: irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: ears_per_plant_. NA detected: yield. invalid terms in long table (disease/pest_species not yet in terminag): aspergillus flavus, corn stunt, sclerospora macrospora, sheath blight, smut, tar spot, maize weevil.
# El Parque, Los Chiles (Costa Rica, 380 m — Pérez Zeledón) has blank minutes in the locations sheet (9° N, 84° W); assigned 9.33 N, -83.68 W (geo_from_source = FALSE).
# Kabacan, Cotabato (Philippines) is printed as 7°6' N, 114°50' E (South China Sea — 1-digit typo for 124°50' E); corrected to 124.8333 E (geo_from_source = FALSE).
# The two Kabacan sets share one coordinate pair.
# Agua Fria set 6 has scores and no grain yield.
# Fusarium mapped to "fusarium spp" and pyrausta nubilalis mapped to "stem borer". Other diseases/pests not yet in terminag are kept in long$disease / long$pest_species.


## NEW VARIABLES
# ears_per_plant_: ears per plant, a ratio.

## NOT PROCESSED
# Percent of the best check.
# Plants harvested and plants at germination.
# Plant aspect, ear aspect, husk cover, grain texture, and anthesis-silking interval.
# Unspecified leaf-disease and plant-health scores.
# 05CHTTW-Combined.xls and the fieldbook workbooks that are not the "-1" means.
# 05CHTTW10.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW18.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW19.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW20.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial (CHTTW  - 2005). Observation records are missing from d until this file is standardized.
# 05CHTTW23.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW - 2005). Observation records are missing from d until this file is standardized.
# 05CHTTW24.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005), ...7, ...8. Observation records are missing from d until this file is standardized.
# 05CHTTW25.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW26.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW27.xls: not read. sheet Fieldbook (2) (of 6): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW28.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW32.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW38.xls: not read. sheet Fieldbook (2) (of 6): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial. Observation records are missing from d until this file is standardized.
# 05CHTTW5.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW7.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW8.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.
# 05CHTTW9.xls: not read. sheet Fieldbook (of 5): 2005 CHTTW  (ILWH0505), ...2, ...3, ...4, ...5, ...6, ...7, CIMMYT Hybrid Trial Tropical White Normal and QPM Trial  (CHTTW  -  2005). Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
International Late White Hybrid Trial - ILWH0505

Summary results and individual trial results from the International Late White Hybrid - ILWH, (Elite Tropical Late White Normal and QPM Hybrid Trial - CHTTW) conducted in 2005.
"

	uri <- "hdl:11529/10572"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late White Hybrid Trial - ILWH0505",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 68,
		carob_effort = 0
	)

	num <- function(x) {
		y <- trimws(as.character(x))
		y[is.na(y) | y %in% c("", "NA", "- - -", "-", ".")] <- NA_character_
		y[!is.na(y) & grepl("[A-Za-z]", y)] <- NA_character_
		as.numeric(y)
	}
	piece <- function(x) {
		y <- trimws(as.character(x))
		y[is.na(y) | y %in% c("", "NA", "- - -")] <- NA_character_
		y <- gsub("o", "", y)
		y <- gsub("°", "", y)
		y <- gsub("'", "", y)
		y <- trimws(y)
		y[!is.na(y) & y %in% c("", "-")] <- NA_character_
		as.numeric(y)
	}
	excel_date <- function(x) {
		n <- num(x)
		as.character(as.Date(n, origin="1899-12-30"))
	}
	fix_country <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | x %in% c("", "NA")] <- NA_character_
		ok <- !is.na(x)
		if (any(ok)) x[ok] <- carobiner::fix_name(x[ok], "title")
		x[x == "Viet Nam"] <- "Vietnam"
		x[x == "Philipines"] <- "Philippines"
		x[x == "México"] <- "Mexico"
		x
	}
	blank <- function(x) {
		x <- as.character(x)
		x[is.na(x)] <- ""
		trimws(x)
	}

	f1 <- ff[basename(ff) == "05CHTTW-Locations.xls"]
	r1 <- carobiner::read.excel(f1, col_names=FALSE, col_types="text")
	c2 <- blank(r1[[2]])
	hdr <- which(c2 == "Country")[1]
	sites <- r1[seq_len(nrow(r1)) > hdr, ]
	set_no <- num(sites[[4]])
	sites <- sites[!is.na(set_no), ]
	set_no <- set_no[!is.na(set_no)]
	lat <- piece(sites[[5]]) + piece(sites[[6]]) / 60
	lon <- piece(sites[[8]]) + piece(sites[[9]]) / 60
	ns <- toupper(blank(sites[[7]]))
	we <- toupper(blank(sites[[10]]))
	lat[ns == "S"] <- -abs(lat[ns == "S"])
	lon[we == "W"] <- -abs(lon[we == "W"])
	# A missing minute is not zero.
	lat[is.na(piece(sites[[6]]))] <- NA_real_
	lon[is.na(piece(sites[[9]]))] <- NA_real_
	sites <- data.frame(
		set_no = set_no,
		country = fix_country(sites[[2]]),
		location = trimws(as.character(sites[[3]])),
		latitude = lat,
		longitude = lon,
		elevation = num(sites[[11]]),
		planting_date = excel_date(sites[[12]]),
		harvest_date = excel_date(sites[[13]]),
		geo_from_source = !is.na(lat) & !is.na(lon)
	)
	ok <- !is.na(sites$location) & nzchar(sites$location)
	if (any(ok)) sites$location[ok] <- carobiner::fix_name(sites$location[ok], "title")
	# Kabacan, Cotabato (Philippines): printed 114°50' E (ocean) instead of 124°50' E
	i_kab <- grepl("Kabacan", sites$location, ignore.case = TRUE)
	sites$longitude[i_kab] <- 124 + 50 / 60
	sites$geo_from_source[i_kab] <- FALSE
	# El Parque, Los Chiles (Pérez Zeledón, Costa Rica, 380 m): minutes missing in source
	i_ch <- grepl("Los Chiles", sites$location, ignore.case = TRUE)
	sites$latitude[i_ch] <- 9.33
	sites$longitude[i_ch] <- -83.68
	sites$geo_from_source[i_ch] <- FALSE

	one <- function(f) {
		r <- carobiner::read.excel(f, sheet="Results", col_names=FALSE, col_types="text")
		first <- blank(r[[1]])
		h <- which(first %in% c("Ent.", "No. Entr", "Entr No."))[1]
		multi <- identical(first[h], "Ent.")
		if (multi) {
			a <- blank(unlist(r[h, ]))
			b <- blank(unlist(r[h + 1, ]))
			unit <- blank(unlist(r[h + 2, ]))
			name <- trimws(paste(a, b))
			start <- h + 3
		} else {
			name <- blank(unlist(r[h, ]))
			unit <- name
			start <- h + 1
		}
		sub <- r[start:nrow(r), ]
		ent <- blank(sub[[1]])
		ent[ent %in% c("Mean", "Maximum", "Minimum", "LSD", "LSD (0.05)", "CV", "MSe", "p", "Checks (Testigos)", "Check Means")] <- ""
		ent[grepl("[A-Za-z]", ent)] <- ""
		keep <- ent != ""
		sub <- sub[keep, ]
		ent <- ent[keep]
		n <- nrow(sub)
		find_col <- function(pat, exclude=NULL) {
			i <- grep(pat, name, ignore.case=TRUE)
			if (!is.null(exclude) && length(i)) i <- i[!grepl(exclude, name[i], ignore.case=TRUE)]
			if (!length(i)) return(NA_integer_)
			i[1]
		}
		pull <- function(i) {
			if (is.na(i)) return(rep(NA_real_, n))
			num(sub[[i]])
		}
		tagged <- function(i, suffix) {
			v <- pull(i)
			out <- rep(NA_character_, n)
			ok <- !is.na(v)
			out[ok] <- paste0(v[ok], suffix)
			out
		}
		iy <- find_col("Grain Yield|GrainYield")
		scale <- 1
		if (!is.na(iy) && grepl("t/ha", unit[iy], ignore.case=TRUE) && !grepl("kg/ha", unit[iy], ignore.case=TRUE)) scale <- 1000
		iv <- find_col("Genealogia|Pedigree")
		if (is.na(iv)) iv <- find_col("^Name$")
		variety <- if (is.na(iv)) rep(NA_character_, n) else trimws(as.character(sub[[iv]]))
		variety[variety %in% c("", "NA")] <- NA_character_
		loc_cell <- blank(unlist(r[2, ]))
		country_src <- loc_cell[grepl("^Country:", loc_cell)]
		location_src <- loc_cell[grepl("^Location:", loc_cell)]
		country_src <- if (length(country_src)) sub("^Country:\\s*", "", country_src[1]) else NA_character_
		location_src <- if (length(location_src)) sub("^Location:\\s*", "", location_src[1]) else NA_character_
		ih <- find_col("Plnt ht|Plt\\.? Hght|PlantHeight", "Asp|Harv|aspect")
		ie <- find_col("Ear ht|Ear Hght|EarHeight", "Asp|aspect|Rot|Position")
		ir <- find_col("Root Lodg|Rt Ldg|RootLodging")
		is <- find_col("Stem Lodg|Stalk Lodg|Stlk Ldg|StemLodging")
		iepp <- find_col("Ears/|EPPNo|EPP")
		d <- data.frame(
			set_no = as.integer(sub("^.*?([0-9]+)$", "\\1", sub("(-1)?\\.xls$", "", basename(f), ignore.case=TRUE))),
			entry_no = as.integer(ent),
			country_src = fix_country(country_src),
			location_src = trimws(location_src),
			variety = variety,
			yield = pull(iy) * scale,
			anthesis_days = pull(find_col("Pollen|Anthesis|Flwr.*Male")),
			silking_days = pull(find_col("Silk|Flwr.*Fem|DaysToSilk")),
			plant_height = pull(ih),
			ear_height = pull(ie),
			yield_moisture = pull(find_col("^Moist|GrainMoisture")),
			root_lodging = tagged(ir, "%"),
			stalk_lodging = tagged(is, "%"),
			ears_per_plant_ = pull(iepp)
		)
		specs <- rbind(
			c("bipolaris", "southern leaf blight", "disease"),
			c("rhizoc", "rhizoctonia", "disease"),
			c("turcicum|helm", "maize leaf blight", "disease"),
			c("fusa", "fusarium spp", "disease"),
			c("head smut", "head smut", "disease"),
			c("smut", "smut", "disease"),
			c("ear rot", "ear rot", "disease"),
			c("southern rust", "rust", "disease"),
			c("\\bgls\\b|gray leaf", "gray leaf spot", "disease"),
			c("\\bmsv\\b|streak", "maize streak virus", "disease"),
			c("sheath blight", "sheath blight", "disease"),
			c("sclerop", "sclerospora macrospora", "disease"),
			c("stunt", "corn stunt", "disease"),
			c("tar\\s*spot|tarspot", "tar spot", "disease"),
			c("flav|aspergillus", "aspergillus flavus", "disease"),
			c("pyrausta", "stem borer", "pest"),
			c("borer", "stem borer", "pest"),
			c("weevil", "maize weevil", "pest")
		)
		used <- c(iy, iv, ih, ie, ir, is, iepp, find_col("Pollen|Anthesis|Flwr.*Male"), find_col("Silk|Flwr.*Fem|DaysToSilk"), find_col("^Moist|GrainMoisture"))
		used <- used[!is.na(used)]
		long <- NULL
		for (k in seq_len(nrow(specs))) {
			i <- grep(specs[k, 1], name, ignore.case=TRUE)
			i <- setdiff(i, used)
			if (!length(i)) next
			i <- i[1]
			used <- c(used, i)
			v <- pull(i)
			if (all(is.na(v))) next
			score <- grepl("1-5|1_5", paste(name[i], unit[i]))
			txtv <- rep(NA_character_, n)
			ok <- !is.na(v)
			if (score) txtv[ok] <- paste0(v[ok], " (1-5)")
			else txtv[ok] <- paste0(v[ok], "%")
			row <- data.frame(
				set_no = d$set_no,
				entry_no = d$entry_no,
				disease = if (specs[k, 3] == "disease") specs[k, 2] else NA_character_,
				disease_incidence = if (specs[k, 3] == "disease" && !score) txtv else NA_character_,
				disease_severity = if (specs[k, 3] == "disease" && score) txtv else NA_character_,
				pest_species = if (specs[k, 3] == "pest") specs[k, 2] else NA_character_,
				pest_severity = if (specs[k, 3] == "pest") txtv else NA_character_
			)
			row <- row[ok, ]
			long <- if (is.null(long)) row else rbind(long, row)
		}
		d <- d[!is.na(d$variety), ]
		list(d=d, long=long)
	}

	f2 <- ff[basename(ff) %in% c(
		"05CHTTW10-1.xls",
		"05CHTTW18-1.xls",
		"05CHTTW19-1.xls",
		"05CHTTW20-1.xls",
		"05CHTTW23-1.xls",
		"05CHTTW24-1.xls",
		"05CHTTW25-1.xls",
		"05CHTTW26-1.xls",
		"05CHTTW27-1.xls",
		"05CHTTW28-1.xls",
		"05CHTTW32-1.xls",
		"05CHTTW38-1.xls",
		"05CHTTW5-1.xls",
		"05CHTTW6-1.xls",
		"05CHTTW7-1.xls",
		"05CHTTW8-1.xls",
		"05CHTTW9-1.xls"
	)]
	parts <- lapply(f2, one)
	d <- do.call(rbind, lapply(parts, function(x) x$d))
	long <- do.call(rbind, lapply(parts, function(x) x$long))
	d <- merge(d, sites, by="set_no", all.x=TRUE)
	miss <- is.na(d$country)
	d$country[miss] <- d$country_src[miss]
	miss <- is.na(d$location) | d$location == ""
	d$location[miss] <- d$location_src[miss]
	ok <- !is.na(d$location) & nzchar(d$location)
	if (any(ok)) d$location[ok] <- carobiner::fix_name(d$location[ok], "title")
	d$country_src <- NULL
	d$location_src <- NULL
	d$treatment <- d$variety
	d$crop <- "maize"
	d$yield_part <- "grain"
	loclab <- d$location
	loclab[is.na(loclab)] <- ""
	d$trial_id <- trimws(paste(d$set_no, loclab))
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$record_id <- as.integer(seq_len(nrow(d)))
	if (!is.null(long)) {
		long <- merge(long, d[, c("set_no", "entry_no", "record_id")], by=c("set_no", "entry_no"))
		long$set_no <- NULL
		long$entry_no <- NULL
	}
	d$location <- gsub("(portuguesa)", "(Portuguesa)", d$location, fixed=TRUE)
	d$set_no <- NULL
	d$entry_no <- NULL
	drop_na <- c("anthesis_days", "silking_days", "plant_height", "ear_height", "yield_moisture", "root_lodging", "stalk_lodging", "ears_per_plant_", "elevation")
	for (nm in drop_na) {
		if (nm %in% names(d) && all(is.na(d[[nm]]))) d[[nm]] <- NULL
	}

	if (is.null(long) || nrow(long) == 0) {
		carobiner::write_files(path, meta, d)
	} else {
		carobiner::write_files(path, meta, d, long=long)
	}
}
