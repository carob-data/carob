# R script for "carob"
# license: GPL (>=3)

## NOTES
# Each location workbook is a variety-mean table. Grain yield is labeled kg/ha.
# Days to pollen is stored as anthesis_days. Moisture is percent.
# Site coordinates, elevation, and dates come from 05CHTHIW-Locations.xls.
# Latitude and longitude are degrees and minutes with a hemisphere letter.
# Planting and harvest dates in that sheet are Excel serials (origin 1899-12-30).
# Root lodging, fusarium, and smut are percent. Fusarium and smut go in the long table.

## ISSUES
# write_files: missing variables: K_fertilizer, N_fertilizer, P_fertilizer. datespan: 11 harvest_date more than 366 days after planting_date. duplicate coordinates: fewer coordinates than locations. NA detected: yield. invalid terms: disease: head smut, smut.
# 05CHTHIW15.xls has no grain-yield column. Lodging is a percent in some locations and a 1-5 score in others. The scale is kept on the value.
# 05CHTHIW31.xls is not in the locations sheet; row 2 of the workbook gives Location: Kitale, Country: Kenya (assigned KALRO Kitale coordinates 1.017 N, 35.000 E, geo_from_source = FALSE). San Jose Iturbide, Gto. (set 1, Mexico) has no coordinates in the locations sheet (assigned 21.000 N, -100.383 W, geo_from_source = FALSE).
# Fusarium mapped to "fusarium spp"; smut and head smut (not yet in values_disease.csv) are kept in long$disease.
## NOT PROCESSED
# Percent of the best check. It is computed from yield.
# Plants harvested, a count with no plot area.
# 05CHTHIW-Combined.xls, which repeats the location means.
# Cooperator names.


carob_script <- function(path) {

"
International Intermediate White Hybrid trial - IIWH0514

Summary results and individual trial results from the International Intermediate White Hybrid - IIWH, (Highland Intermediate White Hybrid - CHTHIW) conducted in 2005.
"

	uri <- "hdl:11529/10596"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Intermediate White Hybrid trial - IIWH0514",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 72,
		carob_effort = 0
	)

	excel_date <- function(x) {
		if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(as.Date(x)))
		x <- trimws(as.character(x))
		x[x %in% c("", "- - -", "NA")] <- NA_character_
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}
	num_piece <- function(x) {
		x <- gsub("o|'", "", as.character(x))
		x <- trimws(x)
		x[x %in% c("", "- - -", "NA")] <- NA_character_
		as.numeric(x)
	}

	f1 <- ff[basename(ff) == "05CHTHIW-Locations.xls"]
	r1 <- carobiner::read.excel(f1, col_names=FALSE, na=c("", "- - -"))
	c2 <- trimws(as.character(r1[[2]]))
	keep <- !is.na(c2) & !c2 %in% c("Country", "País", "Pais") & nzchar(c2)
	r1 <- r1[keep, ]
	lat <- num_piece(r1[[5]]) + num_piece(r1[[6]]) / 60
	lon <- num_piece(r1[[8]]) + num_piece(r1[[9]]) / 60
	ns <- toupper(trimws(as.character(r1[[7]])))
	we <- toupper(trimws(as.character(r1[[10]])))
	i <- which(ns == "S")
	lat[i] <- -abs(lat[i])
	i <- which(we == "W")
	lon[i] <- -abs(lon[i])
	locs <- data.frame(
		set_no = as.integer(r1[[4]]),
		country = carobiner::fix_name(trimws(as.character(r1[[2]])), "title"),
		location = carobiner::fix_name(trimws(as.character(r1[[3]])), "title"),
		latitude = lat,
		longitude = lon,
		elevation = as.numeric(r1[[11]]),
		planting_date = excel_date(r1[[12]]),
		harvest_date = excel_date(r1[[13]]),
		geo_from_source = !is.na(lat) & !is.na(lon)
	)
	locs$country[locs$country == "México"] <- "Mexico"
	# San Jose Iturbide, Gto. (Mexico) has blank coordinates in locations sheet
	i_sji <- grepl("San Jose Iturbide", locs$location, ignore.case = TRUE)
	locs$latitude[i_sji] <- 21.000
	locs$longitude[i_sji] <- -100.383
	locs$geo_from_source[i_sji] <- FALSE

	read_trial <- function(f) {
		r <- carobiner::read.excel(f, col_names=FALSE)
		blank <- function(x) {
			x <- as.character(x)
			x[is.na(x)] <- ""
			trimws(x)
		}
		nm <- trimws(paste(blank(unlist(r[5, ])), blank(unlist(r[6, ]))))
		unit <- blank(unlist(r[7, ]))
		sub <- r[9:nrow(r), ]
		lab <- trimws(as.character(sub[[2]]))
		end <- which(lab %in% c("Mean", "Maximum", "Minimum"))[1]
		if (!is.na(end)) sub <- sub[seq_len(end - 1), ]
		lab <- trimws(as.character(sub[[2]]))
		sub <- sub[!is.na(lab) & nzchar(lab) & !lab %in% c("Checks (Testigos)", "Check Means"), ]
		n <- nrow(sub)
		find <- function(pat) {
			i <- grep(pat, nm, ignore.case=TRUE)
			if (length(i) == 0) return(NA_integer_)
			i[1]
		}
		pull <- function(i) {
			if (is.na(i)) return(rep(NA_real_, n))
			as.numeric(sub[[i]])
		}
		as_text <- function(i) {
			if (is.na(i)) return(rep(NA_character_, n))
			v <- as.numeric(sub[[i]])
			out <- rep(NA_character_, n)
			ok <- !is.na(v)
			if (grepl("%", unit[i])) out[ok] <- paste0(v[ok], "%")
			else if (grepl("1-5", unit[i])) out[ok] <- paste0(v[ok], " (1-5)")
			else out[ok] <- as.character(v[ok])
			out
		}
		row2 <- blank(unlist(r[2, ]))
		loc_src <- row2[grepl("^Location:", row2)]
		cty_src <- row2[grepl("^Country:", row2)]
		loc_src <- if (length(loc_src)) trimws(sub("^Location:\\s*", "", loc_src[1])) else NA_character_
		cty_src <- if (length(cty_src)) trimws(sub("^Country:\\s*", "", cty_src[1])) else NA_character_
		iy <- find("Yield")
		if (!is.na(iy) && !grepl("Kg/ha", unit[iy])) iy <- NA_integer_
		id <- sub("-1$", "", sub("\\.xls$", "", basename(f), ignore.case=TRUE))
		data.frame(
			set_no = as.integer(sub("^05CHTHIW", "", id)),
			country_src = cty_src,
			location_src = loc_src,
			variety_code = as.character(sub[[1]]),
			variety = trimws(as.character(sub[[2]])),
			yield = pull(iy),
			anthesis_days = pull(find("Pollen")),
			silking_days = pull(find("Silk")),
			plant_height = pull(find("Plnt ht")),
			ear_height = pull(find("Ear ht")),
			yield_moisture = pull(find("^Moist")),
			root_lodging = as_text(find("Root Lodg")),
			stalk_lodging = as_text(find("Stalk Lodg|Stem Lodg")),
			fusarium_txt = as_text(find("Fusa")),
			smut_txt = as_text(find("Smut")),
			ear_rot_txt = as_text(find("Ear Rot")),
			smut_name = if (any(grepl("Head Smut", nm))) "head smut" else "smut"
		)
	}

	f2 <- ff[basename(ff) %in% c(
		"05CHTHIW1.xls",
		"05CHTHIW10.xls",
		"05CHTHIW15.xls",
		"05CHTHIW16.xls",
		"05CHTHIW17.xls",
		"05CHTHIW20-1.xls",
		"05CHTHIW22.xls",
		"05CHTHIW23.xls",
		"05CHTHIW24.xls",
		"05CHTHIW27.xls",
		"05CHTHIW31.xls",
		"05CHTHIW32.xls",
		"05CHTHIW8.xls"
	)]
	d <- do.call(rbind, lapply(f2, read_trial))
	d <- merge(d, locs, by="set_no", all.x=TRUE)
	miss_c <- is.na(d$country)
	d$country[miss_c] <- d$country_src[miss_c]
	miss_l <- is.na(d$location)
	d$location[miss_l] <- d$location_src[miss_l]
	# Set 31 (Kitale, Kenya) is absent from Locations sheet
	i_kit <- !is.na(d$location) & d$location == "Kitale" & is.na(d$latitude)
	d$latitude[i_kit] <- 1.017
	d$longitude[i_kit] <- 35.000
	d$geo_from_source[i_kit] <- FALSE
	d$country_src <- NULL
	d$location_src <- NULL
	d$crop <- "maize"
	d$treatment <- d$variety
	d$yield_part <- "grain"
	d$trial_id <- paste(d$set_no, d$location, sep="_")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$record_id <- seq_len(nrow(d))

	one_dis <- function(txt, disease) {
		keep <- !is.na(txt) & nzchar(txt)
		if (!any(keep)) return(NULL)
		inc <- txt[keep]
		is_pct <- grepl("%$", inc)
		data.frame(
			record_id = d$record_id[keep],
			disease = disease[keep],
			disease_incidence = ifelse(is_pct, inc, NA_character_),
			disease_severity = ifelse(is_pct, NA_character_, inc)
		)
	}
	long <- rbind(
		one_dis(d$fusarium_txt, rep("fusarium spp", nrow(d))),
		one_dis(d$smut_txt, d$smut_name),
		one_dis(d$ear_rot_txt, rep("ear rot", nrow(d)))
	)
	d$fusarium_txt <- NULL
	d$smut_txt <- NULL
	d$ear_rot_txt <- NULL
	d$smut_name <- NULL
	d$set_no <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
