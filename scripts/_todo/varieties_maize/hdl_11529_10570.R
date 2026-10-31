# R script for "carob"
# license: GPL (>=3)

## NOTES
# International hybrid trial. Coordinates and dates are in the Locations workbook.
# Degrees and minutes are converted to decimal degrees. West and south are negative.
# México in the locations sheet is recorded as Mexico.
# Grain yield is the field-weight column, tonnes per hectare, converted to kg/ha.
# Where a site has both a workbook and a "-1" workbook, the "-1" file is used.
# Summary workbooks are not used.

## ISSUES
# write_files: missing variables: on_farm, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.


## NOT PROCESSED
# Combined results, grain-yield summary, and the dendrogram.
# Lodging, husk cover, ear rot, disease scores, and plant stand.
# Anthesis date and anthesis-silking interval.


carob_script <- function(path) {

"
International Intermediate Yellow Hybrid Trial - IIYH0653

Summary results and individual trial results from the International Intermediate Yellow Hybrid - IIYH, (Highland Intermediate Yellow Hybrids - CHTHIY) conducted in 2006.
"

	uri <- "hdl:11529/10570"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Intermediate Yellow Hybrid",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.5
	)

	dms <- function(deg, minute, hemi) {
		y <- gsub("o", "", as.character(deg))
		y <- gsub("°", "", y)
		y <- gsub("º", "", y)
		y <- gsub("'", "", y)
		m <- gsub("'", "", as.character(minute))
		x <- as.numeric(y) + as.numeric(m) / 60
		h <- toupper(trimws(as.character(hemi)))
		x[h %in% c("S", "W", "O")] <- -x[h %in% c("S", "W", "O")]
		x
	}

	floc <- ff[grepl("Locations", basename(ff))]
	m <- carobiner::read.excel(floc, col_names=FALSE)
	hdr <- which(apply(m, 1, function(x) any(x == "Country")))[1]
	labs <- as.character(unlist(m[hdr, ]))
	icountry <- which(labs == "Country")[1]
	iloc <- which(labs == "Location")[1]
	iset <- which(labs == "Set no.")[1]
	ilat <- which(labs == "Latitude")[1]
	ilon <- which(labs == "Longitude")[1]
	ialt <- which(labs == "Masl")[1]
	dates <- which(labs == "Date")
	iplant <- dates[1]
	iharv <- dates[2]
	setno <- trimws(as.character(m[[iset]]))
	ok <- seq_len(nrow(m)) > hdr & !is.na(setno) & setno != "" & setno != "Set no."
	mm <- m[ok, ]
	country <- as.character(mm[[icountry]])
	country[country == "México"] <- "Mexico"
	blank_num <- function(x) {
		y <- trimws(as.character(x))
		y[y == ""] <- NA_character_
		as.numeric(y)
	}
	loc <- data.frame(
		site = trimws(as.character(mm[[iset]])),
		country = country,
		location = gsub("México", "Mexico", as.character(mm[[iloc]])),
		latitude = dms(mm[[ilat]], mm[[ilat + 1]], mm[[ilat + 2]]),
		longitude = dms(mm[[ilon]], mm[[ilon + 1]], mm[[ilon + 2]]),
		elevation = blank_num(mm[[ialt]]),
		planting_date = as.character(as.Date(blank_num(mm[[iplant]]), origin="1899-12-30")),
		harvest_date = as.character(as.Date(blank_num(mm[[iharv]]), origin="1899-12-30"))
	)

	xls <- ff[grepl("(?i)\\.xls$", basename(ff))]
	xls <- xls[!grepl("Combined|Grain Yield|Locations", basename(xls))]
	dash <- grepl("-1\\.xls$", basename(xls))
	stem <- sub("-1\\.xls$", ".xls", basename(xls))
	xls <- xls[!(stem %in% stem[dash] & !dash)]

	one <- function(f) {
		r <- carobiner::read.excel(f)
		ent <- trimws(as.character(r$Entry))
		stats <- c("ENT", "NumberFormat", "Count", "TrialMean", "TrialSeries:", "Site:", "Location:", "Country:", "Entry", "Abb", "Mean", "LSD (0.05)", "MSe", "CV", "p", "Min", "Max", "NumSignificantSites", "Error df", "NumReps", "Correlation(AugmentedDesign)", "TrialMeanExcludingChecks", "MeanOfChecks", "StandardError", "Lower Limit", "Upper Limit", "SitesUsedForComputingMean", "SignificantSites", "SelectionDirection", "SEMin", "SEMax", "SelectionWeight", "AvgOfSelectedFraction", "SEMoved")
		ent[ent %in% stats] <- NA_character_
		keep <- !is.na(ent)
		r <- r[keep, ]
		ent <- ent[keep]
		variety <- as.character(r$BreedersPedigree1)
		name <- as.character(r$Name)
		miss <- is.na(variety) | variety == ""
		variety[miss] <- name[miss]
		site <- sub("\\.xls$", "", basename(f))
		site <- sub("-1$", "", site)
		site <- sub("^.*CHTHIY", "", site)
		data.frame(
			site = site,
			variety = variety,
			yield = as.numeric(r$GrainYieldTons_FieldWt) * 1000,
			yield_moisture = as.numeric(r$GrainMoisturePer),
			plant_height = as.numeric(r$PlantHeightCm),
			ear_height = as.numeric(r$EarHeightCm),
			geo_from_source = TRUE
		)
	}

	d <- do.call(rbind, lapply(xls, one))
	d <- merge(d, loc, by="site", all.x=TRUE)
	d$trial_id <- d$site
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$is_survey <- FALSE
	d$site <- NULL
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(path, meta, d)
}
