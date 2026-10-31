# R script for "carob"
# license: GPL (>=3)

## NOTES
# Variety means are in the -1 workbooks. GrainYieldTons_FieldWt is t/ha, stored as kg/ha.
# The unit row labels anthesis as days, so AnthesisDate is anthesis_days.
# Country, location, elevation, coordinates, and planting and harvest dates come from 06EVT16A-Locations.xls.
# Latitude and longitude are degrees and minutes with a hemisphere letter. geo_from_source is TRUE.
# Planting and harvest dates in that sheet are Excel serials (origin 1899-12-30).

## ISSUES
# India sites R&D Wadadhe and R&D (Pachora) record 83 minutes of latitude. Minutes above 59 were still converted as degrees plus minutes/60. Those two sites share one coordinate pair.
# Xocoyucan harvest serial 39780 is 947 days after planting. It is stored as written.
# Bako has no harvest date. Some variety means have no grain yield.
# write_files: missing variables: K_fertilizer, N_fertilizer, P_fertilizer. datespan: 17 harvest_date more than 366 days after planting_date. duplicate coordinates: fewer coordinates than locations. NA detected: yield.

## NOT PROCESSED
# Plot-level workbooks without the -1 suffix. Those sheets are field-book templates without yield values.
# 06EVT16A-Combined.xls and 06EVT16A-Grain Yield.xls repeat the location means.
# Disease and lodging scores in the means files: gray leaf spot, rust, blight, ear rot, husk cover, weevil, and streak.


carob_script <- function(path) {

"
International Intermediate Yellow Variety Trial - IIYV0632

Summary results and individual trial results from the International Intermediate Yellow Variety - IIYV, (Subtropical Intermediate Yellow Normal and QPM Varieties - EVT16A) conducted in 2006.
"

	uri <- "hdl:11529/10566"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Intermediate Yellow Variety Trial - IIYV0632",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	num <- function(z) {
		z <- trimws(as.character(z))
		z[z %in% c("", "NA")] <- NA_character_
		as.numeric(z)
	}
	excel_date <- function(z) as.character(as.Date(num(z), origin="1899-12-30"))
	num_piece <- function(z) {
		z <- gsub("o|'|°", "", as.character(z))
		num(z)
	}

	floc <- ff[basename(ff) == "06EVT16A-Locations.xls"]
	loc0 <- carobiner::read.excel(floc, col_names=FALSE)
	c2 <- trimws(as.character(loc0[[2]]))
	keep <- !is.na(c2) & nzchar(c2) & !c2 %in% c("Country", "País", "Pais")
	loc0 <- loc0[keep, ]
	lat <- num_piece(loc0[[5]]) + num_piece(loc0[[6]]) / 60
	lon <- num_piece(loc0[[8]]) + num_piece(loc0[[9]]) / 60
	ns <- toupper(trimws(as.character(loc0[[7]])))
	we <- toupper(trimws(as.character(loc0[[10]])))
	lat[ns == "S"] <- -abs(lat[ns == "S"])
	lon[we == "W"] <- -abs(lon[we == "W"])
	country <- carobiner::fix_name(trimws(as.character(loc0[[2]])), "title")
	country[country == "México"] <- "Mexico"
	locs <- data.frame(
		set_no = as.integer(num(loc0[[4]])),
		country = country,
		location = carobiner::fix_name(trimws(as.character(loc0[[3]])), "title"),
		latitude = lat,
		longitude = lon,
		elevation = num(loc0[[11]]),
		planting_date = excel_date(loc0[[12]]),
		harvest_date = excel_date(loc0[[13]])
	)

	read_means <- function(f) {
		r <- carobiner::read.excel(f, col_names=FALSE)
		hdr <- as.character(unlist(r[1, ]))
		col <- function(name) {
			i <- which(hdr == name)
			if (!length(i)) return(rep(NA_real_, nrow(r)))
			r[[i[1]]]
		}
		ent <- trimws(as.character(r[[2]]))
		ped <- trimws(as.character(col("BreedersPedigree1")))
		drop_ent <- c("Entry", "ENT", "Mean", "LSD_0.05", "LSD (0.05)", "MSe", "")
		drop_ped <- c("Pedigree", "PED2", "PED1", "")
		keep <- !ent %in% drop_ent & !ped %in% drop_ped & !is.na(ped)
		id <- sub("-1\\.xls$", "", basename(f), ignore.case=TRUE)
		id <- sub("^06EVT16A", "", id)
		data.frame(
			set_no = as.integer(id),
			variety = ped[keep],
			yield = num(col("GrainYieldTons_FieldWt")[keep]) * 1000,
			anthesis_days = num(col("AnthesisDate")[keep]),
			asi = num(col("ASI")[keep]),
			plant_height = num(col("PlantHeightCm")[keep]),
			ear_height = num(col("EarHeightCm")[keep]),
			yield_moisture = num(col("GrainMoisturePer")[keep])
		)
	}

	trials <- ff[grepl("-1\\.xls$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(trials, read_means))
	d <- merge(d, locs, by="set_no", all.x=TRUE)
	d$crop <- "maize"
	d$treatment <- d$variety
	d$yield_part <- "grain"
	d$trial_id <- paste(d$set_no, d$location)
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$set_no <- NULL
	carobiner::write_files(path, meta, d)
}
