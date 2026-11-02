# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey (2010) of rice farmers in Cambodia, Pakistan and Sri Lanka. The 290 files are 30 survey
# tables, most split into up to 11 files (no suffix and suffixes _1 to _10), one per survey area:
# Cambodia (none, _1 to _4), Philippines (_5), Pakistan (_6), Sri Lanka (_7 to _10).
# Mapped: the rice variety table (tbl04aricevariety_x*), one record per farmer x season x parcel x plot x
# variety with area (ha) and production (kg), read in a loop over the 10 Cambodia/Pakistan/Sri Lanka files that
# share the same layout. District and village come from the matching tbl00surveyinfo* file.
# Farmer ids are reused across files, so hhid includes the file suffix.
# Sri Lanka seasons Maha (main, north-east monsoon) and Yala (minor) are mapped to "wet" and "dry"; the original
# season names are in season_local_.

## ISSUES
# The _5 (Philippines) files are the 2012-2013 Laguna survey of GSR Phase 2 (doi:10.7910/DVN/26195, mapped
#   there); the _5 rice variety file also has a different layout (production in local units). Not used here.
# Not mapped: tbl06a/b/c "largest parcel" tables (planting and harvest month, grain and straw production, price
#   for the largest parcel per season), material inputs (fertilizer, pesticides; tbl06matinputs*), labour, power,
#   costs, consumption, income, credit, decisions, training, stress-tolerant variety and household tables.
#   The largest parcel and input tables could be added to give planting/harvest month and N/P/K.
# Year of the seasons is not given (survey year 2010, interview dates mostly mid-2010, with some clear entry
#   errors, e.g. 2001, 2020, 6474); planting_date and harvest_date are NA.
# Area is documented as ha, but Pakistan (_6) areas (median 4, max 50) and production look like acres; Pakistan
#   yields (median 1280 kg/ha) may be per acre. Check before use.
# "Wet 2" in the Cambodia files is not explained (perhaps the second wet season or a late wet crop); mapped
#   as "wet", original in season_local_.
# Crop establishment "Planting" (Sri Lanka _8, 2 records) is ambiguous and set to NA. "Sowing" (Puttalam) is
#   mapped as "direct seeding". WSR (wet seeded rice) and DSR as "direct seeding".
# Production is taken as fresh paddy (moisture not given). There are 0-production records (e.g. "No any
#   production (because flood)"), kept as 0.
# Variety names are as recorded (local names and codes), not harmonized.
# No coordinates; only district and village are given.
# write_files remaining: NA coordinates (and NA planting_date, N/P/K fertilizer, yield_moisture); unknown
#   (new) variables.

## NEW VARIABLES
# land_type_: land type of the plot as reported (lowland, medium, upland)
# season_local_: season as recorded in the survey (Wet, Wet 2, Dry, Kharif, Maha, Yala)


carob_script <- function(path) {

"
Green Super Rice for the Resource Poor of Asia and Africa (GSR) - Phase 1

1. To analyze the patterns of adoption and diffusion of existing improved varieties, and identify constraints to adoption;            2.  To analyze the economics of rice production and farmer livelihood strategies and understand the gender roles in rice production and women's participation in decision making;   3. To estimate the potential impact of improved varieties being developed under the project on rice production, farmer income and poverty reduction; and     4.  To draw implications for technology development, targeting and policy reforms.
"

	uri <- "doi:10.7910/DVN/26194"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "Green Super Rice (GSR)",
		design = "household survey; about 1200 households in Cambodia, Pakistan and Sri Lanka",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = "Yield is farmer-reported production per reported area",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.8
	)

	sfx <- c("", paste0("_", c(1:4, 6:10)))

	get_file <- function(s) {
		f4 <- ff[basename(ff) == paste0("tbl04aricevariety_x", s, ".xlsx")]
		f0 <- ff[basename(ff) == paste0("tbl00surveyinfo", s, ".xlsx")]
		r <- carobiner::read.excel(f4)
		h <- carobiner::read.excel(f0)
		i <- match(r$farmerid, h$farmerid)
		data.frame(
			file = s,
			farmerid = r$farmerid,
			country = h$country[i],
			adm2 = h$district[i],
			location = h$village[i],
			season_local_ = trimws(r$season),
			parcel = r$parcel,
			plot = r$plot,
			subplot = r$subplot,
			land_type_ = tolower(trimws(r$landtype)),
			variety = trimws(r$ricevar),
			mvtv = r$mvtv,
			cem = tolower(trimws(r$cem)),
			area = as.numeric(r$area),
			prod = as.numeric(r$prod)
		)
	}
	r <- do.call(rbind, lapply(sfx, get_file))

	d <- data.frame(
		hhid = paste0("f", r$file, "_", r$farmerid),
		country = r$country,
		adm2 = r$adm2,
		location = r$location,
		season_local_ = r$season_local_,
		variety = r$variety,
		field_size = r$area,
		yield = r$prod / r$area
	)
	d$trial_id <- paste(d$hhid, r$parcel, r$plot, r$subplot, sep="_")

	slc <- tolower(r$season_local_)
	d$season <- c("wet", "wet", "dry", "kharif", "wet", "dry")[match(slc, c("wet", "wet 2", "dry", "kharif", "maha", "yala"))]

	lt <- r$land_type_
	lt[lt %in% c("low land", "lawland")] <- "lowland"
	d$land_type_ <- lt

	d$variety_type <- ifelse(r$mvtv == "MV", "improved", "landrace")
	cem <- r$cem
	d$planting_method <- NA
	d$planting_method[cem %in% c("broadcasting", "broardcast")] <- "broadcasting"
	d$planting_method[cem %in% c("tpr", "transplant", "transplanting")] <- "transplanting"
	d$planting_method[cem %in% c("dsr", "wsr", "sowing")] <- "direct seeding"

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)
	d$geo_from_source <- FALSE
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
