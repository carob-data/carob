# R script for "carob"
# license: GPL (>=3)

## NOTES
# SIMLEZA adoption survey, Eastern Province, Zambia. simleza.csv is one row per household with up to 10 crop slots.
# Metadata: CURRENT_AREA_01 and the later "Current level of planting (Area in heactares)" columns are hectares. FARM_SIZEHA is the whole farm, stored as farm_area_ha_.
# Slots whose crop text is not soybean, maize or cowpea (zeros and truncated phrases such as "labor shor") are dropped.
# YEAR is 2013 for 1022 rows and 2012 for one row. planting_date is that survey year, because the area is the current planted area at the interview, not the "year first planted" column.
# There is no yield column and no GPS or district column. adm1 Eastern is the study area named in the title (Eastern Zambia), not a household field.

## ISSUES
# No coordinates and no camp or district. Georeference within Eastern Province, Zambia.
# Variety columns (NAMF_*, VARNAME_1) are included when present. Confirm they are variety names rather than codes; several slot layouts use different column names (see the script).
# Area 0 is kept when a crop name is present (current planted area can be zero).
# Practice and variety-expansion blocks (PRACTICE_*, VARIETY_1 to VARIETY_5) are a second adoption module and were not stacked on these crop slots.
# No publication DOI.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude; unknown farm_area_ha_.

## NEW VARIABLES
# farm_area_ha_: FARM_SIZEHA, household farm size, repeated on each crop row

## NOT PROCESSED
# Household roster, assets, livestock, group membership (MEMBER_*, YEAR_JOINED_*).
# Per-slot information source, decision maker, rank scores, and year first planted.
# NAMG_* / management-practice slots and PRACTICE_CODE_* / VARIETY_* expansion plots.
# metadata_simleza.csv, aside from the area-unit descriptions.


carob_script <- function(path) {

"Monitoring Adoption of Agriculture Technologies in Eastern Zambia

This data was collected under a project called “Sustainable Intensification of the Maize-Legume Systems of the Eastern Province of Zambia. The objective of the study was to assess the awareness and adoption of agricultural technologies promoted by SIMLEZA Project. The study was conducted by the IITA and CIMMYT in collaboration with the Zambia Agricultural Research Institute (ZARI).
"

	uri <- "doi:10.25502/n029-j541/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIMMYT",
		publication = NA,
		project = "SIMLEZA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	r <- read.csv(ff[basename(ff) == "simleza.csv"])

	slots <- data.frame(
		crop = c("CROP_01", "CROP_02", "CROP_03", "CROP_04", "CROP_05", "CROP_06", "CROP_07", "CROP_08", "CROP_09", "CROP_10"),
		area = c("CURRENT_AREA_01", "CURRENT_AREA_02", "CURRENT_LEVEL_OF_PLANTING_AREA", "CURRENT_LEVEL_OF_PLANTING_AREA_0", "CURRENT_LEVEL_OF_PLANTING_AREA_1", "CURRENT_LEVEL_OF_PLANTING__AREA2", "CURRENT_LEVEL_OF_PLANTING__AREA3", "CURRENT_LEVEL_OF_PLANTING__AREA4", "CURRENT_LEVEL_OF_PLANTING__AREA5", "CURRENT_LEVEL_OF_PLANTING__AREA6"),
		variety = c("NAMF_01", "VARNAME_1", "NAMF_03", "NAMF_04", "NAMF_05", "NAMF_06", "NAMF_07", "NAMF_08", "NAMF_09", "NAMF_10")
	)

	parts <- list()
	for (i in seq_len(nrow(slots))) {
		if (!(slots$crop[i] %in% names(r)) || !(slots$area[i] %in% names(r))) next
		variety <- if (slots$variety[i] %in% names(r)) trimws(as.character(r[[slots$variety[i]]])) else rep(NA_character_, nrow(r))
		parts[[i]] <- data.frame(
			hhid = as.character(r$ID),
			plot_id = as.character(i),
			crop_raw = trimws(as.character(r[[slots$crop[i]]])),
			field_size = as.numeric(r[[slots$area[i]]]),
			variety = variety,
			farm_area_ha_ = r$FARM_SIZEHA,
			planting_date = as.character(r$YEAR)
		)
	}
	d <- do.call(rbind, parts)
	d <- d[!is.na(d$crop_raw) & d$crop_raw != "" & tolower(d$crop_raw) != "not applic", ]
	crop <- tolower(d$crop_raw)
	crop[crop == "soybean"] <- "soybean"
	crop[crop == "maize"] <- "maize"
	crop[crop == "cowpea"] <- "cowpea"
	d$crop <- crop
	d <- d[d$crop %in% c("soybean", "maize", "cowpea"), ]
	d$crop_raw <- NULL
	d$variety[d$variety %in% c("", "NA", "Not applic")] <- NA
	d$yield_part <- ifelse(d$crop == "maize", "grain", ifelse(d$crop %in% c("soybean", "cowpea"), "seed", NA_character_))
	d$country <- "Zambia"
	d$adm1 <- "Eastern"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
