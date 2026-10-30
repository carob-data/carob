# R script for "carob"
# license: GPL (>=3)

## NOTES
# The dictionary unit for Yield (Kg/Row) is Kg. It is kilograms per row, not per hectare. Row length is not in the files, so this is not converted to kg/ha.
# SPAD is the chlorophyll index. The value NR is treated as missing. One cell is "50 .6"; the internal space is removed so it reads 50.6. SPAD was recorded at 159 and 187 days after planting. Row yield was recorded at 228 and 236 days. Those dates are in the long table.
# Country Peru, adm1 La Libertad, and adm3 Chugay come from the title. Trial is the field name (La Soledad or San Salvador).
# The Dataverse bounding box publishes only south -7.785575 and west -77.854397. That is one point for the dataset, not a coordinate of each field, so latitude and longitude are omitted.

## ISSUES
# No yield in kg/ha. Row length is unknown.
# La Soledad and San Salvador need georeferencing. The bounding box is not used as a field coordinate.
# No variety, fertilizer, irrigation, or planting and harvest dates. DAP is days after planting, not a calendar date.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: row_yield_kg_, plants_per_row_.

## NEW VARIABLES
# row_yield_kg_: harvested weight of the row, kilograms.
# plants_per_row_: number of plants in the row.

## NOT PROCESSED
# Plano_La Soledad.png and Plano_San Salvador.png: field sketches.


carob_script <- function(path) {

"
Dataset for: Dataset for potato yields and SPAD values recorded on two potato fields in the Chugay district – La Libertad on the frame of Excellence in Agronomy project (LAC Use Cases in Peru).

The aim of this dataset is to generate a database for estimating potato yield using remote sensing data and machine learning algorithms. The dataset includes potato yields per plot and SPAD measurements.
"

	uri <- "doi:10.21223/MMKKZ4"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "none",
		response_vars = "SPAD",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.35
	)

	y <- carobiner::read.excel(ff[basename(ff) == "01_yield.xlsx"])
	s <- carobiner::read.excel(ff[basename(ff) == "02_spad.xlsx"])

	y$ID_QGIS <- as.integer(y$ID_QGIS)
	s$ID_QGIS <- as.integer(s$ID_QGIS)
	ids <- sort(unique(c(y$ID_QGIS, s$ID_QGIS)))
	loc_y <- y[!duplicated(y$ID_QGIS), c("ID_QGIS", "Trial")]
	loc_s <- s[!duplicated(s$ID_QGIS), c("ID_QGIS", "Trial")]
	loc <- rbind(loc_y, loc_s)
	loc <- loc[!duplicated(loc$ID_QGIS), ]
	names(loc)[2] <- "location"

	d <- data.frame(
		record_id = ids,
		plot_id = as.character(ids),
		crop = "potato",
		country = "Peru",
		adm1 = "La Libertad",
		adm3 = "Chugay",
		trial_id = loc$location[match(ids, loc$ID_QGIS)],
		location = loc$location[match(ids, loc$ID_QGIS)],
		geo_from_source = FALSE,
		on_farm = NA,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	spad <- trimws(as.character(s$SPAD))
	spad[grepl("^NR$", spad, ignore.case=TRUE)] <- NA_character_
	spaced <- !is.na(spad) & grepl(" ", spad, fixed=TRUE)
	collapsed <- gsub(" ", "", spad, fixed=TRUE)
	keep <- spaced & grepl("^[0-9]+(\\.[0-9]+)?$", collapsed)
	spad[keep] <- collapsed[keep]
	long_s <- data.frame(
		record_id = s$ID_QGIS,
		DAP = as.integer(s$DAP),
		SPAD = as.numeric(spad),
		row_yield_kg_ = NA_real_,
		plants_per_row_ = NA_real_,
		stringsAsFactors = FALSE
	)
	long_y <- data.frame(
		record_id = y$ID_QGIS,
		DAP = as.integer(y$DAP),
		SPAD = NA_real_,
		row_yield_kg_ = as.numeric(y[["Yield (Kg/Row)"]]),
		plants_per_row_ = as.numeric(y[["Number plants"]]),
		stringsAsFactors = FALSE
	)
	dlong <- rbind(long_s, long_y)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
