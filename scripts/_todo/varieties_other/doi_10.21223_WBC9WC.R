# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station sweetpotato bulking trials. Geographic coverage names Uganda, Wakiso, and Serere. Dictionary location 1 is Namulonge in Wakiso and location 2 is Serere. Season 1 is 2017A and season 2 is 2017B.
# SRY and VY are tons per hectare. Storage-root yield is SRY * 1000. A cell value "*" is missing.
# The 90 DAP sheet is entirely harvest time 1, the 150 DAP sheet is harvest time 3, and the 180 DAP sheet is harvest time 4. Harvest time 2 is the remaining 120 DAP sheet. DAP is set from that workbook structure.
# Variety names come from the material list. The entry code is kept when the list has no name.
# Season codes 2017A and 2017B are not names in the season list, so they stay in season_code_. Harvest index is the percent term.

## ISSUES
# No coordinates.
# Commercial and non-commercial root weights are kg per plot, and plot area is not in the file, so they are not converted to kg/ha.
# SRY of about 112 t/ha is left as recorded.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: season_code_ (season?). unknown variable: vine_yield_ (yield?). unknown variables: weevil_score_, bulking_score_. high crop yield: sweetpotato: 112488.75. NA detected: yield, yield_part.

## NEW VARIABLES
# vine_yield_: vine yield, kg/ha, from VY in t/ha.
# weevil_score_: sweetpotato weevil reaction, 1 to 5.
# bulking_score_: continuous storage-root formation score, 1 to 9.
# season_code_: 2017A or 2017B.

## NOT PROCESSED
# ANOVA, means, and regression sheets.
# Root counts, root length, root diameter, vine weight in kg per plot, and senescence. Plot area is absent, and length and diameter have no unit in the dictionary row.


carob_script <- function(path) {

"
Dataset for: Continuous Storage Root Formation and Bulking in Sweetpotato

The data set for the 'Continuous Storage Root Formation and Bulking in sweetpotato ' Gates Open Research paper has on-station plant breeding trials conducted at Namulonge and Serere in Wakiso and Serere districts respectively between 2016 and 2017 leading to an original research article. The on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-station data on Continuous storage root formation and bulking scores, root numbers, vegetative growth data, and root weight from plots over 4 harvesting times used to estimate growth overtime and compute root yield, and biomass yield was the basis for the article. The pdf file has details on background information, site description, materials and methods, analysis, results and discussion, and relevant tables and illustrations. Supporting raw data and analysis tables include spreadsheets for singles harvests (at 90, 120, 150 and 180 Days after planting), the combined data sheet for all seasons and locations, and its respective analysis tables, summary means and breeding value estimations.
"

	uri <- "doi:10.21223/WBC9WC"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	f <- ff[grepl("Data and Analysis", basename(ff))]
	# "*" is the missing marker. Reading the sheet as text avoids a warning on every one of those cells.
	r <- readxl::read_excel(f, sheet="8_Combined harvests", col_types="text")
	mf <- ff[grepl("Material_list", basename(ff))]
	mat <- carobiner::read.excel(mf)
	names(mat) <- trimws(names(mat))
	num <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "*" | z == "NA"] <- NA_character_
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	ht <- as.integer(num(r$HT))
	dap <- c("1"=90, "2"=120, "3"=150, "4"=180)[as.character(ht)]
	loc <- as.integer(num(r$Loc))
	seas <- as.integer(num(r$Seas))
	code <- trimws(as.character(r$ENTRY))
	name <- trimws(as.character(mat$Names))
	names(name) <- trimws(as.character(mat$Code))
	variety <- name[code]
	variety <- ifelse(is.na(variety), code, as.character(variety))

	d <- data.frame(
		country = "Uganda",
		location = ifelse(loc == 1, "Namulonge", ifelse(loc == 2, "Serere", NA_character_)),
		adm1 = ifelse(loc == 1, "Wakiso", ifelse(loc == 2, "Serere", NA_character_)),
		season_code_ = ifelse(seas == 1, "2017A", ifelse(seas == 2, "2017B", NA_character_)),
		variety = variety,
		treatment = variety,
		rep = as.integer(num(r$Rep)),
		plot_id = trimws(as.character(r$Plot)),
		DAP = as.integer(dap),
		crop = "sweetpotato",
		yield = num(r$SRY) * 1000,
		yield_part = "roots",
		vine_yield_ = num(r$VY) * 1000,
		weevil_score_ = num(r$Weevil),
		bulking_score_ = num(r$CSRFAB),
		harvest_index = num(r$HI),
		geo_from_source = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	d$trial_id <- paste(d$location, d$season_code_, sep="_")
	d$yield_part[is.na(d$yield)] <- NA_character_
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
