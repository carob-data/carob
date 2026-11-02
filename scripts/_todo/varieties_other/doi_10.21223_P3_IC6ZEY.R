# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station sweetpotato bulking trial. Dictionary location 1 is Namulonge in Wakiso and location 2 is Serere. Season 1 is 2017A and season 2 is 2017B. Country is Uganda from geographicCoverage.
# The combined harvest sheet is used. The 90, 150, and 180 DAP sheets are entirely harvest times 1, 3, and 4. Harvest time 2 is the 120 DAP sheet, which has no harvest-time column. DAP is set from that.
# SRY and VY are t/ha in the dictionary. Storage-root yield is SRY * 1000 and vine yield is VY * 1000. Harvest index is the fresh-weight percent in the dictionary formula. A cell value * is missing.
# Variety names are the material-list names. The entry code is variety_code.

## ISSUES
# No coordinates, fertilizer, or irrigation. Commercial and non-commercial root weights are kg per plot, and plot area is not a column, so those weights are not converted again. SRY is already t/ha.
# Vine yield reaches about 283 t/ha. The dictionary unit is t/ha, so the value is left as recorded.
# Storage-root yield reaches about 112 t/ha.
# Weevil and the bulking score include 0. The dictionary categories start at 1.
# The combined sheet has 1039 rows at harvest time 2. The 120 DAP sheet has 1040 rows.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: season_code_ (season?). unknown variables: weevil_score_, bulking_score_. out of bounds: fwy_residue (0, 283305). high crop yield: sweetpotato: 112488.75. NA detected: yield.

## NEW VARIABLES
# season_code_: 2017A or 2017B. Those codes are not season names.
# weevil_score_: sweetpotato weevil score. The label says 1-5 and the categories run from 1 to 9.
# bulking_score_: continuous storage-root formation and bulking score, 1 to 9. The column is CSRFAB.

## NOT PROCESSED
# ANOVA, means, and regression sheets.
# Root counts, root length, root diameter, senescence, and vine weight in kg per plot. Length and diameter have no unit in the dictionary.


carob_script <- function(path) {

"
Dataset for: Continuous Storage Root Formation and Bulking in Sweetpotato

The data set for the “Continuous Storage Root Formation and Bulking in sweetpotato ” Gates Open Research paper has  on-station plant breeding trials conducted at Namulonge and Serere in Wakiso and Serere districts respectively  between 2016 and 2017 leading to an original research article. The on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-station data on Continuous storage root formation and bulking scores, root numbers, vegetative growth data, and root weight from plots over 4 harvesting times used to estimate growth overtime and compute root yield, and biomass yield was the basis for the article. The pdf file has details on background information,  site description, materials and methods, analysis, results and discussion, and relevant tables and illustrations. Supporting raw data and analysis tables include spreadsheets for singles harvests (at 90, 120, 150 and 180 Days after planting), the combined data sheet for all seasons and locations, and its respective analysis tables, summary means and breeding value estimations.
"

	uri <- "doi:10.21223/P3/IC6ZEY"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;fwy_residue;harvest_index",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Data and Analysis.xlsx"]
	r <- carobiner::read.excel(f, sheet="8_Combined harvests", col_types="text")
	mf <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Material_list.xlsx"]
	mat <- carobiner::read.excel(mf)
	num <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "*"] <- NA_character_
		as.numeric(z)
	}
	ht <- as.integer(num(r$HT))
	dap <- rep(NA_integer_, length(ht))
	dap[ht == 1] <- 90L
	dap[ht == 2] <- 120L
	dap[ht == 3] <- 150L
	dap[ht == 4] <- 180L
	loc <- as.integer(num(r$Loc))
	seas <- as.integer(num(r$Seas))
	code <- trimws(as.character(r$ENTRY))
	name <- trimws(as.character(mat$Names))
	key <- trimws(as.character(mat$Code))
	variety <- name[match(code, key)]
	variety[is.na(variety)] <- code[is.na(variety)]
	season_code_ <- ifelse(seas == 1, "2017A", ifelse(seas == 2, "2017B", NA_character_))

	d <- data.frame(
		crop = "sweetpotato",
		country = "Uganda",
		location = ifelse(loc == 1, "Namulonge", ifelse(loc == 2, "Serere", NA_character_)),
		adm2 = ifelse(loc == 1, "Wakiso", ifelse(loc == 2, "Serere", NA_character_)),
		season_code_ = season_code_,
		variety = variety,
		variety_code = code,
		treatment = variety,
		rep = as.integer(num(r$Rep)),
		plot_id = trimws(as.character(r$Plot)),
		DAP = dap,
		yield = num(r$SRY) * 1000,
		yield_part = "roots",
		fwy_residue = num(r$VY) * 1000,
		harvest_index = num(r$HI),
		weevil_score_ = num(r$Weevil),
		bulking_score_ = num(r$CSRFAB),
		geo_from_source = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$trial_id <- paste(d$location, d$season_code_, sep="_")
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
