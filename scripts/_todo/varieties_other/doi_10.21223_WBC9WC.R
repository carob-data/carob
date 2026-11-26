# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station sweetpotato bulking trials. Geographic coverage names Uganda, Wakiso, and Serere. Dictionary location 1 is Namulonge in Wakiso and location 2 is Serere. Season 1 is 2017A and season 2 is 2017B.
# SRY and VY are tons per hectare. Storage-root yield is SRY * 1000. A cell value "*" is missing.
# The 90 DAP sheet is entirely harvest time 1, the 150 DAP sheet is harvest time 3, and the 180 DAP sheet is harvest time 4. Harvest time 2 is the remaining 120 DAP sheet. DAP is set from that workbook structure.
# Variety names come from the material list. The entry code is kept when the list has no name.
# Season codes 2017A and 2017B are not names in the season list, so they stay in season_code_. Harvest index is the percent term.

## ISSUES
# Coordinates are approximate research station locations for NaCRRI Namulonge and NaSARRI Serere (geo_from_source = FALSE).
# Commercial and non-commercial root weights are kg per plot, and plot area is not in the file, so they are not converted to kg/ha.
# SRY reaches about 112 t/ha; left as recorded.

## NEW VARIABLES
# bulking_score_: continuous storage-root formation score, 1 to 9.

## NOT PROCESSED
# ANOVA, means, and regression sheets.
# Root counts, root length, root diameter, vine weight in kg per plot, and senescence. Plot area is absent, and length and diameter have no unit in the dictionary row.
# Continuous_Storage_Root_Bulking_Data_Dictionary.xls: documentation or a codebook, not an observation table.


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
		response_vars = "yield;fwy_residue;harvest_index;pest_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Data and Analysis.xlsx"]
	# "*" is the missing marker. Reading the sheet as text avoids a warning on every one of those cells.
	r <- readxl::read_excel(f, sheet="8_Combined harvests", col_types="text")
	f1 <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Material_list.xlsx"]
	r1 <- carobiner::read.excel(f1)
	names(r1) <- trimws(names(r1))
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
	name <- trimws(as.character(r1$Names))
	names(name) <- trimws(as.character(r1$Code))
	variety <- name[code]
	variety <- ifelse(is.na(variety), code, as.character(variety))

	weevil <- num(r$Weevil)
	d <- data.frame(
		country = "Uganda",
		location = ifelse(loc == 1, "Namulonge", ifelse(loc == 2, "Serere", NA_character_)),
		adm1 = ifelse(loc == 1, "Wakiso", ifelse(loc == 2, "Serere", NA_character_)),
		# NaCRRI Namulonge and NaSARRI Serere station coordinates
		latitude = ifelse(loc == 1, 0.526, ifelse(loc == 2, 1.500, NA_real_)),
		longitude = ifelse(loc == 1, 32.613, ifelse(loc == 2, 33.450, NA_real_)),
		planting_date = "2017",
		season = ifelse(seas == 1, "first", ifelse(seas == 2, "second", NA_character_)),
		variety = variety,
		treatment = variety,
		rep = as.integer(num(r$Rep)),
		plot_id = trimws(as.character(r$Plot)),
		DAP = as.integer(dap),
		crop = "sweetpotato",
		yield = num(r$SRY) * 1000,
		yield_part = "roots",
		yield_isfresh = TRUE,
		fwy_residue = num(r$VY) * 1000,
		pest_species = ifelse(!is.na(weevil), "sweetpotato weevil", NA_character_),
		pest_severity = as.character(weevil),
		bulking_score_ = num(r$CSRFAB),
		harvest_index = num(r$HI),
		geo_from_source = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$trial_id <- paste(d$location, ifelse(seas == 1, "2017A", "2017B"), sep="_")
	d$yield_part[is.na(d$yield)] <- NA_character_
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
