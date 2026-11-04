# R script for "carob"
# license: GPL (>=3)

## NOTES
# Marketable tuber yield (MTYA) is t/ha in the dictionary (MkbYldAj_Cp_tha). Multiplied by 1000 to kg/ha. It is the only yield figure. Moisture is not stated.
# rAUDPC is the relative area under the late-blight curve. AUDPC is stored as recorded. The dictionary does not name the AUDPC scale. The numbers run into the thousands, which is the usual percent-days scale, while the vocabulary AUDPC is fraction-days.
# RLB is the 1-to-6 late-blight resistance score (dictionary RLb_Et_1to6). 1 is extreme resistance.
# Season LR and LR2 are both long rains. The source code is kept in season_code_ so the second season is not collapsed.
# Fungicide treatments are Control, Metalaxyl + Mancozeb, and Phosphonate. A control is no fungicide.
# Country and district come from 4296_Localities.xlsx. Sites have no coordinates in the files.

## ISSUES
# Sites need georeferencing. The localities file gives GeoNames links, not coordinates.
# No fertilizer rates, irrigation, planting day, or harvest day. YEAR values 2011a and 2011b are not a single year, so planting_date is left empty on those rows.
# AUDPC is not divided by 100. write_files will report it out of bounds if the check uses the fraction-days limit.
# Phosphonate is not in the fungicide vocabulary. It is kept as printed.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variable: season_code_. out of bounds: AUDPC (0, 4645). invalid terms: fungicide_product: phosphonate.

## NEW VARIABLES
# season_code_: season code as printed (LR or LR2).

## NOT PROCESSED
# 4296_Data.xlsx columns Disease_Pressure, Pressure, Disease_reduction, Relative_to_Asante, Yield_increase_relative_to_control. Comparisons derived from the plot scores.
# 4296_Data.xlsx sheet Calculate_Efficacy and sheet results. Efficacy and susceptibility summaries, not plot records.
# 4296_DataDictionary_ElementDescription.xlsx and 4296_DataDictionary_UniqueIdentifier.xlsx. Definitions.


carob_script <- function(path) {

"
Dataset for: Integrated management of late blight on potato

Integrated management of late blight on potato
"

	uri <- "doi:10.21223/6RV436"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;fungicide_product",
		response_vars = "yield;yield_marketable;AUDPC;rAUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "4296_Data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")

	sites <- data.frame(
		SITE = c("UoN", "Koibatek", "Njabini", "Limuru"),
		location = c("University of Nairobi", "Koibatek", "Njabini", "Limuru"),
		adm2 = c("Nairobi", "Koibatek", "Nyandarua", "Kiambu")
	)
	bad <- setdiff(unique(r$SITE), sites$SITE)
	if (length(bad) > 0) stop("unmapped site: ", paste(bad, collapse=", "))

	fung <- trimws(r$TREATMENT)
	fung_product <- rep(NA_character_, length(fung))
	fung_product[fung == "Control"] <- "none"
	fung_product[fung == "Metalaxyl + Mancozeb"] <- "metalaxyl;mancozeb"
	fung_product[fung == "Phosphonate"] <- "phosphonate"
	if (anyNA(fung_product)) stop("unmapped fungicide treatment")

	year <- trimws(as.character(r$YEAR))
	plant_year <- ifelse(grepl("^[0-9]{4}$", year), year, NA_character_)
	season_code <- trimws(as.character(r$SEASON))

	d <- data.frame(
		country = "Kenya",
		location = sites$location[match(r$SITE, sites$SITE)],
		adm2 = sites$adm2[match(r$SITE, sites$SITE)],
		variety = trimws(r$ACCENAME),
		variety_code = trimws(as.character(r$ACCENUMB)),
		treatment = fung,
		fungicide_product = fung_product,
		fungicide_used = fung != "Control",
		rep = as.integer(r$REP),
		season = "long rains",
		season_code_ = season_code,
		planting_date = plant_year,
		yield = as.numeric(r$MTYA) * 1000,
		yield_marketable = as.numeric(r$MTYA) * 1000,
		yield_part = "tubers",
		AUDPC = as.numeric(r$AUDPC),
		rAUDPC = as.numeric(r$rAUDPC),
		disease = "potato late blight",
		pathogen = "Phytophthora infestans",
		stress = "disease",
		disease_severity = as.character(r$RLB),
		severity_scale = "1-6",
		crop = "potato",
		trial_id = paste(r$SITE, year, season_code),
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
