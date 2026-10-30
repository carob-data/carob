# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI farm survey of AWD (alternate wetting and drying) adoption in the Zhanghe Irrigation System (ZIS), Hubei, China; 1999 and 2000 rice season.
# Only IOMERGE.xls holds data (one row per household x year x parcel, 112 rows, 60 households). Variable descriptions are in "AWD on Rice Productivity and Profitability.xls".
# All inputs are already per ha in the source (AREAPLT is ha). YIELDHA is kg/ha (PRODN kg/parcel / AREAPLT, checked).
# PHA and KHA are P2O5 and K2O (codebook); converted to elemental P (/2.29) and K (/1.2051).
# CEDATE is the transplanting date (METHOD is TPR for all rows); planting_date (seedbed sowing) = CEDATE - SEEDAGE (seedling age, days).
# Labour columns are 8-hr person-days/ha; fertilizer_labour and weeding_labour sum family + hired + exchange labour.
# Prices and costs are Yuan (CNY).
# Associated papers (Project details.xlsx, no DOI given): Moya et al. 2004, Paddy and Water Environment 2(4):207-216; Moya et al. 2001, in Barker et al. (eds) Water Saving Irrigation for Rice, Wuhan.

## ISSUES
# No coordinates. SITE is WJX or TUANLIN (WJX abbreviation not expanded) within ZIS, Hubei; human must georeference (Zhanghe reservoir area, Jingmen).
# SEASON code "WSRICE" mapped to season "wet"; verify it means wet season (single mid-season rice, May-Sep).
# ELEV is the toposequence position of the field (UPPER/MIDDLE/LOWER/COMBI), not elevation in m; kept as field_position_.
# PWSOURCE (animal/tractor/cow) spellings normalized to "animal"/"tractor"; land_prep_traction values may not be in the vocabulary.
# HDATE is missing for all 2000 rows (60); harvest_date only for 1999.
# Manure type is not given (MANQTYHA only); OM_type not set.
# Not mapped: most labour and cost detail (LP*, CE*, HT*, HDC*, IN*/HE*/MA* labour, fuel, machine rental, food costs), water depth by stage (DEPGROW, DEPLPREP, DEPSBED, DEPTPL), FREQWH2O/FREQDRY/FREQWET (codebook descriptions are duplicated, meaning unclear), AWDSCORE2.
# Publication DOI not given in the metadata; Paddy and Water Environment 2004 paper should be checked for site coordinates and the WJX name.
# write_files: missing longitude/latitude/geo_from_source; land_prep_traction reported as unknown variable (although listed in terminag management); out of bounds transplanting_days (seedling age up to 60 d, valid max 50), K_fertilizer (one parcel 409 kg K2O/ha), irrigation_amount (TOTDEPTH up to 1290 mm, valid max 1000). Values left as reported.

## NEW VARIABLES
# field_position_: position of the field in the toposequence of the irrigation system (UPPER, MIDDLE, LOWER, COMBI)
# land_prep_date_: date of plowing (PLDATE)
# awd_practiced_: farmer practices AWD (PRACTAWD, TRUE/FALSE)
# awd_score_: estimated AWD score over the crop growth period, transplanting to harvest (AWDSCORE, 0-1)
# awd_group_: AWD classification of the parcel (AWD, CF = continuous flooding, COMB = combination)
# irrigation_hours_: total hours of irrigation (IRRGHOUR, h)


carob_script <- function(path) {

"
The Impact of AWD on rice productivity and profitability in ZIS, Hubei, China

This is a component of a big research project in China whose objective is to develop irrigation management strategies and techniques  for rice based systems that are high in water efficiency, productivity and cost effective. This research is an interdisciplinary collaboration among scientists from Wuhan University, Zhanghe irrigation System, IWMI in Sri Lanka and IRRI.  This particular study was undertaken to evaluate the impact of AWD on crop management practices and the profitability of rice production in Zhnaghe Irrigation system.
"

	uri <- "doi:10.7910/DVN/26299"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "IOMERGE.xls"]
	r <- carobiner::read.excel(f)

	ceday <- as.Date(r$CEDATE)
	pw <- tolower(trimws(r$PWSOURCE))
	pw <- gsub("ani|cow", "animal", pw)
	pw <- gsub("animalmal", "animal", pw)
	pw <- gsub("/", ";", pw)

	d <- data.frame(
		hhid = as.character(r$HHCODE),
		field_id = as.character(r$PARNO),
		country = "China",
		adm1 = "Hubei",
		location = carobiner::fix_name(r$SITE, "title"),
		season = ifelse(r$SEASON == "WSRICE", "wet", NA),
		field_position_ = tolower(r$ELEV),
		crop = "rice",
		field_size = r$AREAPLT,
		land_prep_date_ = as.character(as.Date(r$PLDATE)),
		land_prep_times = as.integer(r$PLNOTIME),
		land_prep_traction = pw,
		planting_method = ifelse(r$METHOD == "TPR", "transplanting", NA),
		transplanting_date = as.character(ceday),
		planting_date = as.character(ceday - r$SEEDAGE),
		transplanting_days = r$SEEDAGE,
		seed_rate = r$QSEEDHA,
		N_fertilizer = r$NHA,
		P_fertilizer = r$PHA / 2.29,
		K_fertilizer = r$KHA / 1.2051,
		OM_used = r$MANQTYHA > 0,
		OM_amount = r$MANQTYHA,
		harvest_date = as.character(as.Date(r$HDATE)),
		irrigated = TRUE,
		irrigation_amount = r$TOTDEPTH,
		irrigation_number = as.integer(r$FREQIRRG),
		irrigation_hours_ = r$IRRGHOUR,
		awd_practiced_ = r$PRACTAWD == 1,
		awd_score_ = r$AWDSCORE,
		awd_group_ = r$AWDGRP,
		yield = r$YIELDHA,
		crop_price = r$PRICE,
		crop_value = r$YLDVALHA,
		currency = "CNY",
		seed_cost = r$SEEDCSTH,
		fertilizer_cost = r$FERCOSTH,
		insecticide_cost = r$INSCSTHA,
		herbicide_cost = r$HERCSTHA,
		irrigation_cost = r$IRRFEEHA,
		fertilizer_labour = r$CFFLABHA + r$CFHLABHA + r$CFELABHA,
		weeding_labour = r$HWFLABHA + r$HWHLABHA + r$HWELABHA
	)

	d$trial_id <- paste(d$hhid, r$YEAR, d$field_id, sep="-")
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
