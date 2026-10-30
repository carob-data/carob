# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI survey of hybrid and inbred rice growers in Isabela (Cauayan City) and Bukidnon (Valencia City), Philippines, 2005-2006; 128 households, 324 household x season x variety records.
# Main table "costs and return.xlsx"; codes and units from the "Costs & Return" sheet of "Socio-economic evaluation of hybrid rice....xls".
# Production PROD is in cavans; WEIGHT is kg per cavan, so yield = PROD * WEIGHT / AREA (kg/ha). AREA is area planted (ha).
# Seed (SEEDQ, kg) and costs (PHP, totals for the area planted) are divided by AREA to get per ha values.
# Codes: SEASON 1 wet, 2 dry; VARTYP 1 hybrid, 2 inbred; MESTB 1 direct seeded, 2 transplanted; IRRIG 1 gravity (NIA), 2 pump, 3 rainfed, 12 pump & NIA; TENURE 1 owned, 2 CLT holder, 3 share tenant, 4 fixed rent, 5 mortgaged-in; LANDEL 1 upland, 2 medium land, 3 lowland (12, 23 combinations).
# AGEHS (age of hybrid seedlings at transplanting, days after sowing) is used as transplanting_days for transplanted records only where > 0.

## ISSUES
# No coordinates; adm1 (province), adm2 (city) and village (location) given. Human must georeference the 6 villages.
# Survey year per season is not given (collection 2005-2006); planting_date not set.
# Fertilizer: only total cost (FRTCST) is given; no product amounts or N/P/K, so nutrient rates are missing.
# AGEHS is 0 for most inbred/direct-seeded records; 0 treated as not applicable.
# HXAPP (number of herbicide applications) has values 0.5; kept as given, so write_files reports bad datatype for herbicide_times (numeric, not integer).
# write_files also reports missing longitude/latitude/geo_from_source and the new variables.
# INSQ, FUNAMT, FUNUPRI are text with mixed units ("12boxes", "10packs", "450/800"); not processed.
# Not mapped: labour days and labour/food/fuel/rental costs, production disposal (SOLD, shares, home consumption), Utilization.xlsx (parcel-level utilisation, overlaps with this table), Household information/variables (income sources, hybrid seed source), Farmer's perception, hybrid seed availability.

## NEW VARIABLES
# land_elevation_: land elevation class of the farm (upland, medium land, lowland or combinations)
# education_years_: education of the household head (years)


carob_script <- function(path) {

"
Socio-Economic Evaluation of Hybrid Rice Cultivation in the Philippines

To estimate yield and profitability of gains in the cultivation of hybrids compared to inbreds.
"

	uri <- "doi:10.7910/DVN/26246"
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
		carob_completion = 45,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "costs and return.xlsx"]
	r <- carobiner::read.excel(f)

	irr <- c("1"="gravity (NIA)", "2"="pump", "3"="rainfed", "12"="pump;gravity (NIA)")[as.character(r$IRRIG)]
	ten <- c("owned", "CLT holder", "share tenant", "fixed rent", "mortgaged-in")[r$TENURE]
	elev <- c("1"="upland", "2"="medium land", "3"="lowland", "12"="upland;medium land", "23"="medium land;lowland")[as.character(r$LANDEL)]

	d <- data.frame(
		hhid = as.character(r$RECNO),
		country = "Philippines",
		adm1 = carobiner::fix_name(r$PROV, "title"),
		adm2 = carobiner::fix_name(r$MUN, "title"),
		location = carobiner::fix_name(r$VIL, "title"),
		land_elevation_ = as.vector(elev),
		land_tenure = ten,
		farmland = r$FSIZEha,
		age = r$HHAGE,
		education_years_ = r$HHEDU,
		hh_size = r$HHSIZE,
		season = c("wet", "dry")[r$SEASON],
		crop = "rice",
		field_size = r$AREA,
		variety = r$VAR,
		variety_type = c("hybrid", "inbred")[r$VARTYP],
		planting_method = c("direct seeding", "transplanting")[r$MESTB],
		seed_rate = r$SEEDQ / r$AREA,
		seed_cost = r$SEEDC / r$AREA,
		transplanting_days = ifelse(r$MESTB == 2 & r$AGEHS > 0, r$AGEHS, NA),
		irrigation_source = as.vector(irr),
		irrigated = r$IRRIG != 3,
		irrigation_cost = r$IRRNIA / r$AREA,
		fertilizer_cost = r$FRTCST / r$AREA,
		insecticide_cost = r$INSCST / r$AREA,
		insecticide_times = as.integer(r$IXAPP),
		herbicide_cost = r$HERBC / r$AREA,
		herbicide_times = r$HXAPP,
		yield = r$PROD * r$WEIGHT / r$AREA,
		crop_price = r$PRIKG,
		currency = "PHP"
	)

	d$insecticide_used <- d$insecticide_times > 0
	d$herbicide_used <- d$herbicide_times > 0
	d$trial_id <- paste(d$hhid, r[[4]], sep="-")
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
