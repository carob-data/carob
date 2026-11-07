# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per farm in ProductionCost_UnitArea.xlsx, 2011. The variable list says seedcotton yield is a Chinese pound of 0.5 kg per mu, and that a mu is 1/15 ha, so yield kg/ha is the column times 7.5. Costs in RMB per mu are times 15 to RMB per ha. Seedcotton price is RMB per 0.5 kg, divided by 0.5 to RMB per kg.
# Spray dates are the long table. The target is the Chinese text in pest_controlled. Some rows are disease, weeds, or a growth regulator, not an insect.
# Country is China and adm1 is Hebei, from geographicCoverage. District, county, and village are the Chinese names in the sheets. No coordinates are in the files.

## ISSUES
# Opinions are not mapped as yield.
# The highest seedcotton yield converts to 5250 kg/ha, above the cotton check.
# write_files: missing geo_from_source, latitude, longitude. unknown variables: labour_cost_ (labour?), spray_order_ (order?), spray_target_. high crop yield: cotton: 5250.

## NOT PROCESSED
# Farmers_opinions_evol.xlsx and Opinions_pest_change.xlsx.
# Cot_var_used.xlsx: variety name, Bt, and area in mu. Several varieties can occur on one farm, and the yield above is the farm seedcotton yield.
# Farm_families.xlsx and Farm_Land.xlsx. Land area is total mu, not the cotton yield area.
# Plastic-film and growth-regulator costs.
# List_variables.xlsx: not read. sheet info: Variables, Meaning, Comments. Observation records are missing from d until this file is standardized.
# Read_me.txt: not read. columns: The meaning of the variables are indicated in the file List_variables.. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Cotton farms and practices of insecticide sprays in Hebei, China, 15 years after Bt cotton commercial release

Data on a sample of cotton farms in Hebei province, out of which a sub-sample was followed about farmers' practices in chemical sprays to control cotton pests
"

	uri <- "doi:10.18167/DVN1/LWJOZQ"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 40,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "ProductionCost_UnitArea.xlsx"]
	r <- carobiner::read.excel(f, sheet="ProductionCost_UnitArea")
	per_ha <- 15
	# One fertilizer cost is written with a decimal comma.
	num <- function(x) as.numeric(gsub(",", ".", trimws(as.character(x)), fixed=TRUE))

	d <- data.frame(
		trial_id = trimws(as.character(r$Farm_ID)),
		hhid = trimws(as.character(r$Farm_ID)),
		country = "China",
		adm1 = "Hebei",
		adm2 = trimws(as.character(r$district)),
		adm3 = trimws(as.character(r$county)),
		location = trimws(as.character(r$village)),
		crop = "cotton",
		yield = num(r$yield_lbpermu) * 7.5,
		yield_part = "seed",
		crop_price = num(r$seedcot_price_RMB_lb) / 0.5,
		currency = "CNY",
		fertilizer_cost = num(r$ferti_mu_cost) * per_ha,
		insecticide_cost = num(r$pestcontrol_mu_cost) * per_ha,
		herbicide_cost = num(r$herbicide_mu_cost) * per_ha,
		irrigation_cost = num(r$irri_mu_cost) * per_ha,
		land_prep_cost = num(r$soil_prepa_mu_cost) * per_ha,
		planting_cost = num(r$sowing_mu_cost) * per_ha,
		fungicide_cost = num(r$disease_mu_cost) * per_ha,
		labour_cost_ = num(r$labour_mu_cost) * per_ha,
		date = "2011",
		on_farm = TRUE,
		is_survey = TRUE
	)

	f1 <- ff[basename(ff) == "Sub_Spray_Date_WhatControlled.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Sub_Spray_Date_WhatControlled")
	long <- data.frame(
		hhid = trimws(as.character(r1$Farm_ID)),
		date = as.character(as.Date(r1$Date_spraying, tz="UTC")),
		spray_target_ = trimws(as.character(r1$pest_controlled))
	)
	long <- long[!is.na(long$hhid) & !is.na(long$date), ]
	long$spray_order_ <- ave(seq_len(nrow(long)), long$hhid, long$date, long$spray_target_, FUN=seq_along)

	carobiner::write_files(path, meta, d, long=long)
}
