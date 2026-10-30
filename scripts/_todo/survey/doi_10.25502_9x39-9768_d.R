# R script for "carob"
# license: GPL (>=3)

## NOTES
# Humidtropics / CIALCA baseline, Burundi. Plot-crop table is input_use.csv (metadata: crop_area_ha in hectares, Qty_fert_kg and Qty_fert2_used in kg of product, T_harvst_dry and T_harvst_fresh in kg).
# Every Country value is code 2. The file hh_compostion_characteristic_b_burundi.csv and the dataset title identify the survey as Burundi, so country is Burundi. District, sector and village are numeric codes with no label table in the deposit; they are kept as district_code_, sector_code_ and location_code_.
# Yield uses fresh kilograms when T_harvst_fresh > 0, otherwise dry kilograms, divided by crop_area_ha when area > 0. If both fresh and dry are positive, yield is the fresh figure and dry_harvest_kg_ holds the dry kilograms.
# Elemental N/P/K (kg/ha) are computed only when every listed product has a known fraction: urea 46% N, DAP 18% N and 20.1% P, CAN 26% N (terminag fertilizer table). Generic NPK and "Other (Specify)" block the calculation. "No fertilizer" on both slots is 0.
# "Interncropping" is the source spelling of intercropping. Each row is one crop component, so crop is not collapsed into an intercrop name. Sweet banana -> banana, potatoes -> potato, caffee -> coffee, field peas -> pea.

## ISSUES
# No coordinates. District codes 1-7 and village codes need a lookup before georeferencing. Do not treat the codes as GADM names.
# crop_area_ha is 0 on most rows (340 of 4074 are > 0), so many harvests have no yield. Confirm whether 0 means "not recorded".
# Organic input types are stored in OM_type only where they match the vocabulary (compost, farmyard manure for "Animal manure"). Crop residues, household refuse, biomass transfer and fallow are not in the OM list and are left in organic_input_.
# Non-crop rows (wood, firewood, forages, leaves, others) were dropped.
# Country code 2 = Burundi is inferred from the dataset, not from a codebook.
# No publication DOI.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude; unknown location_code_, dry_harvest_kg_, fertilizer_product_kg_, organic_input_, district_code_, sector_code_; low yield for coffee, groundnut, lettuce, onion, orange, pumpkin, sweetpotato, tobacco; NA yield, yield_part.

## NEW VARIABLES
# district_code_: District_Cefferie_Commune (numeric code)
# sector_code_: Sector_Groupment_Zone (numeric code)
# location_code_: Village_Sun_colline (numeric code)
# dry_harvest_kg_: T_harvst_dry when a fresh harvest was used for yield
# organic_input_: organic input text that is not an accepted OM_type
# fertilizer_product_kg_: sum of Qty_fert_kg and Qty_fert2_used (kg of product on the plot)

## NOT PROCESSED
# agricultural_activities.csv, asset_roaster.csv, asset_roaster2.csv, comments.csv, communal_property_resources.csv, credit_source.csv.
# crop_harvestd_utilizatn.csv and utilzationshortrainyseason.csv: a second harvest/sales table (quantities sold, consumed, fed). Not merged, to avoid double counting with input_use harvests.
# fruit_beverages.csv, household_diseases.csv, meat_animalproducts.csv, staple_food.csv, vegetable_fat_others.csv, month_food_scarcity.csv, nonfood_items*.csv, other_income.csv: consumption, income, nutrition.
# hh_compostion_characteristic*.csv: household roster.
# improved_crop_management.csv, improved_crop_varieties.csv, ipm.csv: technology awareness, not this season's plot harvest.
# land_use_management.csv, land_use_mgt_short_rainy.csv: parcel area and soil conservation, not crop yield. Plot_area_ha was not merged because the link to input_use plot ids is a code.
# livestock_ownership.csv, livestock_products.csv, other_livestock.csv.
# input_short_rainy_season.csv: short-rains input module, not processed (same reason as the utilisation file: separate season table, needs a decision on stacking).
# platform_type.csv, warrantage_use.csv, market_access metadata only.
# input_use.csv columns not used: Percent_area, seed quantities and cost, Dec_maker, persn_wrkn_plot, pesticide and herbicide quantities, hired labour, pest/disease stress, crop-residue use.


carob_script <- function(path) {

"HumidTropic Baseline and Impact assessment of CIALCA project in Burundi

'IITA is implementing a baseline survey of Humid Tropic program in Rwanda, Congo and Burundi. The survey will serve not only as the baseline against which the HumidTropic program could be evaluated but will also serve as avenue to evaluate the impact of CIALCA project implemented since 2006 in the same area. The idea is to go back to some of the farmers that were interviewed in 2006 and see changes that have happened in their agricultural livelihoods and wellbeing after about 8 years of CIALCA implementation. 

The objectives of this activity were: (1) to insure that the households surveyed in 2006 are still living in their respective villages and are available for the survey in preparation. (2) To identify a group of about 12 farmers that are supposed to know CIALCA technologies so that they participate in Focus Group Discussions to be organised at the village level.'
"

	uri <- "doi:10.25502/9x39-9768/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "CIALCA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	r <- read.csv(ff[basename(ff) == "input_use.csv"], stringsAsFactors=FALSE)

	z <- tolower(trimws(r$crops_grown))
	crop <- z
	crop[z %in% c("beans", "bean")] <- "common bean"
	crop[z == "sweet banana"] <- "banana"
	crop[z == "banana"] <- "banana"
	crop[z %in% c("potatoes", "potato")] <- "potato"
	crop[z %in% c("sweet potatoes", "sweet potato")] <- "sweetpotato"
	crop[z == "field peas"] <- "pea"
	crop[z == "groundnut"] <- "groundnut"
	crop[z == "sorghum"] <- "sorghum"
	crop[z == "caffee"] <- "coffee"
	crop[z == "egg plant"] <- "eggplant"
	crop[z %in% c("tomatoes", "tomato")] <- "tomato"
	crop[z == "maize"] <- "maize"
	crop[z == "cassava"] <- "cassava"
	crop[z == "rice"] <- "rice"
	crop[z == "cowpea"] <- "cowpea"
	crop[z == "cabbage"] <- "cabbage"
	crop[z == "onion"] <- "onion"
	crop[z == "tea"] <- "tea"
	crop[z == "pear"] <- "pear"
	crop[z %in% c("mango", "mangoes")] <- "mango"
	crop[z %in% c("letuce", "lettuce")] <- "lettuce"
	crop[z %in% c("peper", "pepper", "black pepper")] <- "chili pepper"
	crop[z %in% c("sugar cane", "sugarcane")] <- "sugarcane"
	crop[z %in% c("tef", "teff")] <- "teff"
	drop <- z %in% c("", "na", "others", "other", "wood", "firewood", "forages", "leaves")
	crop[drop] <- NA

	area <- r$crop_area_ha
	fresh <- r$T_harvst_fresh
	dry <- r$T_harvst_dry
	use_fresh <- !is.na(fresh) & fresh > 0
	kg <- ifelse(use_fresh, fresh, dry)
	yield <- ifelse(!is.na(area) & area > 0 & !is.na(kg), kg / area, NA_real_)
	dry_keep <- ifelse(use_fresh & !is.na(dry) & dry > 0, dry, NA_real_)

	f1 <- trimws(r$Inorg_fert1_used)
	f2 <- trimws(r$Inorg_fert2_used)
	q1raw <- r$Qty_fert_kg
	q2raw <- r$Qty_fert2_used
	q1 <- q1raw
	q2 <- q2raw
	q1[is.na(q1)] <- 0
	q2[is.na(q2)] <- 0
	product_kg <- ifelse(is.na(q1raw) & is.na(q2raw), NA_real_, q1 + q2)
	none1 <- f1 == "No fertilizer" | f1 == "" | is.na(f1)
	none2 <- f2 == "No fertilizer" | f2 == "" | is.na(f2)

	n_frac <- function(lab) {
		ifelse(lab == "UREA", 0.46, ifelse(lab == "Diammonium Phosphate (DAP)", 0.18, ifelse(lab == "Calcium Ammonium nitrate (CAN)", 0.26, NA_real_)))
	}
	p_frac <- function(lab) {
		ifelse(lab == "Diammonium Phosphate (DAP)", 0.201, ifelse(lab %in% c("UREA", "Calcium Ammonium nitrate (CAN)"), 0, NA_real_))
	}
	known <- function(lab) lab %in% c("UREA", "Diammonium Phosphate (DAP)", "Calcium Ammonium nitrate (CAN)", "No fertilizer", "")

	blocked <- (!none1 & !known(f1)) | (!none2 & !known(f2))
	N <- ifelse(blocked, NA_real_, 0)
	P <- ifelse(blocked, NA_real_, 0)
	K <- ifelse(blocked, NA_real_, 0)
	has_area <- !is.na(area) & area > 0
	addN1 <- ifelse(has_area, q1 * n_frac(f1) / area, 0)
	addP1 <- ifelse(has_area, q1 * p_frac(f1) / area, 0)
	addN2 <- ifelse(has_area, q2 * n_frac(f2) / area, 0)
	addP2 <- ifelse(has_area, q2 * p_frac(f2) / area, 0)
	addN1[is.na(addN1)] <- 0
	addP1[is.na(addP1)] <- 0
	addN2[is.na(addN2)] <- 0
	addP2[is.na(addP2)] <- 0
	N[!blocked] <- addN1[!blocked] + addN2[!blocked]
	P[!blocked] <- addP1[!blocked] + addP2[!blocked]

	fname <- function(lab) {
		ifelse(lab == "UREA", "urea",
			ifelse(lab == "Diammonium Phosphate (DAP)", "DAP",
			ifelse(lab == "Calcium Ammonium nitrate (CAN)", "CAN",
			ifelse(lab == "NPK", "NPK", NA_character_))))
	}
	t1 <- fname(f1)
	t2 <- fname(f2)
	fert <- ifelse(!is.na(t1) & !is.na(t2) & t1 != t2, paste(t1, t2, sep=";"), ifelse(!is.na(t1), t1, t2))
	fert[none1 & none2] <- "none"

	om <- trimws(r$organic_input_type)
	om_type <- rep(NA_character_, nrow(r))
	om_type[om == "Compost"] <- "compost"
	om_type[om == "Animal manure"] <- "farmyard manure"
	om_used <- !is.na(om) & !(om %in% c("", "Natural fallow", "Improved fallow"))
	organic_input_ <- ifelse(is.na(om_type) & om_used, om, NA_character_)

	d <- data.frame(
		hhid = as.character(r$ID),
		plot_id = as.character(r$Plt_id),
		crop = crop,
		variety_type = tolower(trimws(r$Crop_variety)),
		field_size = area,
		yield = yield,
		dry_harvest_kg_ = dry_keep,
		fertilizer_type = fert,
		fertilizer_product_kg_ = product_kg,
		N_fertilizer = N,
		P_fertilizer = P,
		K_fertilizer = K,
		OM_used = om_used,
		OM_type = om_type,
		organic_input_ = organic_input_,
		country = "Burundi",
		district_code_ = as.character(r$District_Cefferie_Commune),
		sector_code_ = as.character(r$Sector_Groupment_Zone),
		location_code_ = as.character(r$Village_Sun_colline),
		stringsAsFactors = FALSE
	)
	d$variety_type[d$variety_type %in% c("", "na")] <- NA
	d$variety_type[d$variety_type == "local"] <- "landrace"
	d$variety_type[d$variety_type == "improved"] <- "improved"
	d <- d[!is.na(d$crop), ]

	grain <- c("maize", "sorghum", "rice")
	seed <- c("common bean", "pea", "groundnut", "cowpea", "coffee")
	d$yield_part <- NA_character_
	d$yield_part[d$crop %in% grain] <- "grain"
	d$yield_part[d$crop %in% seed] <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato")] <- "roots"
	d$yield_part[d$crop == "potato"] <- "tubers"
	d$yield_part[d$crop %in% c("banana", "tomato", "eggplant", "pear", "mango", "chili pepper")] <- "fruit"
	d$yield_part[d$crop %in% c("cabbage", "tea", "lettuce")] <- "leaves"
	d$yield_part[d$crop == "sugarcane"] <- "stems"
	d$yield_part[d$crop == "teff"] <- "grain"

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
