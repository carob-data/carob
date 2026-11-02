# R script for "carob"
# license: GPL (>=3)

## NOTES
# Focal adaptation trial of climbing bean, season I 2015. experiment.csv is the trial; general.csv is the household.
# production.csv, nutrition, labour, livestock and constraints files are empty of crop data and were not used.
# One row for the N2Africa plot and, where the farmer planted one, a second row for the farmer's own legume plot.
# Area of the N2Africa plot is m2 (area_of_n2africa_plot). Own-plot area is m2 when the unit is meter_squa, and acres converted to m2 (1 acre = 4046.8564224 m2) when the unit is acre.
# Fertilizer type is recorded for the N2Africa plot only. No product amount is in the file, so N/P/K are 0 only when the type is None.
# District names are Ugandan (Kabale, Kanungu, Kapchorwa). Country is set to Uganda. Two rows are coded ethiopia or nigeria in general.csv.
# Coordinates come from the field GPS. Negative latitudes at Kabale and Kanungu are south of the equator and were kept.

## ISSUES
# No grain or biomass harvest was recorded, so yield cannot be calculated.
# Fertilizer product amounts are absent, so DAP/TSP/NPK/other cannot be converted to N/P/K.
# Inoculant use this season is not a column (only previous-season inoculant, all "n").
# Control plots: 9 farms, area_of_control_plot unit in the dictionary is "m" while values look like areas. Not mapped.
# sector_ward often lists several wards in one cell and was stored as location without splitting.
# One record is district Kampala with coordinates like Kapchorwa.
# Stake lengths and plants per stake (five subsamples) were not reshaped to a long table.
# Household production.csv is entirely blank.
# farm_id FB_ADAP_UG_KAN307 and FB_ADAP_UG_BEAN_388 are each entered twice, with the same values. The household table was reduced to one row per farm. The repeated experiment rows were kept, so write_files still reports duplicate records.
# write_files: missing carob_contributor, yield, yield_moisture, irrigated, planting_date, harvest_date; unknown staking_method_, intercrop_crop_, n_stakes_4m2_, plant_survival_; plot_area about 5 to 363570 and plant_spacing 4 to 155; fertilizer_type "other"; duplicate coordinates; duplicate records; NA latitude and longitude.

## NEW VARIABLES
# staking_method_: staking method recorded for that plot.
# n_stakes_4m2_: number of stakes counted on a 2 x 2 m area.
# plant_survival_: survival class (for example 75-100%).
# intercrop_crop_: other crops in the legume plot, as recorded.

## NOT PROCESSED
# experiment.csv: reasons for practices, soil depth points, drainage, slope, ownership, stake lengths and plants per stake (five each), plot width and length (area is kept), control-plot size, previous-season fertilizer amounts (types of previous fertilizer are not rates).
# general.csv: household composition, education, demo-visit flag. Raw country values ethiopia/nigeria were not used (see ISSUES).
# labour_income_and_assets.csv, land_crops_and_livestock.csv, nutrition.csv: household income, herd size, food shortage.
# production.csv: field crops, areas and harvests are all missing.
# production_constraints.csv: all missing.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa focal adapt trial - Uganda, 2015, season I climbing bean

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/s5zv-p584"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = "on-farm demonstration: N2Africa plot and farmer legume plot",
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "plant_survival_",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "experiment.csv"]
	f2 <- ff[basename(ff) == "general.csv"]
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r2 <- read.csv(f2, na.strings = c("", "NA"))

	loc <- carobiner::fix_name(r2$sector_ward, "title")
	loc[which(loc == "")] <- NA
	g <- data.frame(
		trial_id = as.character(r2$farm_id),
		country = "Uganda",
		adm2 = carobiner::fix_name(r2$lga_district_woreda, "title"),
		location = loc,
		farmer_gender = tolower(trimws(r2$gender_of_farmer))
	)
	g <- g[!duplicated(g$trial_id), ]

	ft <- trimws(r1$mineral_fertilizer_type)
	ft[which(tolower(ft) == "none")] <- "none"
	ft[which(tolower(ft) == "dap")] <- "DAP"
	ft[which(tolower(ft) == "tsp")] <- "TSP"
	ft[which(tolower(ft) == "npk")] <- "NPK"
	prev <- tolower(gsub("_", " ", trimws(r1$crop_1_previous_season)))
	prev <- carobiner::replace_values(prev,
		c("bush bean", "climbing bean", "irish potato", "maize", "banana", "millet", "sorghum", "coffee", "cassava", "cowpea", "sweet potato"),
		c("common bean", "common bean", "potato", "maize", "banana", "millet", "sorghum", "coffee", "cassava", "cowpea", "sweetpotato"),
		FALSE)
	known <- c("common bean", "potato", "maize", "banana", "millet", "sorghum", "coffee", "cassava", "cowpea", "sweetpotato")
	prev[which(!prev %in% known)] <- NA

	ic <- trimws(r1$with_which_crops_did_the_farmer_intercrop_n2a_plot)
	ic[which(r1$intercropping_n2a_plot != "y")] <- NA
	ic[which(ic == "")] <- NA

	n2 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = "n2africa",
		treatment = "n2africa",
		crop = "common bean",
		variety_type = "climbing bean",
		variety = trimws(r1$legume_variety),
		fertilizer_type = ft,
		plot_area = r1$area_of_n2africa_plot.m2,
		row_spacing = r1$row_spacing_n2africa_plot.cm,
		plant_spacing = r1$plant_spacing_n2africa_plot.cm,
		latitude = r1$gps_latitude_fld.decimal_degrees,
		longitude = r1$gps_longitude_fld.decimal_degrees,
		elevation = r1$gps_altitude_fld.m,
		previous_crop = prev,
		staking_method_ = trimws(r1$which_staking_method_did_the_farmer_use_n2a_plot),
		n_stakes_4m2_ = r1$number_of_stakes_on_2x2_m_n2africa_plot,
		plant_survival_ = trimws(r1$plant_survival_n2africa_plot),
		intercrop_crop_ = ic
	)
	n2$variety[which(n2$variety == "")] <- NA
	n2$staking_method_[which(n2$staking_method_ == "")] <- NA
	n2$plant_survival_[which(n2$plant_survival_ == "")] <- NA
	n2$row_spacing[which(n2$row_spacing == 0)] <- NA
	n2$plant_spacing[which(n2$plant_spacing == 0)] <- NA

	own_crop <- tolower(trimws(r1$which_legume_species_did_the_farmer_plant))
	vtype <- ifelse(own_crop == "bush_bean", "bush bean",
		ifelse(own_crop == "climbing_bean", "climbing bean", NA_character_))
	unit <- tolower(trimws(r1$unit_of_own_legume_plot_area))
	own_m2 <- rep(NA_real_, nrow(r1))
	i_m <- which(unit == "meter_squa")
	i_a <- which(unit == "acre")
	own_m2[i_m] <- r1$area_of_own_legume_plot[i_m]
	own_m2[i_a] <- r1$area_of_own_legume_plot[i_a] * 4046.8564224
	ic2 <- trimws(r1$with_which_crops_did_the_farmer_intercrop_own_legume_plot)
	ic2[which(r1$intercropping_own_legume_plot != "y")] <- NA
	ic2[which(ic2 == "")] <- NA
	own <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = "farmer",
		treatment = "farmer",
		crop = ifelse(own_crop %in% c("bush_bean", "climbing_bean"), "common bean", NA_character_),
		variety_type = vtype,
		variety = trimws(r1$which_legume_variety_did_the_farmer_plant),
		fertilizer_type = NA_character_,
		plot_area = own_m2,
		row_spacing = r1$row_spacing_own_legume_plot_cm.cm,
		plant_spacing = r1$plant_spacing_own_legume_plot_cm.cm,
		latitude = r1$gps_latitude_fld.decimal_degrees,
		longitude = r1$gps_longitude_fld.decimal_degrees,
		elevation = r1$gps_altitude_fld.m,
		previous_crop = NA_character_,
		staking_method_ = trimws(r1$which_staking_method_did_the_farmer_use_own_legume_plot),
		n_stakes_4m2_ = r1$number_of_stakes_on_2x2_m_own_legume_plot,
		plant_survival_ = trimws(r1$plant_survival_own_legume_plot),
		intercrop_crop_ = ic2
	)
	own$variety[which(own$variety == "")] <- NA
	own$staking_method_[which(own$staking_method_ == "")] <- NA
	own$plant_survival_[which(own$plant_survival_ == "")] <- NA
	own$row_spacing[which(own$row_spacing == 0)] <- NA
	own$plant_spacing[which(own$plant_spacing == 0)] <- NA
	planted <- which(r1$did_the_farmer_plant_his_her_own_legume == "y")
	d <- rbind(n2, own[planted, ])
	d <- merge(d, g, by = "trial_id", all.x = TRUE)

	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	none <- !is.na(d$fertilizer_type) & d$fertilizer_type == "none"
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0

	d$yield_part <- "seed"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)

	carobiner::write_files(path, meta, d)
}
