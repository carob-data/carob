# R script for "carob"
# license: GPL (>=3)

## NOTES
# N2Africa on-farm groundnut monitoring, Oyam district (Minakulu action site), Uganda, season 2013B (title: Season II).
# Plot rows from c_use_of_package_1 joined to e_harvest (240 plots, 60 farms, 4 plots each).
# crop is empty; varieties are Serenut 5 and Serenut 6 and e_harvest crop_1_grain_unshelled is Y, so crop is groundnut and yield_part is pod.
# plot_size is "10M X 5M" and area_harvested_m2 is 50. Yield is weight_kg / 50 m2, as kg/ha of unshelled pods. weight_grain is empty.
# Fertilizer amounts are kg product per plot. "o.5" is read as 0.5 (letter o). TSP+gypsum strings are TSP then gypsum.
# Element contents from terminag values_fertilizer_type: TSP 19.23% P, 2% S, 14% Ca; gypsum 19% S, 22% Ca. Neither product lists N or K, so those rates are 0.
# Control / control with amount 0 is fertilizer_type "none" and nutrient rates 0. No organic fertilizer was recorded (organic dates are 0).
# inoculant_used and chemicals_used are N on every plot.
# Dates in d_cropping_calendar are one row per farm, repeated on that farm's four plots. Planting is September 2013; harvest is December 2013 or January 2014.
# Row spacing 45 cm and plant spacing 10 cm from c_use_of_package_3. Three weeding dates on every farm.
# farm_size_ha is 0; farm_size_acre is converted with 1 acre = 0.4046863 ha.
# gps_latitude and gps_longitude are empty. gps_altitude is 3381-3517 with no unit (implausible as metres for Oyam) and is not used as elevation.

## ISSUES
# No plot coordinates. Georeference villages in Oyam, Minakulu action site: Adwar, Agungu arac, Agungu araca, Alati B, Omolo, Zericho. Parish/sector is Atek (58 farms), Zericho (1), and one farm with sector and village blank. Dataset JSON coverage_x/coverage_y (1.37333, 32.29028) is one dataset point, not used.
# One TSP plot is 1.38 kg; the other TSP plots are 0.38 kg. Left as recorded.
# gps_altitude not converted (unit not stated).
# previous crop "peas" mapped to pea; in this region it may be pigeon pea. "Simsim/sorghum" and "Cotton/beans" kept as two crops in previous_crop.
# Two farms have harvest 4 Jan 2013 and 13 Jan 2013, before planting in September 2013 (8 plot rows). One farm has harvest 30 Dec 2014, 466 days after planting (4 plot rows). Dates left as recorded.
# Farmer assessment scores are 1-3 with no legend.
# chemicals_used is only Y/N; no product, so herbicide/insecticide/fungicide are FALSE rather than named products.
# write_files: missing carob_contributor, yield_moisture, geo_from_source, latitude, longitude, irrigated.
# write_files: unknown variables germination_class_ (a class, not germination_rate), yield_score_, labour_score_, weediness_score_, pest_score_, overall_score_, earlier_crop_, soil_fertility_farmer_, soil_drainage_farmer_, farm_area_ha_, soil_conservation_.
# write_files: datespan, 8 plot rows with harvest before or within 45 days of planting, and 4 plot rows with harvest more than 366 days later (see above).

## NOT PROCESSED
# a_family_composition.csv: age-sex counts of household members. Not crop production.
# a_inputs.csv: input, quantity. Farm-level purchases; variable_definitions units are blank, so rates were not computed.
# a_livestock.csv: livestock type and count.
# a_technician.csv: enumerator name, position, date.
# b_conservation_structure.csv: conservation_method (separate from the Y/N flag on b_info_site_1).
# b_info_site_1.csv: signs_of_soil_erosion_plot is N on every farm. fertility_n2africa_plot is a comparison with other fields, not a rate.
# b_info_site_2.csv: last-season fertilizer and inoculant type columns are empty. Crop before last season is earlier_crop_.
# c_use_of_package_2.csv: chemical and inoculant storage columns are empty (no chemicals and no inoculant).
# c_use_of_package_3.csv: crop_2 spacing and germination are empty (no second crop). crop_1_planted_in_rows is Y on every plot.
# d_cropping_calendar.csv: land preparation and organic-fertilizer dates are 0. Comment fields. Mineral-fertilizer application date is present but there is no fertilizer-date term; nutrients are taken from the amount columns.
# e_harvest.csv: weight_grain is empty. crop_1_grain_unshelled is Y and is the reason yield_part is pod.
# f_farmer_assessment.csv: additional_remarks free text.
# g_treatments_1.csv, g_treatments_2.csv, g_treatments_lead_farmer.csv, g_treatments_n2africa_plot.csv: training and opinion questions, not plot treatments.
# variable_definitions.csv: units and codes are blank.
# a_general.csv: legumes grown outside the trial, labour, education, age, household-head fields. gps_altitude (see ISSUES).

## NEW VARIABLES
# germination_class_: c_use_of_package_3 crop_1_germination, recorded as a class (50-75%, 75-90%, >90%), not a percent.
# yield_score_, labour_score_, weediness_score_, pest_score_, overall_score_: f_farmer_assessment scores (1-3, scale not defined).
# soil_fertility_farmer_: farmer rating (poor, moderate, fertile). Not a soil test.
# soil_drainage_farmer_: Good/Moderate/Poor as recorded. Not the USDA drainage classes.
# land_tenure: "family" or "hired" (from "Family land" / "Hired land").
# earlier_crop_: crop two seasons before the trial (b_info_site_2 main_crop_before_last_season).
# soil_conservation_: TRUE/FALSE from soil_conservation_structures Y/N.
# farm_area_ha_: whole-farm area from farm_size_acre (the hectare column is 0).


carob_script <- function(path) {

"
N2Africa farm monitoring - Uganda, 2013, Season II Groundnut

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/zzzd-d180"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;fertilizer_type",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 1
	)

	f0 <- ff[basename(ff) == "a_general.csv"]
	f1 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	f3 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	f5 <- ff[basename(ff) == "e_harvest.csv"]
	f6 <- ff[basename(ff) == "b_info_site_2.csv"]
	f7 <- ff[basename(ff) == "b_info_site_1.csv"]
	f8 <- ff[basename(ff) == "f_farmer_assessment.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r3 <- read.csv(f3, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))
	r6 <- read.csv(f6, na.strings = c("", "NA"))
	r7 <- read.csv(f7, na.strings = c("", "NA"))
	r8 <- read.csv(f8, na.strings = c("", "NA"))

	loc <- gsub("['\"]", "", trimws(r0$vilage))
	loc <- gsub("\\s+", " ", loc)
	loc[loc == ""] <- NA

	adm3 <- trimws(r0$sector_ward)
	adm3 <- gsub("(?i)\\s*parish$", "", adm3, perl = TRUE)
	adm3[adm3 == ""] <- NA

	d0 <- data.frame(
		trial_id = r0$farm_id,
		country = "Uganda",
		adm2 = "Oyam",
		adm3 = carobiner::fix_name(adm3, "title"),
		location = carobiner::fix_name(loc, "title"),
		season = "second",
		farmer_gender = tolower(trimws(r0$gender_of_farmer)),
		farm_area_ha_ = as.numeric(r0$farm_size_acre) * 0.4046863
	)
	d0$adm3[is.na(adm3)] <- NA
	d0$location[is.na(loc)] <- NA

	ft <- trimws(r1$min_fertilizer_type)
	amt <- tolower(trimws(r1$min_fertiliser_amount_kg))
	amt <- gsub("o\\.", "0.", amt)
	amt <- gsub("kg", "", amt)
	amt <- gsub(" ", "", amt)
	first_n <- as.numeric(sub("\\+.*", "", amt))
	second_n <- rep(NA_real_, length(amt))
	has2 <- grepl("\\+", amt, fixed = FALSE)
	second_n[has2] <- as.numeric(sub(".*\\+", "", amt[has2]))

	is_none <- tolower(ft) == "control"
	is_tsp <- ft == "TSP"
	is_gyp <- ft == "Gypsum"
	is_both <- grepl("TSP", ft) & grepl("Gypsum", ft)

	tsp_kg <- rep(0, nrow(r1))
	gyp_kg <- rep(0, nrow(r1))
	tsp_kg[is_tsp | is_both] <- first_n[is_tsp | is_both]
	gyp_kg[is_gyp] <- first_n[is_gyp]
	gyp_kg[is_both] <- second_n[is_both]

	fertilizer_type <- rep(NA_character_, nrow(r1))
	fertilizer_type[is_none] <- "none"
	fertilizer_type[is_tsp] <- "TSP"
	fertilizer_type[is_gyp] <- "gypsum"
	fertilizer_type[is_both] <- "TSP; gypsum"

	d1 <- data.frame(
		trial_id = r1$farm_id,
		plot_id = as.character(r1$plot_no),
		crop = "groundnut",
		variety = trimws(r1$variety),
		treatment = paste(trimws(r1$variety), fertilizer_type, sep = "; "),
		fertilizer_type = fertilizer_type,
		inoculated = FALSE,
		herbicide_used = FALSE,
		insecticide_used = FALSE,
		fungicide_used = FALSE,
		OM_used = FALSE,
		OM_type = "none",
		OM_amount = 0,
		tsp_kg = tsp_kg,
		gyp_kg = gyp_kg
	)

	d3 <- data.frame(
		trial_id = r3$farm_id,
		plot_id = as.character(r3$plot_no),
		row_spacing = as.numeric(r3$crop_1_spacing_row_to_row),
		plant_spacing = as.numeric(r3$crop_1_spacing_plant_to_plant),
		germination_class_ = trimws(r3$crop_1_germination)
	)

	ymd <- function(dd, mm, yyyy) {
		dd <- as.integer(dd)
		mm <- as.integer(mm)
		yyyy <- as.integer(yyyy)
		ok <- !is.na(yyyy) & yyyy > 1900 & !is.na(mm) & mm >= 1 & mm <= 12 & !is.na(dd) & dd >= 1 & dd <= 31
		out <- rep(NA_character_, length(yyyy))
		out[ok] <- sprintf("%04d-%02d-%02d", yyyy[ok], mm[ok], dd[ok])
		out
	}
	w1 <- ymd(r4$date_weeding_1_dd, r4$date_weeding_1_mm, r4$date_weeding_1_yyyy)
	w2 <- ymd(r4$date_weeding_2_dd, r4$date_weeding_2_mm, r4$date_weeding_2_yyyy)
	w3 <- ymd(r4$date_weeding_3_dd, r4$date_weeding_3_mm, r4$date_weeding_3_yyyy)
	d4 <- data.frame(
		trial_id = r4$farm_id,
		planting_date = ymd(r4$date_planting_dd, r4$date_planting_mm, r4$date_planting_yyyy),
		harvest_date = ymd(r4$date_harvest_dd, r4$date_harvest_mm, r4$date_harvest_yyyy),
		weeding_times = 3L,
		weeding_dates = paste(w1, w2, w3, sep = "; ")
	)

	d5 <- data.frame(
		trial_id = r5$farm_id,
		plot_id = as.character(r5$plot_no),
		plot_area = as.numeric(r5$area_harvested_m2),
		weight_kg = as.numeric(r5$weight_kg)
	)

	map_prev <- function(x) {
		x <- trimws(tolower(x))
		x[x %in% c("bean", "beans")] <- "common bean"
		x[x %in% c("groundnut", "groundnuts", "gnut", "gnuts")] <- "groundnut"
		x[x %in% c("cowpea", "cowpeas")] <- "cowpea"
		x[x == "simsim"] <- "sesame"
		x[x == "fallow"] <- "none"
		x[x == "simsim/sorghum"] <- "sesame; sorghum"
		x[x == "cotton/beans"] <- "cotton; common bean"
		x[x == "peas"] <- "pea"
		x[x == ""] <- NA
		x
	}
	prev <- map_prev(r6$main_crop_last_season)
	bef <- map_prev(r6$main_crop_before_last_season)
	d6 <- data.frame(
		trial_id = r6$farm_id,
		previous_crop = prev,
		earlier_crop_ = bef
	)

	ten <- trimws(r7$owner_trial_plot)
	ten[ten == "Family land"] <- "family"
	ten[ten == "Hired land"] <- "hired"
	d7 <- data.frame(
		trial_id = r7$farm_id,
		land_tenure = ten,
		soil_fertility_farmer_ = tolower(trimws(r7$perception_soil_fertility_farmer)),
		soil_drainage_farmer_ = tolower(trimws(r7$soil_drainage)),
		soil_conservation_ = toupper(trimws(r7$soil_conservation_structures)) == "Y"
	)

	d8 <- data.frame(
		trial_id = r8$farm_id,
		plot_id = as.character(r8$plot_no),
		yield_score_ = as.integer(r8$yield),
		labour_score_ = as.integer(r8$labour),
		weediness_score_ = as.integer(r8$weediness),
		pest_score_ = as.integer(r8$pests_diseases),
		overall_score_ = as.integer(r8$overall)
	)

	d <- merge(d1, d5, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d3, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d8, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d4, by = "trial_id", all.x = TRUE)
	d <- merge(d, d6, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)

	# kg product per plot -> kg nutrient / ha. Plot area is m2.
	per_ha <- 10000 / d$plot_area
	d$N_fertilizer <- 0
	d$K_fertilizer <- 0
	d$P_fertilizer <- d$tsp_kg * per_ha * 0.1923
	d$S_fertilizer <- d$tsp_kg * per_ha * 0.02 + d$gyp_kg * per_ha * 0.19
	d$Ca_fertilizer <- d$tsp_kg * per_ha * 0.14 + d$gyp_kg * per_ha * 0.22
	d$yield <- d$weight_kg / d$plot_area * 10000
	d$tsp_kg <- NULL
	d$gyp_kg <- NULL
	d$weight_kg <- NULL

	d$yield_part <- "pod"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$trial_id <- as.character(d$trial_id)

	carobiner::write_files(path, meta, d)
}
