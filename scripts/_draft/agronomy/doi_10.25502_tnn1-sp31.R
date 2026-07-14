# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
N2Africa diagnostic trial - Eastern Uganda, 2014, Season II

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.25502/tnn1-sp31"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

## Non-metadata .json files in ff (e.g. nested Dataverse Dataset/*.json). Parsed with jsonlite::fromJSON.
## Optional string snapshots: lapply(json_list, function(x) jsonlite::toJSON(x, pretty=TRUE, auto_unbox=TRUE))
	jmeta <- paste0(yuri::simpleURI(uri), ".json")
	json_paths <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_paths <- json_paths[!tolower(basename(json_paths)) %in% tolower(c(jmeta, "metadata.json"))]
	json_list <- if (length(json_paths) > 0) {
		stats::setNames(lapply(json_paths, jsonlite::fromJSON), basename(json_paths))
	} else {
		list()
	}

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "IITA; World Agroforestry Centre (ICRAF); WUR; International Institute of Tropical Agriculture (IITA), Wageningen University",
		publication = NA,
		project = "N2Africa",
		# if available report the experimental or survey design
		design = "on-farm diagnostic trial, to evaluate groundnut variety and P/S fertiliser response",
		# SERENUT 5 & 6 and the presence of TSP/GYPSUM fertiliser 
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "on-farm experiment",
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "variety;P_fertilizer;S_fertilizer",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "yield; dmy_total", 

		# notes for the end-user
		notes = "yield was derived from unshelled groundnut wight in kg/plot and converted to kg/ha. in the raw data plots 5-8 are mostly empty, so only treatments will be included.",

		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-13",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "experiment.csv"]
	f2 <- ff[basename(ff) == "general.csv"]
	f3 <- ff[basename(ff) == "labour_income_and_assets.csv"]
	f4 <- ff[basename(ff) == "land_crops_and_livestock.csv"]
	f5 <- ff[basename(ff) == "variable_definitions.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)

## select the variables of interest and assign them to the correct name
	browser() #temp remove when done
	make_plot <- function(r1, i){ # constructing a function to extract data for each experimental plot
		name <- r1[[paste0("name_treatment_", i)]]
		desc <- r1[[paste0("description_treatment_", i)]]
		#width <- as.numeric(r1[[paste0("width_of_harvested_plot_crop_1_plot_", i)]])
		width_col <- grep( 
			paste0("width_of_harvested_plot_crop_1_plot_", i), names(r1), value = TRUE)
			width <- as.numeric(r1[[width_col[1]]])
			pod_col <- grep(
			paste0("pod_weight.*crop_1.*plot_", i, "\\.kg$"),
			names(r1),
			value = TRUE
		)
		podweight <- as.numeric(r1[[pod_col[1]]])
		depth <- as.numeric(r1[[paste0("depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_", i, ".m")]])
		num_rows <- as.numeric(r1[[paste0("number_of_rows_in_plot_crop_1_plot_", i)]])
		#podweight <- as.numeric(r1[[paste0("pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_", i, ".kg")]])
		agbio <- as.numeric(r1[[paste0("above_ground_biomass_weight_husks_stover_res_crop_1_plot_", i, ".kg")]])
		fert1 <- as.numeric(r1[[paste0("fert_1_kg_plot_plot_", i, ".kg_per_plot")]]) # for tsp
		fert2 <- as.numeric(r1[[paste0("fert_2_kg_plot_plot_", i, ".kg_per_plot")]]) # for gypsum
		inoc <- r1[[paste0("inoculant_y_n_plot_", i)]]
		performance <- r1[[paste0("relative_performance_plot_", i, "_yield")]]	
		germ_crop <- r1[[paste0("germination_crop_1_treatment_", i)]]

		data.frame(
			id = r1[["id"]],
			farm_id = r1[["farm_id"]],
			treatment = name,
			treatment_description = desc,
			plot_num= i,
			variety = r1[["experimental_treatments_variety_crop_1"]],
			plot_width_m = width,
			plot_depth_m = depth,
			plot_area_m2 = width * depth,
			num_rows = num_rows,
			pod_weight_kg = podweight,
			biomass_kg = agbio,
			TSP_fert_kg = fert1,
			Gypsum_fert_kg = fert2,
			inoculated = tolower(inoc),
			yield = performance,
			germination_crop = germ_crop,
			stringsAsFactors = FALSE
		)
	}

	p1 <- make_plot(r1,1)
	p2 <- make_plot(r1,2)
	p3 <- make_plot(r1,3)
	p4 <- make_plot(r1,4)
	d1 <- rbind(p1, p2, p3, p4)
	#d1 <- do.call(rbind, lapply(1:4, function(i) make_plot(r1, i))) # creating plot records for treatments 1-4 and stacking them
##r1: "SN", "id", "farm_id", "experiment_id", "name_treatment_2", "name_treatment_3", "name_treatment_4", "name_treatment_5", "name_treatment_6", "name_treatment_7", "name_treatment_8", "description_treatment_1", "description_treatment_2", "description_treatment_3", "description_treatment_4", "description_treatment_5", "description_treatment_6", "description_treatment_7", "description_treatment_8", "inoculation_used_in_n2africa_field", "inoculation_stored", "if_yes_how_was_it_stored", "time_stored_days", "time_stored_weeks", "time_stored_months", "days_between_inoculant_mixing_and_planting", "hours_between_inoculant_mixing_and_planting", "minutes_between_inoculant_mixing_and_planting", "germination_crop_1_treatment_2", "germination_crop_1_treatment_3", "germination_crop_1_treatment_4", "germination_crop_1_treatment_5", "germination_crop_1_treatment_6", "germination_crop_1_treatment_7", "germination_crop_1_treatment_8", "germination_crop_2_treatment_1", "germination_crop_2_treatment_2", "germination_crop_2_treatment_3", "germination_crop_2_treatment_4", "germination_crop_2_treatment_5", "germination_crop_2_treatment_6", "germination_crop_2_treatment_7", "germination_crop_2_treatment_8", "relative_performance_plot_1_weeds", "relative_performance_plot_1_pests_diseases", "relative_performance_plot_1_why", "relative_performance_plot_2_labour", "relative_performance_plot_2_weeds", "relative_performance_plot_2_pests_diseases", "relative_performance_plot_2_yield", "relative_performance_plot_2_why", "relative_performance_plot_3_labour", "relative_performance_plot_3_weeds", "relative_performance_plot_3_pests_diseases", "relative_performance_plot_3_yield", "relative_performance_plot_3_why", "relative_performance_plot_4_labour", "relative_performance_plot_4_weeds", "relative_performance_plot_4_pests_diseases", "relative_performance_plot_4_yield", "relative_performance_plot_4_why", "relative_performance_plot_5_labour", "relative_performance_plot_5_weeds", "relative_performance_plot_5_pests_diseases", "relative_performance_plot_5_yield", "relative_performance_plot_5_why", "relative_performance_plot_6_labour", "relative_performance_plot_6_weeds", "relative_performance_plot_6_pests_diseases", "relative_performance_plot_6_yield", "relative_performance_plot_6_why", "relative_performance_plot_7_labour", "relative_performance_plot_7_weeds", "relative_performance_plot_7_pests_diseases", "relative_performance_plot_7_yield", "relative_performance_plot_7_why", "relative_performance_plot_8_labour", "relative_performance_plot_8_weeds", "relative_performance_plot_8_pests_diseases", "relative_performance_plot_8_yield", "relative_performance_plot_8_why", "width_of_harvested_plot_crop_1_plot_1", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_1.m", "number_of_rows_in_plot_crop_1_plot_1", "grain_weight_kg_shelled_grain_crop_1_plot_1.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_1.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_1.kg", "width_of_harvested_plot_crop_1_plot_2.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_2.m", "number_of_rows_in_plot_crop_1_plot_2", "grain_weight_shelled_grain_crop_1_plot_2.kg", "pod_weight_unshelled_grain_groundnut_crop_1_plot_2.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_2.kg", "width_of_harvested_plot_crop_1_plot_3.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_3.m", "number_of_rows_in_plot_crop_1_plot_3", "grain_weight_kg_shelled_grain_crop_1_plot_3.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_3.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_3.kg", "width_of_harvested_plot_crop_1_plot_4.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_4.m", "number_of_rows_in_plot_crop_1_plot_4", "grain_weight_kg_shelled_grain_crop_1_plot_4.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_4.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_4.kg", "width_of_harvested_plot_crop_1_plot_5.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_5.m", "number_of_rows_in_plot_crop_1_plot_5", "grain_weight_kg_shelled_grain_crop_1_plot_5.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_5.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_5.kg", "width_of_harvested_plot_crop_1_plot_6.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_6.m", "number_of_rows_in_plot_crop_1_plot_6", "grain_weight_kg_shelled_grain_crop_1_plot_6.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_6.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_6.kg", "width_of_harvested_plot_crop_1_plot_7.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_7.m", "number_of_rows_in_plot_crop_1_plot_7", "grain_weight_kg_shelled_grain_crop_1_plot_7.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_7.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_7.kg", "width_of_harvested_plot_crop_1_plot_8.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_8.m", "number_of_rows_in_plot_crop_1_plot_8", "grain_weight_kg_shelled_grain_crop_1_plot_8.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_8.kg", "above_ground_biomass_weight_husks_stover_res_crop_1_plot_8.kg", "width_of_harvested_plot_crop_2_plot_1.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_1.m", "number_of_rows_in_plot_crop_2_plot_1", "grain_weight_kg_shelled_grain_crop_2_plot_1.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_1.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_1.kg", "width_of_harvested_plot_crop_2_plot_2.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_2.m", "number_of_rows_in_plot_crop_2_plot_2", "grain_weight_kg_shelled_grain_crop_2_plot_2.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_2.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_2.kg", "width_of_harvested_plot_crop_2_plot_3.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_3.m", "number_of_rows_in_plot_crop_2_plot_3", "grain_weight_kg_shelled_grain_crop_2_plot_3.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_3.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_3.kg", "width_of_harvested_plot_crop_2_plot_4.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_4.m", "number_of_rows_in_plot_crop_2_plot_4", "grain_weight_kg_shelled_grain_crop_2_plot_4.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_4.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_4.kg", "width_of_harvested_plot_crop_2_plot_5.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_5.m", "number_of_rows_in_plot_crop_2_plot_5", "grain_weight_kg_shelled_grain_crop_2_plot_5.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_5.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_5.kg", "width_of_harvested_plot_crop_2_plot_6.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_6.m", "number_of_rows_in_plot_crop_2_plot_6", "grain_weight_kg_shelled_grain_crop_2_plot_6.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_6.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_6.kg", "width_of_harvested_plot_crop_2_plot_7.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_7.m", "number_of_rows_in_plot_crop_2_plot_7", "grain_weight_kg_shelled_grain_crop_2_plot_7.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_7.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_7.kg", "width_of_harvested_plot_crop_2_plot_8.m", "depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_8.m", "number_of_rows_in_plot_crop_2_plot_8", "grain_weight_kg_shelled_grain_crop_2_plot_8.kg", "pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_8.kg", "above_ground_biomass_weight_husks_stover_res_crop_2_plot_8.kg", "experimental_treatments_crop_1", "experimental_treatments_crop_2", "experimental_treatments_variety_crop_2", "experimental_treatments_fertilizer_1", "experimental_treatments_fertilizer_2", "experimental_treatments_fertilizer_3", "experimental_treatments_inoculant_type", "experimental_treatments_type_of_manure", "experimental_treatments_density_1_row_spacing.m", "experimental_treatments_density_1_plant_spacing.m", "experimental_treatments_density_1_seeds_per_hole", "experimental_treatments_density_1_plants_after_thinning", "experimental_treatments_density_2_row_spacing.m", "experimental_treatments_density_2_plant_spacing.m", "experimental_treatments_density_2_seeds_per_hole", "experimental_treatments_density_2_plants_after_thinning", "experimental_treatments_chemical_herbicide_pesticide", "experimental_treatments_staking_method_1", "experimental_treatments_staking_method_2", "fert_1_kg_plot_plot_1.kg_per_plot", "fert_2_kg_plot_plot_1.kg_per_plot", "fert_3_kg_plot_plot_1.kg_per_plot", "inoculant_y_n_plot_1", "manure_kg_plot_plot_1.kg_per_plot", "crop_1_y_n_plot_1", "crop_2_y_n_plot_1", "density_1_plot_1", "density_2_plot_1", "chemical_y_n_plot_1", "staking_1_plot_1", "staking_2_plot_1", "fert_1_kg_plot_plot_2.kg_per_plot", "fert_2_kg_plot_plot_2.kg_per_plot", "fert_3_kg_plot_plot_2.kg_per_plot", "inoculant_y_n_plot_2", "manure_kg_plot_plot_2.kg_per_plot", "crop_1_y_n_plot_2", "crop_2_y_n_plot_2", "density_1_plot_2", "density_2_plot_2", "chemical_y_n_plot_2", "staking_1_plot_2", "staking_2_plot_2", "fert_1_kg_plot_plot_3.kg_per_plot", "fert_2_kg_plot_plot_3.kg_per_plot", "fert_3_kg_plot_plot_3.kg_per_plot", "inoculant_y_n_plot_3", "manure_kg_plot_plot_3.kg_per_plot", "crop_1_y_n_plot_3", "crop_2_y_n_plot_3", "density_1_plot_3", "density_2_plot_3", "chemical_y_n_plot_3", "staking_1_plot_3", "staking_2_plot_3", "fert_1_kg_plot_plot_4.kg_per_plot", "fert_2_kg_plot_plot_4.kg_per_plot", "fert_3_kg_plot_plot_4.kg_per_plot", "inoculant_y_n_plot_4", "manure_kg_plot_plot_4.kg_per_plot", "crop_1_y_n_plot_4", "crop_2_y_n_plot_4", "density_1_plot_4", "density_2_plot_4", "chemical_y_n_plot_4", "staking_1_plot_4", "staking_2_plot_4", "fert_1_kg_plot_plot_5.kg_per_plot", "fert_2_kg_plot_plot_5.kg_per_plot", "fert_3_kg_plot_plot_5.kg_per_plot", "inoculant_y_n_plot_5", "manure_kg_plot_plot_5.kg_per_plot", "crop_1_y_n_plot_5", "crop_2_y_n_plot_5", "density_1_plot_5", "density_2_plot_5", "chemical_y_n_plot_5", "staking_1_plot_5", "staking_2_plot_5", "fert_1_kg_plot_plot_6.kg_per_plot", "fert_2_kg_plot_plot_6.kg_per_plot", "fert_3_kg_plot_plot_6.kg_per_plot", "inoculant_y_n_plot_6", "manure_kg_plot_plot_6.kg_per_plot", "crop_1_y_n_plot_6", "crop_2_y_n_plot_6", "density_1_plot_6", "density_2_plot_6", "chemical_y_n_plot_6", "staking_1_plot_6", "staking_2_plot_6", "fert_1_kg_plot_plot_7.kg_per_plot", "fert_2_kg_plot_plot_7.kg_per_plot", "fert_3_kg_plot_plot_7.kg_per_plot", "inoculant_y_n_plot_7", "manure_kg_plot_plot_7.kg_per_plot", "crop_1_y_n_plot_7", "crop_2_y_n_plot_7", "density_1_plot_7", "density_2_plot_7", "chemical_y_n_plot_7", "staking_1_plot_7", "staking_2_plot_7", "fert_1_kg_plot_plot_8.kg_per_plot", "fert_2_kg_plot_plot_8.kg_per_plot", "fert_3_kg_plot_plot_8.kg_per_plot", "inoculant_y_n_plot_8", "manure_kg_plot_plot_8.kg_per_plot", "crop_1_y_n_plot_8", "crop_2_y_n_plot_8", "density_1_plot_8", "density_2_plot_8", "chemical_y_n_plot_8", "staking_1_plot_8", "staking_2_plot_8", "instanceid"


	d2 <- data.frame(
		id = r2[["id"]],
		farm_id = r2[["farm_id"]],
		survey_date = as.character(as.Date(paste(r2[["date_hhsurvey_yyyy.years"]], r2[["date_hhsurvey_mm.months"]], r2[["date_hhsurvey_dd.days"]], sep="-"), format = "%Y-%B-%d")),
		country = r2[["country"]],
		village = r2[["village"]],
		adm2 = carobiner::fix_name(r2[["district"]], "title"),
		adm3 = carobiner::fix_name(r2[["sector_ward"]], "title"),
		location = r2[["village"]],
		latitude = as.numeric(r2[["gps_latitude_hh.decimal_degrees"]]),
		longitude = as.numeric(r2[["gps_longitude_hh.decimal_degrees"]]),
		elevation = as.numeric(r2[["gps_altitude_hh.m"]]),
		harvest_date = as.character(as.Date(paste(r2[["date_field_survey_yyyy"]], r2[["date_field_survey_mm"]], r2[["date_field_survey_dd"]], sep="-"), format = "%Y-%B-%d")),
		soil_sample_collected = r2[["soil_sample_collected"]],
		soil_sample_code = r2[["soil_sample_code"]],
		rain_data_collected = r2[["rain_data_collected"]],
		harvest_technician = r2[["organization_harvest_technician_1"]],
		stringsAsFactors = FALSE

	)

##r2: "SN", "id", "farm_id", "date_hhsurvey_dd.days", "date_hhsurvey_mm.months", "age_of_farmer.years", "farm_id.", "previous_participation", "previous_fieldbook", "previous_farm_id", "previous_role", "previous_role_other", "farmer_hh_head", "hh_head_gender", "hh_head_age.years", "no_females_0_16_yrs", "no_females_17_35_yrs", "no_females_36_60_yrs", "no_females_over_60_yrs", "no_males_0_16_yrs", "no_males_17_35_yrs", "no_males_36_60_yrs", "no_males_over_60_yrs", "education_hh_yrs_primary", "education_hh_yrs_secondary", "education_hh_yrs_post_secundary", "education_hh_yrs_university", "education_hh_yrs_other", "education_hh_head_yrs_primary", "education_hh_head_yrs_secondary", "education_hh_head_yrs_post_secundary", "education_hh_head_yrs_university", "education_hh_head_yrs_other", "education_hh_spec_other", "date_field_survey_dd", "date_field_survey_mm", "date_field_survey_yyyy", "organization_field_survey", "phone_field_survey", "field_number", "gps_latitude_field", "gps_longitude_field", "gps_altitude_field.m", "soil_sample_collected", "soil_sample_code", "rain_data_collected", "closest_rain_gauge", "organization_harvest_technician_1", "date_harvest_dd_technician_1", "date_harvest_mm_technician_1", "date_harvest_yyyy_technician_1", "organization_harvest_technician_2", "date_harvest_dd_technician_2", "date_harvest_mm_technician_2", "date_harvest_yyyy_technician_2", "organization_harvest_technician_3", "date_harvest_dd_technician_3", "date_harvest_mm_technician_3", "date_harvest_yyyy_technician_3", "instanceid"


	d3 <- data.frame(
		id = r3[["id"]],
		farm_id = r3[["farm_id"]],
		crop_income = r3[["proportion_income_from_crops$percentage"]],
		livestock_income = r3[["proportion_income_from_livestock$percentage"]],
		business_income = r3[["proportion_income_from_business$percentage"]],
		wealth_index = r3[["estimated_wealth"]],
	)
##r3: "SN", "id", "farm_id", "hired_labour", "home_consumption", "proportion_income_from_livestock.percentage", "proportion_income_from_lanour_agriculture.percentage", "proportion_income_from_lanour_off.percentage", "proportion_income_from_trade.percentage", "proportion_income_from_business.percentage", "proportion_income_from_salary.percentage", "proportion_income_from_pension.percentage", "proportion_income_from_remittances.percentage", "proportion_income_from_other.percentage", "proportion_income_from_spec_other", "assets_bicycle", "assets_motorbike", "assets_car", "assets_cellphone", "assets_radio", "assets_tv", "assets_fridge", "assets_sofa", "assets_tiledroof_walls", "assets_ironsheet_roof", "assets_tractor", "assets_plough", "assets_cart", "assets_tapwater", "assets_well", "assets_electricity", "assets_solar", "assets_generator", "assets_other1", "assets_other2", "estimated_wealth", "instanceid"


	d4 <- data.frame(
		id <- r4[["id"]],
		farm_id = r4[["farm_id"]],
		farm_size = r4[["farm_size"]], r4[["farm_size_unit"]],

	)
##r4: "SN", "id", "farm_id", "farm_size", "farm_size_unit", "no_cattle", "no_sheep", "no_goats", "no_pigs", "no_poutry", "no_livestock_other1", "no_livestock_other2", "spec_livestock_other1", "spec_livestock_other2", "main_crop_1", "yield_main_crop_1_unit", "area_main_crop_1", "area_main_crop_1_unit.ha", "main_crop_2", "yield_main_crop_2", "yield_main_crop_2_unit.kg_per_ha", "area_main_crop_2", "area_main_crop_2_unit.ha", "main_crop_3", "yield_main_crop_3", "yield_main_crop_3_unit.kg_per_ha", "area_main_crop_3", "area_main_crop_3_unit.ha", "instanceid"


	d5 <- data.frame(
		plot_id = r5[["Level.1..field.plot."]],
		crop = tolower(r5[["Level.2..person.crop."]])
	)
##r5: "variable_name", "variable_name_raw", "code", "category", "question..FARM.HH.", "sub_question..FARM.HH.", "Level.3..time.visit.repeated.measurement.", "category.1", "question.description", "data_field", "vartype", "units", "units_mysql", "comments"


# ## separate individual trials. For example trials in different locations or years. 
# ## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
# 	#d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
# ## about the data (TRUE/FALSE)
# 	d$on_farm <- 
# 	d$is_survey <- 
# 	d$irrigated <-
	
# ## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
#     d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
# ## each site must have corresponding longitude and latitude
# ## if the raw data do not provide them you can estimate them from the location/adm data 
# ## see carobiner::geocode
# 	d$longitude <- 
# 	d$latitude <- 
# # are the coordinates from the source (data/publication) or estimated by you?	
# 	d$geo_from_source <- TRUE/FALSE


# ## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
# ## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
# 	d$planting_date <- as.character(as.Date(   ))
# 	d$harvest_date  <- as.character(as.Date(    ))

# ### Fertilizers 
# ## note that we use P and K, not P2O5 and K2O
# ## P <- P2O5 / 2.29
# ## K <- K2O / 1.2051
#    d$P_fertilizer <- 
#    d$K_fertilizer <-
#    d$N_fertilizer <- 
#    d$S_fertilizer <- 
#    d$lime <- 
# ## normalize names 
#    d$fertlizer_type <- 

# ## for legumes   
#    #d$inoculated <- TRUE or FALSE
#    d$inoculant <- "name of inoculant"
   
# ### in general, add comments to your script if computations are
# ### based on information gleaned from metadata, a publication, 
# ### or when they are not immediately obvious for other reasons

# ### Yield

# 	yield <- r$yield_tonha * 1000
# 	#what plant part does yield refer to?
# 	d$yield_part <- "tubers"
# 	d$yield_moisture <- r$moisture * 100

# #NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# # Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# # if the data allow for that 

# 	d$fwy_storage <- r$yield_tonha * 1000
# 	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
# 	d$dmy_totat <- r$dry_biomass
	
# # all scripts must end like this
# 	carobiner::write_files(path, meta, d)
}

# ## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# # carob_script(path=_____)

