# R script for "carob"
# license: GPL (>=3)

## ISSUES
# REJECTED - no quantitative response variable available. This is a package-monitoring survey with only
# management/practice fields, GPS, and qualitative ratings (germination 1-4 scale undefined, farmer
# fertility perception); no yield, biomass, or production data for the trial plots

carob_script <- function(path) {

"
N2Africa focal adapt trial, 2015, I

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.25502/6rjh-ae22"
	group <- "draft"
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
		design = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Oscar Bautista",
		carob_date = "2026-07-10",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "data_table.csv"]
	f2 <- ff[basename(ff) == "variable_definitions.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		country = r1[["country"]],
		adm2 = carobiner::fix_name(r1[["lga_district_woreda"]], "title"),
		adm3 = carobiner::fix_name(r1[["sector_ward"]], "title"),
		hhid = r1[["gender_of_farmer"]],
		variety = r1[["pack_variety"]],
		plot_id = r1[["control_plot_next_to_the_n2africa_plot"]],
		crop = tolower(r1[["intercropping_n2a_plot"]]),
		latitude = r1[["gps_field_device_latitude_decimal_degrees"]],
		longitude = r1[["gps_field_device_longitude_decimal_degrees"]],
		elevation = r1[["gps_field_device_altitude_m"]],
		treatment = r1[["germination_crop_1_treatment_1"]],
		season = r1[["crop_1_previous_season"]]
	)
##r1: "SN", "id", "submissiondate", "start", "end", "deviceid", "date_hhsurvey_1_date", "previous_participation", "previous_fieldbook", "farmer_visit_demo_trial", "farm_id", "pack_species", "pack_mineral_fertilizer_type", "pack_inoculant", "other_input_practice_1", "other_input_practice_2", "new_legume", "new_variety", "new_this_mineral_fertilizer_in_this_legume", "new_inoculant", "new_other_input_practice_1", "new_other_input_practice_2", "reason_for_choosing_package", "planted_n2africa_legume", "planted_own_legume", "all_seed_used", "all_inputs_used", "own_legume_in_same_field_as_n2africa_legume", "two_legume_plots_distinguishable", "legume_species_farmer", "legume_variety_farmer", "if_not_what_did_you_do", "row_planting_n2a_plot", "staking_material_n2a_plot", "other_practices_n2a_plot", "reason_intercropping_n2a_plot", "reason_row_planting_n2a_plot", "intercropping_own_legume_plot", "row_planting_own_legume_plot", "staking_material_own_legume", "other_practices_own_legume", "reason_intercropping_own_legume", "reason_row_planting_own_legume", "i_13_15", "i_13_16", "inoculation_n2africa_field", "farmer_training_on_inoculation", "assistance_with_inoculation", "inoculant_stored", "place_stored", "time_between_opening_and_mixing", "time_between_mixing_and_planting", "gps_field_device_accuracy_m", "gps_latitude_field_decimal_degrees", "gps_longitude_field_decimal_degrees", "gps_altitude_field_m", "ownership_field", "slope_of_the_field", "farmer_perception_fertility_n2africa_field", "relative_fertility_n2africa_field", "drainage_field", "soil_depth_point_1_m", "soil_depth_point_2_m", "soil_depth_point_3_m", "row_spacing_crop_1_plot_1_cm", "plant_spacing_crop_1_plot_1_cm", "no_plants_hole_crop_1_plot_1_nr", "width_of_harvested_plot_crop_1_plot_1_m", "depth_of_harvested_plot_crop_1_plot_1_m", "number_of_rows_crop_1_plot_1_nr", "row_spacing_crop_1_plot_2_cm", "plant_spacing_crop_1_plot_2_cm", "no_plants_hole_crop_1_plot_2_nr", "width_of_harvested_plot_crop_1_plot_2_m", "depth_of_harvested_plot_crop_1_plot_2_m", "plot_area_crop_1_plot_2", "unit_plot_area_crop_1_plot_2", "unit_plot_area_crop_1_plot_2_1", "row_spacing_crop_1_plot_1_cm_1", "plant_spacing_crop_1_plot_1_cm_1", "no_plants_hole_crop_1_plot_1_nr_1", "width_of_harvested_plot_crop_1_plot_1_m_1", "depth_of_harvested_plot_crop_1_plot_1_m_1", "number_of_rows_crop_1_plot_1_nr_1", "reason_plant_spacing_and_no_of_plants_per_hole_crop_1_plot_1", "reason_plant_spacing_and_no_of_plants_per_hole_crop_1_plot_2", "germination_crop_1_treatment_2", "germination_crop_1_treatment_3", "other_crops_previous_season", "type_of_mineral_fertilizer_previous_season", "type_of_organic_fertilizer_previous_season", "inoculants_previous_season", "crop_1_season_before_previous_season", "other_crops_season_before_previous_season", "type_of_mineral_fertilizers_season_before_previous_season", "type_of_organic_fertilizers_season_before_previous_season", "inoculants_before_previous_season", "image_n2a_field", "image_ref_field", "image_cont_field", "instanceid", "key_odk", "staking_method_n2a_plot", "reason_staking_method_n2a_plot", "reason_staking_material_n2a_plot", "reason_other_practices_n2a_plot", "staking_method_own_legume", "reason_staking_method_own_legume", "reason_staking_material_own_legume", "reason_other_practices_own_legume", "intercropping_with_which_crops_n2a_plot", "intercropping_with_which_crops_own_legume_plot", "other_lga_district_woreda", "new_legume_1", "legume_variety_farmer_1", "if_not_what_did_you_do_1", "row_spacing_crop_1_plot_3_cm", "plant_spacing_crop_1_plot_3_cm", "no_plants_hole_crop_1_plot_3_nr", "width_of_harvested_plot_crop_1_plot_3_m", "depth_of_harvested_plot_crop_1_plot_3_m", "number_of_rows_crop_1_plot__nr", "area_harvested_most_important_crop_previous_season", "unit_of_area_harvest_previous_season", "amount_harvested_most_important_crop_previous_season", "unit_amount_harvested_most_important_crop_previous_season", "inoculants_previous_season_1", "area_harvested_most_important_crop_before_previous_season", "unit_of_area_harvest_season_before_previous_season", "amount_harvested_most_important_crop_before_previous_season", "unit_amount_harvested_most_important_crop_before_previous_season"


	d2 <- data.frame(
		plot_id = r2[["Level_1_field_plot"]],
		crop = tolower(r2[["Level_2_person_crop"]])
	)
##r2: "variable_name", "variable_name_raw", "code", "category", "question_FARM_HH", "sub_question_FARM_HH", "Level_3_time_visit_repeated_measurement", "category_1", "question_description", "data_field", "vartype", "units", "units_mysql", "comments"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

