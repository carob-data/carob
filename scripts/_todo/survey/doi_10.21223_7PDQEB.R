# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Agronomic log of small potato farmers during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías y Tayabamba in the department of La Libertad - Peru.

This dataset corresponds to the records of small potato farmers carried out during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías, and Tayabamba, in the department of La Libertad - Peru. Corresponding to the different activities in their agricultural plots, such as field preparation, planting, agronomic management, pest and disease management, and harvest. This dataset has been developed considering the e-Agrology platform and using the modules: Farmers-crops-site (n=759), sowing (n=684), visit (n=794), harvest (n=592), productivity (n=592), and soil analysis (n=204).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/7PDQEB"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Pataz Association - AP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Native Potato producing households; universe: Native Potato producing households from districts of Chugay, Pataz, Pías, and Tayabamba, in the department of La Libertad - Peru; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We randomly selected ten communities of each district, (Chugay, Pataz, Pías, and Tayabamba) of La Libertad
Farmers-crops-site (n=759), sowing (n=684), visit (n=794), harvest (n=592), productivity (n=592), and soil analysis (n=204).",
		
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

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_Data-Agronomic-La Libertad-2021-2022-Farmers-Crop-Site.xlsx"]
	f2 <- ff[basename(ff) == "02_Data-Agronomic-La Libertad-2021-2022-Sowing.xlsx"]
	f3 <- ff[basename(ff) == "03_Data-Agronomic-La Libertad-2021-2022-Visit.xlsx"]
	f4 <- ff[basename(ff) == "04.1_Data-Agronomic-La Libertad-2021-2022-Crop.xlsx"]
	f5 <- ff[basename(ff) == "04.2_Data-Agronomic-La Libertad-2021-2022-Manual activity.xlsx"]
	f6 <- ff[basename(ff) == "04_Data-Agronomic-La Libertad-2021-2022-Harvest.xlsx"]
	f7 <- ff[basename(ff) == "05.1_Data-Agronomic-La Libertad-2021-2022-Products.xlsx"]
	f8 <- ff[basename(ff) == "05_Data-Agronomic-La Libertad-2021-2022-Productivity.xlsx"]
	f9 <- ff[basename(ff) == "06_Data-Agronomic-La Libertad-2021-2022-Soil analysis.xlsx"]
	f10 <- ff[basename(ff) == "07_Data_dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)
	r10 <- carobiner::read.excel(f10)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		location = r1[["Site"]],
		adm2 = carobiner::fix_name(r1[["province"]], "title"),
		plot_id = r1[["plot_area"]],
		latitude = r1[["latitude"]],
		longitude = r1[["longitude"]],
		elevation = r1[["elevation"]],
		year = r1[["year"]],
		crop = tolower(r1[["crop1"]]),
		variety = r1[["variety_name"]]
	)
##r1: "ID", "level_education", "district", "locality", "experience", "organization", "cycle", "log_type", "water_regime", "project", "surface", "production_type", "crop2", "seed_type", "seed_cost", "seed_adquired", "type_cost_seed", "main_product", "cost_transport_seed", "point_origin", "seed_category", "year_seed", "where_by_seed", "because_uses_seed_market", "type_property", "possesion_land"


	d2 <- data.frame(
		location = r2[["Site"]],
		crop = tolower(r2[["crop_before_planting"]]),
		planting_date = r2[["planting_date"]]
	)
##r2: "ID", "activity", "planting_area", "how_register_cost", "determine_planting", "how_planting", "planting_in", "arrangement", "slope_was_considered", "part_furrow", "planting_density", "plants_per_point", "distance_plants", "distance_furrow", "planting_method", "number_implement_used", "used_manual_implement", "manual_number_people", "manual_jornal_cost_day", "manual_jornal_used", "manual_duration_jornal_day", "animal-type", "animal-cost", "hours_animal", "uses_animal_implement", "animal-implement_was"


	d3 <- data.frame(
		location = r3[["Site"]],
		crop = tolower(r3[["crop_name"]]),
		plot_id = r3[["general_condition_plot"]]
	)
##r3: "ID", "visit_date", "crop_stage", "current_problem_crop", "establishment_problems", "percentage_establishment_problems", "caused_by", "part_affect_plagues", "severity", "ipm_type", "ipm_reasons", "incidence", "number_pest", "name_pest", "reasons_problem", "weeds_type", "how_control_weeds", "percentage_damage_weeds", "problem_nutritional_deficiencies", "problema_deficiencias-mas_probable", "type_fertilization", "percentage_damage_nutritional", "problem_water_estres", "percentage_damage_water", "weather_conditions", "percentage_damage_weather", "technologies_implement", "final_remarks", "recommendations", "uses_smartphone", "phone_operator", "quality_phone_signal"


	d4 <- data.frame(
		crop = tolower(r4[["crop_name1"]]),
		adm3 = carobiner::fix_name(r4[["communal_work"]], "title"),
		harvest_date = r4[["harvest_start_date"]]
	)
##r4: "obtained_product_harvest", "harvest_end_date", "how_determine_harvest", "parent_key1"


	d5 <- data.frame(
		crop = tolower(r5[["crop_name2"]])
	)
##r5: "perform_manual_harvest", "date_manual_activity", "manual_activity_performed", "uses_implements_manual", "number_implements_used_manual", "cosecha_manual-actividad1-mas_de_un_implemento-implemento_utilizado", "number_people_harvest", "labor_cost_day", "jornal_used", "duration_jornal_day", "cost_type", "cosecha_manual-actividad1-descripcion1", "parent_key2"


	d6 <- data.frame(
		location = r6[["Site"]],
		plot_id = r6[["plot_name"]],
		crop = tolower(r6[["crop1"]])
	)
##r6: "ID", "meta-instance", "surfice", "harvest_method", "percentage_stubble_plot", "description_harvest", "number_manual_harvest", "manual1"


	d7 <- data.frame(
		yield = r7[["estimated_yield"]]
	)
##r7: "product_obtained", "real_yield", "percentage_for_sale", "percentage_to_self-consumption", "percentage_to_seed", "sold_product", "sale_price", "parent_key3"


	d8 <- data.frame(
		location = r8[["Site"]],
		plot_id = r8[["plot_name"]],
		crop = tolower(r8[["crop_name"]]),
		year = r8[["previous_year_yield"]],
		yield = r8[["previous_year_yield"]]
	)
##r8: "ID", "meta-instance", "general_information", "factor_difference", "indicate_environmental_factors", "indicate_managment_factors", "how_product_harvest", "product1"


	d9 <- data.frame(
		N_fertilizer = r9[["N"]],
		location = r9[["Site"]],
		P_fertilizer = r9[["P"]],
		K_fertilizer = r9[["K"]]
	)
##r9: "pH", "EC", "CaCo3", "OM", "Sand", "Silt", "Clay", "TC", "CEC", "Ca+2", "Mg+2", "K+", "Na+", "Al+3H+", "Scat", "Sbas", "Bsat", "OC", "BD"


	d10 <- data.frame()
##r10: "Sheet", "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


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

