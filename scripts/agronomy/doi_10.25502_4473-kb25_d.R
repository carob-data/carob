# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Out of range errors
           #Tudun Wada plot 216's LAI_At_V8 is 130
           #Tudun Wada plots 119/121 have low plant height (4.8, 9.8cm)

#frac_int_radiation gives "unknown variables" but its on terminag

# suggested terms: planting_window


carob_script <- function(path) {

"
EiA SAA Usecase Nigeria, maize sowing window validation experiment dataset, 2023.

The dataset was obtained from 3 locations (BUK, TOFA, and Tudunwada) in Kano state, Nigeria. 
The dataset consists of a split plot experiment with sowing window as the main plot and varieties as the sub plot.
6 varieties were used for the experiment representing early, medium and late maturity groups. 
Six sowing windows with a week interval was used in the experiment.
"

	uri <- "doi:10.25502/4473-kb25/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
	                                data_organization = "IITA",
	                                publication = NA,
	                                project = "EiA; SAA",
	                                design = "Split-plot experiment",
	                                data_type = "on-station experiment",
	                                treatment_vars = "planting_window; variety",
	                                response_vars = "yield; dmy_total; dmy_leaves; dmy_stems; harvest_index; tassling_days; silking_days; maturity_days",
	                                notes = NA,
	                                carob_contributor = "Stella Muthoni",
	                                carob_date = "2026-09-21",
	                                carob_completion = 80,
	                                carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "buk.csv"]              # Bayero University Kano
	f2 <- ff[basename(ff) == "tofa.csv"]              # Tofa LGA
	f3 <- ff[basename(ff) == "tudun-wada.csv"]        # Tudun Wada LGA
	f4 <- ff[basename(ff) == "data_dictionary4.csv"]  # column definitions
	
	r1 <- read.csv(f1, na.strings = ".")
	r2 <- read.csv(f2, na.strings = ".")
	r3 <- read.csv(f3, na.strings = ".")
	
	## BUK
	d1 <- data.frame(
	  plot_id = as.character(r1[["Plot_no"]]),
	  country = "Nigeria",
	  adm1 = "Kano",
	  location = "Bayero University, Kano",
	  latitude = NA,
	  longitude = NA,
	  geo_uncertainty = NA,
	  geo_source = NA,
	  geo_from_source = FALSE,
	  planting_window = r1[["Planting_window"]],
	  variety = r1[["Varieties"]],
	  planting_date = as.character(as.Date(r1[["Date_of_planting"]], format="%d/%m/%Y")),
	  plant_height = as.numeric(r1[["Plant_Height_cm_At_V8"]]),
	  LAI = as.numeric(r1[["LAI_At_V8"]]),
	  frac_int_radiation = (as.numeric(r1[["PAR_A_At_V8"]]) - as.numeric(r1[["PAR_B_At_V8"]])) / as.numeric(r1[["PAR_A_At_V8"]]),
	  anthesis_days = NA,
	  tassling_days = as.numeric(r1[["Date_to_50_percent_Tasseling"]]),
	  silking_days = as.numeric(r1[["Date_of_50_percent_Silking"]]),
	  maturity_days = as.numeric(r1[["Days_to_95_percent_physiological_maturity"]]),
	  dmy_leaves = as.numeric(r1[["leaves_weight_g_per_m2"]]) * 10,
	  dmy_stems = as.numeric(r1[["stem_weight_g_per_m2"]]) * 10,
	  dmy_total = as.numeric(r1[["Total_dry_matter_kg_per_ha"]]),
	  harvest_index = as.numeric(r1[["HI_ha"]]),
	  yield = as.numeric(r1[["Grain_yield_kg_per_ha"]]),
	  yield_moisture = as.numeric(r1[["Grains_moisture_percent"]])
	)
	
	## TOFA
	d2 <- data.frame(
	  plot_id = as.character(r2[["Plot_no"]]),
	  country = "Nigeria",
	  adm1 = "Kano",
	  location = "Tofa",
	  latitude = 11.9957,
	  longitude = 8.3107,
	  geo_uncertainty = 14992,
	  geo_source = "GADM 4.1, adm2",
	  geo_from_source = FALSE,
	  planting_window = r2[["Planting_window"]],
	  variety = r2[["Varieties"]],
	  planting_date = as.character(as.Date(r2[["Date_of_planting"]], format="%d/%m/%Y")),
	  plant_height = as.numeric(r2[["Plant_Height_cm_At_V8"]]),
	  LAI = as.numeric(r2[["LAI_At_V8"]]),
	  frac_int_radiation = (as.numeric(r2[["PAR_A_At_V8"]]) - as.numeric(r2[["PAR_B_At_V8"]])) / as.numeric(r2[["PAR_A_At_V8"]]),
	  anthesis_days = NA,
	  tassling_days = as.numeric(r2[["Date_of_50_percent_Tasseling"]]),
	  silking_days = as.numeric(r2[["Date_of_50_percent_Silking"]]),
	  maturity_days = as.numeric(r2[["Days_to_95_percent_physiological_maturity"]]),
	  dmy_leaves = as.numeric(r2[["leaves_weight_g_per_m2"]]) * 10,
	  dmy_stems = as.numeric(r2[["stem_weight_g_per_m2"]]) * 10,
	  dmy_total = as.numeric(r2[["Total_dry_matter_g_per_ha"]]),
	  harvest_index = as.numeric(r2[["HI"]]),
	  yield = as.numeric(r2[["Grain_yield_kg_per_ha"]]),
	  yield_moisture = as.numeric(r2[["Grains_moisture_percent"]])
	)
	
	## Tudun Wada
	d3 <- data.frame(
	  plot_id = as.character(r3[["Plot_no"]]),
	  country = "Nigeria",
	  adm1 = "Kano",
	  location = "Tudun Wada",
	  latitude = 11.2511,
	  longitude = 8.5570,
	  geo_uncertainty = 39135,
	  geo_source = "GADM 4.1, adm2",
	  geo_from_source = FALSE,
	  planting_window = r3[["Planting_window"]],
	  variety = r3[["Varieties"]],
	  planting_date = as.character(as.Date(r3[["Date_of_planting"]], format="%d/%m/%Y")),
	  plant_height = as.numeric(r3[["Plant_Height_cm_At_V8"]]),
	  LAI = as.numeric(r3[["LAI_At_V8"]]),
	  frac_int_radiation = (as.numeric(r3[["PAR_A_At_V8"]]) - as.numeric(r3[["PAR_B_At_V8"]])) / as.numeric(r3[["PAR_A_At_V8"]]),
	  anthesis_days = as.numeric(r3[["Days_to_50_percent_anthesis"]]),
	  tassling_days = NA,
	  silking_days = as.numeric(r3[["Days_to_50_percent_Silking"]]),
	  maturity_days = as.numeric(r3[["Days_to_95_percent_physiological_maturity"]]),
	  dmy_leaves = as.numeric(r3[["leaves_weight_g_per_m2"]]) * 10,
	  dmy_stems = as.numeric(r3[["stem_weight_g_per_m2"]]) * 10,
	  dmy_total = as.numeric(r3[["Total_dry_matter_g_per_ha"]]),
	  harvest_index = as.numeric(r3[["HI_ha"]]),
	  yield = as.numeric(r3[["Grain_yield_kg_per_ha"]]),
	  yield_moisture = as.numeric(r3[["Grains_moisture_percent"]])
	)
	
	d <- rbind(d1, d2, d3)
	
	d$crop <- "maize"
	d$is_survey <- FALSE
	d$on_farm <- FALSE
	d$irrigated <- NA
	d$K_fertilizer <- NA
	d$N_fertilizer <- NA
	d$P_fertilizer <- NA
	d$harvest_date <- NA
	d$yield_part <- "grain"
	d$trial_id <- as.character(as.integer(as.factor(d$location)))
	
	carobiner::write_files(path, meta, d)
}



