# R script for "carob"
# license: GPL (>=3)

## NOTES
# yield was derived from unshelled groundnut weight in kg/plot and converted to kg/ha. in the raw data plots 5-8 are mostly empty, so only treatments 1-4 will be included. 

#nutrition.csv and production.csv doesn't seem to contain data pertaining to the actual trial. land_crops_and_livestock.csv contains general farm data.


carob_script <- function(path) {

"
N2Africa diagnostic trial - Eastern Uganda, 2014, Season II

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/tnn1-sp31"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;P_fertilizer;S_fertilizer",
		response_vars = "yield;fwy_total", 
		notes = NA,
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-13",
		carob_completion = 40,	
		carob_effort = 18
	)
	
	f1 <- ff[basename(ff) == "experiment.csv"]
	f2 <- ff[basename(ff) == "general.csv"]
	# f3 <- ff[basename(ff)  == "labour_income_and_assets.csv"]
	# f4 <- ff[basename(ff) == "land_crops_and_livestock.csv"]
	# f5 <- ff[basename(ff) == "variable_definitions.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	# r3 <- read.csv(f3)
	# r4 <- read.csv(f4)
	# r5 <- read.csv(f5)

	# constructing a function to extract data for each experimental plot
	make_plot <- function(i){ 
		width_col <- grep(paste0("width_of_harvested_plot_crop_1_plot_", i), names(r1), value = TRUE) 	
		pod_col <- grep(paste0("pod_weight.*crop_1.*plot_", i, "\\.kg$"), names(r1), value = TRUE)
		d <- data.frame(
			hhid = as.character(r1[["id"]]),
			trial_id = r1[["farm_id"]], # could be field_id (not sure as it identifies specific fields on farm and not the whole farm)
			treatment = r1[[paste0("description_treatment_", i)]], 
			plot_id = as.character(i),
			variety = tolower(r1[["experimental_treatments_variety_crop_1"]]),
			plot_width = as.numeric(r1[[width_col[1]]]),
			plot_length = as.numeric(r1[[paste0("depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_", i, ".m")]]),
			TSP = as.numeric(r1[[paste0("fert_1_kg_plot_plot_", i, ".kg_per_plot")]]), # measured in kg
			gypsum = as.numeric(r1[[paste0("fert_2_kg_plot_plot_", i, ".kg_per_plot")]]), # measured in kg
			inoculated = tolower(r1[[paste0("inoculant_y_n_plot_", i)]]) == "y",
			relative_performance = r1[[paste0("relative_performance_plot_", i, "_yield")]]
			#germination_rate = as.numeric(r1[[paste0("germination_crop_1_treatment_", i)]])
		)
		d$plot_area = d$plot_width * d$plot_length
		num_rows <- as.numeric(r1[[paste0("number_of_rows_in_plot_crop_1_plot_", i)]])
		d$row_spacing = num_rows / d$plot_width

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
		pod_weight = as.numeric(r1[[pod_col[1]]])
		biomass_kg = as.numeric(r1[[paste0("above_ground_biomass_weight_husks_stover_res_crop_1_plot_", i, ".kg")]])
		d$fwy_storage <- d$yield <- (pod_weight/d$plot_area) * 10000 # converted to kg/ha
		d$fwy_total <- (biomass_kg/d$plot_area) * 10000
		d[!is.na(d$yield), ]
	}
	# creating plot records for treatments 1-4 and stacking them
	d1 <- do.call(rbind, lapply(1:8, make_plot))
	
	# normalising treatment
	d1$treatment <- gsub("ground nut variety | applied", "", trimws(d1$treatment))
	d1$treatment <- gsub(" with | plus ", ", ", d1$treatment)
	d1$treatment <- gsub("fertilizers", "fertilizer", d1$treatment)
	d1$treatment <- gsub(", fertilizer", ",", d1$treatment)
	d1$treatment[d1$treatment == ""] <- NA

	d2 <- data.frame(
		hhid = as.character(r2[["id"]]),
		# could be field_id (not sure as it identifies specific fields on farm and not the whole farm)
		trial_id = r2[["farm_id"]], 
		# survey_date = as.character(as.Date(paste(r2[["date_field_survey_yyyy"]], r2[["date_field_survey_mm"]], r2[["date_field_survey_dd"]], sep="-"), format = "%Y-%B-%d")),
		# date of hh survey, but hh survey not yet processed
		# date = as.integer(r2[["date_hhsurvey_yyyy.years"]]),
		country = "Uganda", #r2[["country"]],
		adm2 = carobiner::fix_name(r2[["district"]], "title"),
		adm3 = carobiner::fix_name(r2[["sector_ward"]], "title"),
		location = tolower(r2[["village"]]),
		latitude = as.numeric(r2[["gps_latitude_hh.decimal_degrees"]]),
		longitude = as.numeric(r2[["gps_longitude_hh.decimal_degrees"]]),
		geo_from_source = TRUE,		
		elevation = as.numeric(r2[["gps_altitude_hh.m"]]),
		harvest_date = as.character(as.Date(paste(r2[["date_harvest_yyyy_technician_1"]], r2[["date_harvest_mm_technician_1"]], r2[["date_harvest_dd_technician_1"]], sep="-"), format = "%Y-%B-%d"))
	)
	#d2$date[is.na(d2$date)] <- 2014 # filling in missing data in "year"

	# assigned longitude/latitude for missing location (id9) in general.csv 
	missing_geo <- (is.na(d2$latitude) | is.na(d2$longitude))
	d2$geo_from_source <- !missing_geo
	d2$geo_uncertainty <- NA
	d2$geo_source <- NA
	# d2$adm3[missing_geo] # [1] "Kagumu"
	d2$latitude[missing_geo] <- 1.1187 
	d2$longitude[missing_geo] <- 33.8428
	d2$geo_uncertainty[missing_geo] <- 5906 # meters
	d2$geo_source[missing_geo] <- "GADM 4.1, adm3"

	d <- merge(d1, d2, by = c("hhid", "trial_id"), all.x = TRUE) 
	d$trial_id <- as.character(as.numeric(as.factor(d$trial_id)))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
## TSP: 46% P2O5
## GYPSUM: 18.5% S
	P2O5 <- (d1$TSP/d1$plot_area) * 0.46 * 10000 # converting mass of tsp applied to p2o5 mass kg/ha
   	d$P_fertilizer <- P2O5 / 2.29 # converting to elemental p mass kg/ha
   	d$K_fertilizer <- 0
   	d$N_fertilizer <- 0
   	d$S_fertilizer <- (d1$gypsum/d1$plot_area) * 0.185 * 10000
   	d$lime <- 0

	d$P_fertilizer[is.na(d$P_fertilizer)] <- 0 # if data val is na, fill as 0
	d$S_fertilizer[is.na(d$S_fertilizer)] <- 0

## normalize names 
   	d$fertilizer_type <- ifelse((d$P_fertilizer > 0) & (d$S_fertilizer > 0), "TSP;gypsum",
			ifelse(d$P_fertilizer > 0, "TSP", 
			ifelse(d$S_fertilizer > 0, "gypsum", "none")))

	d$TSP <- d$gypsum <- NULL

## for legumes   
   	d$inoculant <- NA

## Yield
	d$yield_part <- "pod" # recorded as unshelled pods, no values for shelled grain given
	d$yield_moisture <- NA # not recorded
	d$yield_isfresh <- NA # not stated

## about data
	d$on_farm <- TRUE
	d$is_survey <- FALSE	
	d$irrigated <- NA
## crop
	d$crop <- "groundnut"
	d$planting_date <- NA # not recorded
	
 	carobiner::write_files(path, meta, d)
}

