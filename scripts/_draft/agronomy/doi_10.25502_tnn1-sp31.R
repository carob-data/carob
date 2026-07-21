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

	uri <- "doi:10.25502/tnn1-sp31"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	jmeta <- paste0(yuri::simpleURI(uri), ".json")
	json_paths <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_paths <- json_paths[!tolower(basename(json_paths)) %in% tolower(c(jmeta, "metadata.json"))]
	json_list <- if (length(json_paths) > 0) {
		stats::setNames(lapply(json_paths, jsonlite::fromJSON), basename(json_paths))
	} else {
		list()
	}

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = "on-farm diagnostic trial, to evaluate groundnut variety and P/S fertiliser response", # SERENUT 5 & 6 and the presence of TSP/GYPSUM fertiliser 
		data_type = "on-farm experiment",
		treatment_vars = "variety;P_fertilizer;S_fertilizer",
		response_vars = "yield;dmy_total", 
		notes ="yield was derived from unshelled groundnut weight in kg/plot and converted to kg/ha. in the raw data plots 5-8 are mostly empty, so only treatments 1-4 will be included. nutrition.csv and production.csv doesn't seem to contain data pertaining to the actual trial. land_crops_and_livestock.csv contains general farm data.",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-13",
		carob_completion = 75,	
		carob_effort = 18
	)
	

	f1 <- ff[basename(ff) == "experiment.csv"]
	f2 <- ff[basename(ff) == "general.csv"]
	# f3 <- ff[basename(ff) == "labour_income_and_assets.csv"]
	# f4 <- ff[basename(ff) == "land_crops_and_livestock.csv"]
	# f5 <- ff[basename(ff) == "variable_definitions.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	# r3 <- read.csv(f3)
	# r4 <- read.csv(f4)
	# r5 <- read.csv(f5)

	make_plot <- function(r1, i){ # constructing a function to extract data for each experimental plot
		name <- r1[[paste0("name_treatment_", i)]]
		desc <- r1[[paste0("description_treatment_", i)]]

		width_col <- grep(paste0("width_of_harvested_plot_crop_1_plot_", i), names(r1), value = TRUE) 
		width <- as.numeric(r1[[width_col[1]]])
		
		pod_col <- grep(paste0("pod_weight.*crop_1.*plot_", i, "\\.kg$"), names(r1), value = TRUE)
		podweight <- as.numeric(r1[[pod_col[1]]])

		depth <- as.numeric(r1[[paste0("depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_", i, ".m")]])
		num_rows <- as.numeric(r1[[paste0("number_of_rows_in_plot_crop_1_plot_", i)]])

		agbio <- as.numeric(r1[[paste0("above_ground_biomass_weight_husks_stover_res_crop_1_plot_", i, ".kg")]])
		fert1 <- as.numeric(r1[[paste0("fert_1_kg_plot_plot_", i, ".kg_per_plot")]]) # for tsp
		fert2 <- as.numeric(r1[[paste0("fert_2_kg_plot_plot_", i, ".kg_per_plot")]]) # for gypsum
		inoc <- r1[[paste0("inoculant_y_n_plot_", i)]]
		performance <- r1[[paste0("relative_performance_plot_", i, "_yield")]]	
		# germ_crop <- r1[[paste0("germination_crop_1_treatment_", i)]]

		data.frame(
			hhid = as.character(r1[["id"]]),
			farm_id = r1[["farm_id"]], # could be field_id (not sure as it identifies specific fields on farm and not the whole farm)
			treatment_name = name, # treatment_name used instead of "treatment" because the variable is used to describe the treatment not name it
			treatment = desc, 
			plot_id = as.character(i),
			variety = r1[["experimental_treatments_variety_crop_1"]],
			plot_width = width,
			plot_depth_m = depth, # was recorded in meters/ may also be soil depth [not 100% sure]
			plot_area = width * depth,
			row_density = num_rows, # could not find equivalent in terminag
			nodule_weight = podweight,
			biomass_kg = agbio,
			TSP = fert1, # measured in kg/ could not find equivalent variable in terminag
			gypsum = fert2, # measured in kg
			inoculated = tolower(inoc) == "y",
			reletive_yield_performance = performance, # could not find equivalent variable in terminag
			# germination_rate = as.numeric((germ_crop)),
			stringsAsFactors = FALSE
		)
	}
	

	# creating plot records for treatments 1-4 and stacking them
	plots <- vector("list", 4)
	for (i in 1:4) {
		plots[[i]] <- make_plot(r1, i)
	}
	d1 <-do.call(rbind, plots)
	# normalising treatment
	d1$treatment <- trimws(d1$treatment)
	d1$treatment[d1$treatment == ""] <- NA

	d2 <- data.frame(
		hhid = as.character(r2[["id"]]),
		farm_id = r2[["farm_id"]], # could be field_id (not sure as it identifies specific fields on farm and not the whole farm)
		# survey_date = as.character(as.Date(paste(r2[["date_field_survey_yyyy"]], r2[["date_field_survey_mm"]], r2[["date_field_survey_dd"]], sep="-"), format = "%Y-%B-%d")),
		year = as.integer(r2[["date_hhsurvey_yyyy.years"]]), # trial year / could not find equivalent in terminag
		country = r2[["country"]],
		adm2 = carobiner::fix_name(r2[["district"]], "title"),
		adm3 = carobiner::fix_name(r2[["sector_ward"]], "title"),
		location = tolower(r2[["village"]]),
		latitude = as.numeric(r2[["gps_latitude_hh.decimal_degrees"]]),
		longitude = as.numeric(r2[["gps_longitude_hh.decimal_degrees"]]),
		elevation = as.numeric(r2[["gps_altitude_hh.m"]]),
		harvest_date = as.character(as.Date(paste(r2[["date_harvest_yyyy_technician_1"]], r2[["date_harvest_mm_technician_1"]], r2[["date_harvest_dd_technician_1"]], sep="-"), format = "%Y-%B-%d")),
		stringsAsFactors = FALSE
	)
	d2$year[is.na(d2$year)] <- 2014 # filling in missing data in "year"

	# normalising country names
	d2$country <- trimws(d2$country)
	d2$country[d2$country == ""] <- "Uganda"
	d2$country[toupper(d2$country) == "UGANDA"] <- "Uganda"


d <- merge(d1, d2, by = c("hhid", "farm_id"), all.x = TRUE)
## separate individual trials. For example trials in different locations or years. 
## farm_id uniquely id's a farm
	d$trial_id <- as.character(as.integer(as.factor( d$farm_id )))

## about data
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE # no irrigation mentioned. rain data is collected in general.csv

## crop
	d$crop <- "groundnut"
    d$crop_rotation <-  NA # 1. rotation briefly mentioned in experiment.csv, but isn't consistent enough to mention. 2. other crops are mentioned in "production.csv" but they don't seem to relate to the trial

	d$geo_from_source <- TRUE # taken from coordinates in general.csv

	d$planting_date <- NA # not recorded
	d$harvest_date  <- d$harvest_date

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
   	d$fertilizer_type <- ifelse(d$P_fertilizer > 0 | d$S_fertilizer > 0, "TSP ; gypsum", NA)

## for legumes   
   	d$inoculated <- d$inoculated # FALSE, all inoculant use in plots is recorded as "n" in experiment.csv under inoculant_y_n_plot_N
   	d$inoculant <- NA

## Yield
## not numerically recorded. yield was described as "better", "worse", "same" in experiment.csv under relative_performance_plot_N_yield
	d$yield_part <- "pod" # recorded as unshelled pods, no values for shelled grain given
	d$yield_moisture <- NA # not recorded
	d$yield_isfresh <- NA # not stated

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
	d$fwy_storage <- (d$nodule_weight/d$plot_area) * 10000 # converted to kg/ha
	d$yield <- d$fwy_storage
	d$dmy_storage <- NA  # moisture content not recorded
	d$dmy_total <- (d$biomass_kg/d$plot_area) * 10000
	
# all scripts must end like this
 	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

