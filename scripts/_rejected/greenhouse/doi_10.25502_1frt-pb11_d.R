# R script for "carob"
# license: GPL (>=3)

#### NOTES
#  - morphophysiological data (f3) not merged in "d" as its an in-vitro stress-tolerance for isolates grown in test-tubes
#  - Pot trial happened in Brazil at Lat -23.1936 and Lon -51.1842 (publication);
#  - But isolates (rhizobia bacteria) came from soybean root nodules collected at 15 sites across 4 provinces in Mozambique
#  - Current lat/lon are from where samples were isolated (mozambique)
#  - publication reports temp and humidity two times a day as follows:
##            - Trial 1: 09h00 26.0 +/- 1.9 C / 15h00 30.3 +/- 2.9 C; RH 67.0 +/- 9.6% / 54.6 +/- 7.1%
##            - Trial 2: 09h00 22.1 +/- 1.6 C / 15h00 25.0 +/- 2.8 C; RH 69.1 +/- 6.3% / 66.1 +/- 8.1%

### Suggested terms: shoot_N (Total nitrogen accumulation in shoot per plant; TNS)
#                 : sample_date (date of sampling in mozambique)
#                 : isolate_variety (soybean variety where isolate we sampled)

#### ISSUES
# - Available metrics are per plant and no way to convert to kg/ha
#          - NDW = nodule dry weight per plant in mg
#          - SDW = shoot dry weight in g per plant
#          - TNS = Total nitrogen accumulation in shoot per plant
# - Temperature & humidity of the greenhouse not added yet; reported 2 times a day (publication)
#                - if we elongate and add column "weather_record_time", it will double the rows
# - Beyond country the USDA and SEMIA isolates don't have any location or XY of origin


carob_script <- function(path) {

"Isolation, characterization and selection of indigenous Bradyrhizobium strains 
with outstanding symbiotic performance to increase soybean yields in Mozambique

Science-based “research-in-development” project focused on putting nitrogen 
fixation to work for smallholder farmers growing legume crops in Africa"

	uri <- "doi:10.25502/1frt-pb11/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.agee.2017.06.017",
		project = "N2Africa",
		design = "Two greenhouse pot 'authentication' trials, RCBD, Embrapa Soja, Londrina, Brazil. Trial 1: 94 treatments, cv. BRS 133. Trial 2: 20 treatments x 3 cultivars.",
		data_type = "experiment",
		treatment_vars = "variety; treatment",
		response_vars = "node_count",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-08-26",
		carob_completion = 85,
		carob_effort = 3
	)

	f1 <- ff[basename(ff) == "chibeba_et_al_2017_indigenous_rhizobia_database_site_description_mozambique.csv"] #site description
	f2 <- ff[basename(ff) == "chibeba_et_al_2017_indigenous_rhizobia_database_site_first_greenhouse_trial.csv"] # first greenhouse trial
	f3 <- ff[basename(ff) == "chibeba_et_al_2017_indigenous_rhizobia_database_site_morphophysiological_data.csv"] #morphophysiological data
	f4 <- ff[basename(ff) == "chibeba_et_al_2017_indigenous_rhizobia_database_site_second_greenhouse_trial.csv"] #second green house trial
	f5 <- ff[basename(ff) == "metadata_chibeba_et_al_2017_indigenous_rhizobia_database.xlsx"] # metadata
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)   
	r4 <- read.csv(f4)
	
	r5a <- carobiner::read.excel(f5, sheet = "Site_Description_Metadata")
	r5b <- carobiner::read.excel(f5, sheet = "First_Greenhouse_Trial_Metadata")
	r5c <- carobiner::read.excel(f5, sheet = "Morphophysiological_Metadata")
	r5d <- carobiner::read.excel(f5, sheet = "Sec_Greenhouse_Trial_Metadata")
	
	## Trial 1: drop trailing blank export rows
	reference_origin <- c("USDA 110" = "USA","USDA 100" = "USA","SEMIA 587" = "Brazil",
	  "SEMIA 5019" = "Brazil","SEMIA 5079" = "Brazil","SEMIA 5080" = "Brazil","SEMIA  5080" = "Brazil"   # Trial 1's file has a double space in this specific value
	)
	
	r2 <- r2[!is.na(r2[["Treatment"]]) & trimws(r2[["Treatment"]]) != "", ]
	
	d2 <- data.frame(
	  trial_id = r2$Greenhouse_trial_data,
	  block_id = r2$Block,
	  country = ifelse(!is.na(reference_origin[r2$Treatment]), reference_origin[r2$Treatment],
	                   ifelse(is.na(r2$Province) | trimws(r2$Province) == "", NA_character_, "Mozambique")),
	  adm1 = r2$Province,
	  location = r2$Location,
	  treatment = r2$Treatment,
	  variety = "BRS 133",   # the actual pot-trial cultivar - Trial 1 used one variety throughout, confirmed from publication
	  node_count = r2$NN,
	  shoot_N = r2$TNS,
	  on_farm = FALSE
	)
	
	#### Second trial
	variety_key <- c("1" = "TGx 1963-3F", "2" = "TGx 1835-10E", "3" = "BRS 284")
	
	d4 <- data.frame(
	  trial_id = r4$Greenhouse_trial_data,
	  block_id = r4$Block,
	  country = ifelse(!is.na(reference_origin[r4$Treatment]), reference_origin[r4$Treatment],
	                   ifelse(is.na(r4$Province) | trimws(r4$Province) == "", NA_character_, "Mozambique")),
	  adm1 = r4$Province,
	  location = r4$Location,
	  treatment = r4$Treatment,
	  variety = variety_key[as.character(r4$Variety)],   #pot-cultivar
	  node_count = r4$NN,
	  shoot_N = r4$TNS,
	  on_farm = FALSE
	)
	
	## Geocoordinates and isolate origin (trap-crop variety, sampling date) in Mozambique
	site <- data.frame(adm1 = r1[["Province_nodule_sampling"]],
	                   location = r1[["Sitename_nodule_sampling"]],
	                   latitude = -as.numeric(gsub("[^0-9.]", "", r1[["Latitude_S_nodule_sampling"]])),
	                   longitude = as.numeric(gsub("[^0-9.]", "", r1[["Longitude_E_nodule_sampling"]])),
	                   isolate_variety = r1[["Soybean_variety"]],
	                   sample_date = as.character(as.Date(r1[["Sampling_date"]], format = "%m/%d/%Y")))
	
	## combine both trials
	d <- rbind(d2, d4)
	d$trial_id <- ifelse(d$trial_id == "First", "1", "2")
	
	## join site coordinates by matching province + location name
	origin <- match(paste(d$adm1, d$location), paste(site$adm1, site$location))
	d$latitude <- site$latitude[origin]
	d$longitude <- site$longitude[origin]
	d$geo_from_source <- !is.na(d$latitude)
	d$crop <- "soybean"
	d$isolate_variety_ <- site$isolate_variety[origin]   # trap-crop variety in Mozambique 
	d$sample_date_ <- site$sample_date[origin]
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)

## no harvested crop yield: pot trial scored for nodulation/biomass 35 DAE (trial 1) & 41 DAE (trial 2)
	d$yield <- as.numeric(NA)
	d$yield_part <- "none"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- as.logical(NA)
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$planting_date <- NA
	d$harvest_date <- NA
	d$country[d$country == "USA"] <- "United States"
	d$block_id <- as.character(d$block_id)
	
	d$adm1 <- ifelse(trimws(d$adm1) == "", NA_character_, d$adm1)
	d$location <- ifelse(trimws(d$location) == "", NA_character_, d$location)


	carobiner::write_files(path, meta, d)
}

