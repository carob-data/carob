# R script for "carob"
# license: GPL (>=3)

## ISSUES

## Fertilizer amount applied and type are missing

carob_script <- function(path) {

"
Cassava Weed Management Data - Agronomy Trials 2015

The ‘Sustainable Weed Management Technologies for Nigeria’ was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.

Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.

Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria. IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.

Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria.
"


	uri <- "doi.org/10.25502/2rb2-6439/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		carob_date = "2025-11-26",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "agro2015_1st_season_all_locations_cas_datafile_rft.csv"]
	f2 <- ff[basename(ff) == "agro2015_2nd_season_all_locations_cas_datafile_rft.csv"]
	#f3 <- ff[basename(ff) == "metadata_agro2015_1st_season_all_locations_cas_datafile_rft.csv"]
	#f4 <- ff[basename(ff) == "metadata_agro2015_2nd_season_all_locations_cas_datafile_rft.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	#r4 <- read.csv(f4)

### process
	
	## first season
	d1 <- data.frame(
	   trial_id = r1$UniqueID,
	   year = r1$Year,
	   season = r1$Season,
	   adm1 = r1$Loc,
	   location = r1$Site,
	   rep = r1$Rep,
	   land_prep_method = ifelse(grepl("Ridge", r1$Tillage), "ridge tillage", "reduced tillage"),
	   crop_system = r1$cropSystem,
	   fertilizer_used= grepl("Fert", r1$Fertilizer) ,
	   variety = r1$Variety,
	   plant_density = r1$plantsm2*10000,
	   fwy_roots = r1$YLDOKfr_kgm2*10000,
	   yield = r1$YLDOKfr_kgm2*10000,
	   fwy_stems = r1$mstemfr_kgm2*10000,
	   latitude = r1$Lat,
	   longitude = r1$Long,
	   planting_date = as.character(as.Date(r1$Date_Planted_Cas, "%m/%d/%Y")),
	   harvest_date = as.character(as.Date(r1$Date_Harvested_Cas, "%m/%d/%Y")),
	   country = "Nigeria",
	   crop = "cassava", 
	   soil_pH = r1$pH_d10,
	   soil_pH1 = r1$pH_d20,
	   soil_SOC = r1$OC_d10,
	   soil_SOC1 = r1$OC_d20,
	   soil_N = r1$N_d10*10000,
	   soil_N1 = r1$N_d20*10000,
	   soil_P = r1$P_d10,
	   soil_P1 = r1$P_d20,
	   soil_Ca_exch = r1$Ca_d10,
	   soil_Ca1_exch = r1$Ca_d20,
	   soil_Mg_exch = r1$Mg_d10,
	   soil_Mg1_exch = r1$Mg_d20,
	   soil_K_exch = r1$K_d10,
	   soil_K1_exch = r1$K_d20,
	   on_farm = TRUE, 
	   is_survey = FALSE, 
	   yield_part = "roots", 
	   yield_moisture = as.numeric(NA), 
	   yield_isfresh = TRUE,
	   irrigated = NA, 
	   geo_from_source= TRUE
	)

	### second season
	d2 <- data.frame(
	   trial_id = r2$UniqueID,
	   year = r2$Year,
	   season = r2$Season,
	   adm1 = r2$Loc,
	   location = r2$Site,
	   rep = r2$Rep,
	   land_prep_method = ifelse(grepl("Ridge", r2$Tillage), "ridge tillage", "reduced tillage"),
	   crop_system = r2$cropSystem,
	   fertilizer_used= grepl("Fert", r2$Fertilizer) ,
	   variety = r2$Variety,
	   plant_density = r2$Plantsm2*10000,
	   fwy_roots = r2$Yldokfr_Kgm2*10000,
	   yield = r2$Yldokfr_Kgm2*10000,
	   fwy_stems = r2$Mstemfr_Kgm2*10000,
	   latitude = r2$Lat,
	   longitude = r2$Long,
	   planting_date = as.character(as.Date(r2$Date_Planted_Cas, "%m/%d/%Y")),
	   harvest_date = as.character(as.Date(r2$Date_Harvested_Cas, "%m/%d/%Y")),
	   country = "Nigeria",
	   crop = "cassava", 
	   on_farm = TRUE, 
	   is_survey = FALSE, 
	   yield_part = "roots", 
	   yield_moisture = as.numeric(NA), 
	   yield_isfresh = TRUE,
	   irrigated = NA, 
	   geo_from_source= TRUE
	)
	
	#### join d1 and d2
	
	d <- carobiner::bindr(d1, d2)
	
	d$intercrops <- ifelse(grepl("CasMz", d$crop_system), "maize", "none") 
   d$record_id  <- as.integer(1:nrow(d)) 
   
   ## Fixing long and lat 
   
   geo <- data.frame(
      location = c("Umudike","Makurdi","Otobi", "Funaab" , "Isara","Ayedire", "Ido"),
      lon = c(7.543, 8.540, 8.0582, 3.449, 3.678, 4.597, 4.489),
      lat = c(5.479, 7.733, 7.1020, 7.228, 6.987, 7.803, 7.803),
      geo_from = FALSE
   )
   
   d <- merge(d, geo, by= "location", all.x = TRUE)
   
   d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
   d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
   d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
   
   d$year <- d$crop_system <- d$geo_from <- d$lon <- d$lat <- d$season <- NULL
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
   
   
	### Record soil in long format based on depth
	
   soil_var <- d[, grepl("soil|record_id", names(d))]
	var <- names(d)[grep("soil", names(d))]
   
	dsoil <- reshape(soil_var, varying = var, v.names = "value",
	                 timevar = "step",
	                  direction = "long" )
	dsoil$depth <- c(rep(c(10, 20), time= 7))[dsoil$step]
	dsoil$variable <- c("soil_pH", "soil_pH", "soil_SOC", "soil_SOC", "soil_N", "soil_N", "soil_P","soil_P", "soil_Ca_exch", "soil_Ca_exch", "soil_Mg_exch", "soil_Mg_exch", "soil_K_exch", "soil_K_exch")[dsoil$step]
   
	dsoil <- dsoil[!is.na(dsoil$value),]
	dsoil$id <- dsoil$step <- NULL
 	
	d <- d[, !grepl("soil", names(d))]
	
carobiner::write_files(path, meta, d, long = dsoil)

}

