# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava Weed Management Data

The ‘Sustainable Weed Management Technologies for Nigeria’ was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.
Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.
Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria.
IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.
Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria.
"


	uri <- "doi:10.25502/j046-5k77/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1017/wet.2020.26",
		project = NA,
		carob_date = "2025-11-19",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_used",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "herbicide_screen_2015.csv"]
	f2 <- ff[basename(ff) == "herbscreen2015_spp_allloc.csv"]
	#f3 <- ff[basename(ff) == "herbicide_screen_2015_metadata.csv"]
	#f4 <- ff[basename(ff) == "herbscreen2015_spp_allloc_metadata.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	#r4 <- read.csv(f4)

	#### process cassava yield
	
	d1 <- data.frame(
		trial_id = r1$UniqueID,
		year = r1$Year,
		season = r1$Season,
		adm1 = r1$Location,
		location = carobiner::fix_name(r1$Site, "title"),
		crop_system= r1$CropSystem,
		rep = r1$Rep,
		plot_id = as.character(r1$PlotNo),
		herbicide1 = r1$Pretreat,
		herbicide2 = r1$Posttreat,
		treatment = r1$Treatment,
		weeding_done = TRUE,
		yield_c = r1$AllRootYldtha*1000,
		fwy_stems_c = r1$Wtstemtha*1000,
		fwy_stems_m = NA,
		planting_date_c = as.character(as.Date(r1$Date_Planted_Cas, "%Y/%m/%d")),
		harvest_date_c = as.character(as.Date(r1$Date_Harvested_Cas, "%Y/%m/%d")),
		yield_m = r1$MzGrainYld_tha*1000,
		planting_date_m = as.character(as.Date(r1$Date_Planted_Mz, "%Y/%m/%d")),
		harvest_date_m = as.character(as.Date(r1$Date_Harvested_Mz, "%Y/%m/%d")),
		herbicide_timing = as.character(r1$DaysPostApplied),
		soil_pH = r1$pH_d10,
		soil_pH1 = r1$pH_H2O_d20,
		soil_SOC = r1$OC_d10,
		soil_SOC1 = r1$OC_d20,
		soil_N = r1$N_d10*10000,
		soil_N1 = r1$N_d20*10000,
		soil_P = r1$P_d10,
		soil_P1 = r1$P_d20,
		soil_Ca = r1$Ca_d10,
		soil_Ca1 = r1$Ca_d20,
		soil_Mg = r1$Mg_d10,
		soil_Mg1 = r1$Mg_d20,
		soil_K = r1$K_d10,
		soil_K1 = r1$K_d20,
		soil_sand = r1$SAND_pct_d10,
		soil_sand1 = r1$SAND_pct_d20,
		soil_silt = r1$SILT_pct_d10,
		soil_silt1 = r1$SILT_pct_d20,
		soil_clay = r1$CLAY_pct_d10,
		soil_clay1 = r1$CLAY_pct_d20,
		
		crop = "cassava",
		country = "Nigeria",
		geo_from_source = FALSE,
		on_farm = TRUE, 
		is_survey = FALSE,
		yield_moisture = as.numeric(NA), 
		irrigated = NA
	)


	d <- reshape(d1, varying = list(c("yield_c", "yield_m"), c("planting_date_c", "planting_date_m"), c("harvest_date_c", "harvest_date_m"), c("fwy_stems_c", "fwy_stems_m")), 
	             v.names =c("yield", "planting_date", "harvest_date", "fwy_stems"),
	             timevar = "crop" ,
	             times = c("cassava", "maize"),
	             direction = "long")
	
	d$intercrops <- ifelse(grepl("cassava", d$crop) & grepl("Intercrop", d$crop_system), "maize", 
	               ifelse(grepl("cassava", d$crop) & grepl("Monocrop", d$crop_system), "none", "cassava"))
  
	d <- d[!is.na(d$yield),]
	d$yield_part = ifelse(grepl("cassava", d$crop), "roots", "grain")
	
	### process weed species
	d2 <- data.frame(
	   record_id = as.integer(1:nrow(r2)),
		trial_id = r2$UniqueID,
		season = r2$Season ,
		adm1 = r2$Location,
		location = carobiner::fix_name(r2$Site, "title"),
		plot_id = as.character(r2$PlotNo) ,
		treatment = r2$Treatment ,
		crop_system= r2$CropSystem,
		rep = r2$Rep,
		herbicide1 = r2$Pretreat,
		herbicide2 = r2$Posttreat,
		weed_species = r2$WeedSpecies
	)
	
	### merge d and d2 
	d <- merge(d2, d, intersect(names(d), names(d2)), all = TRUE)
	
	d <- d[!duplicated(d$record_id),]
	### remove one row with NA in record_id
	d <- d[!is.na(d$record_id),]
	
	## remove rows with NA in yield 
	d <- d[!is.na(d$yield),]
	
	### Adding herbicide amount
	d$herbicide_amount <- as.numeric(gsub("[^0-9\\.]", "", d$herbicide1))+as.numeric(gsub("[^0-9\\.]", "", d$herbicide2)) 
	
	
	## fixing herbicide 
	
	P <- carobiner::fix_name(d$herbicide1)
	P <- gsub("Goal4F", "oxyfluorfen", P)
	P <- gsub("NoPre\\+HoeWeeded", "none", P)
	P <- gsub("Stallion3L", "pendimethalin", P)
	P <- gsub("Lagon1.25L", "isoxaflutole;aclonifen", P)
	P <- gsub("Authority1.25L", "sulfentrazone", P)
	P <- gsub("Gardoprin5L", "terbuthylazine", P)
	P <- gsub("Fierce0.32kg", "flumioxazin", P)
	P <- gsub("Vigon1.5L", "diflufenican", P)
	P <- gsub("MerlinTotal0.50L", "isoxaflutole", P)
	P <- gsub("Primextra4L", "atrazine;S-metolachlor", P)
	P <- gsub("Movon1.5L", "flufenacet;diflufenican;flurtamone", P)
	P <- gsub("CodalGold4L", "prometryn", P)
	P <- gsub("SencorPlus1.5L", "metribuzin;indaziflam", P)
	d$herbicide1 <- P
	
	P <- carobiner::fix_name(d$herbicide2) 
	P <- gsub("Cobra\\+SelectMax", "lactofen;clethodim", P)
	P <- gsub("Envoke7g", "trifloxysulfuron-sodium", P)
	P <- gsub("NoPre\\+HoeWeeded|ZeroPost|HoeWeeded", "none", P)
	P <- gsub("MaisterPower1.5L|MaisterPower1.0L|Maister61WG", "foramsulfuron;iodosulfuron;thiencarbazone ", P)
	d$herbicide2 <- P
	
	d$herbicide_product <- trimws(gsub("none;none", "none", paste(d$herbicide1, d$herbicide2, sep = ";")))
	d$herbicide_used <- !grepl("^none$", d$herbicide_product)
	
	d$herbicide1 <- d$herbicide2 <- d$id <- d$crop_system <- d$season <- d$year <- NULL
	
	### Fixing long and lat 
	
	geo <- data.frame(
	   location = c("Nrcri", "Aksu" , "Uam" , "Funaab", "Iita_westbank", "Iita_onne" ),
	   longitude = c(8.806, 7.768, 8.618, 3.449, 3.889, 7.176),
	   latitude = c(9.736, 4.622, 7.775, 7.228, 7.486, 4.706)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### records Soil data in long format base on deph 
	
	soil_var <- d[, grepl("soil|record_id", names(d))]
	name_var <- names(d)[grepl("soil", names(d))] 
   dSoil <- reshape(soil_var, list(c(name_var)), v.names = "value",
                   timevar = "step",
                   direction = "long")
	dSoil$variable <- c(rep("soil_pH", 2), rep("soil_SOC", 2),  rep("soil_N", 2),  rep("soil_P", 2),  rep("soil_Ca", 2),  rep("soil_Mg", 2),  rep("soil_K", 2),  rep("soil_sand", 2),  rep("soil_silt", 2),  rep("soil_clay", 2))[dSoil$step]
	dSoil$depth <- rep(c(10, 20), time=10)[dSoil$step]
	dSoil$id <- dSoil$step <- NULL
	
	d <- d[, !grepl("soil", names(d))]
	
	
carobiner::write_files(path, meta, d, long = dSoil)

}


