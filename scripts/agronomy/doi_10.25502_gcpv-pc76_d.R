# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava Weed Management Data - On farm trials 2016

The ‘Sustainable Weed Management Technologies for Nigeria’ was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.

Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.

Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria. IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.

Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria.
"


	uri <- "doi:10.25502/gcpv-pc76/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1564/v27_oct_04",
		project = NA,
		carob_date = "2025-11-24",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_used",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	
	f1 <- ff[basename(ff) == "onfarm2016_all_locations_datafile_rft.csv"]
	f2 <- ff[basename(ff) == "onfarm2016_all_locations_weedspecies_rft.csv"]
	#f3 <- ff[basename(ff) == "metadata_onfarm2016_all_locations_datafile_rft.csv"]
	#f4 <- ff[basename(ff) == "metadata_onfarm2016_all_locations_weedspecies_rft.csv"]
	#f5 <- ff[basename(ff) == "codelist_onfarm2016_all_locations_weedspecies_rft.csv"]
	
	#####
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	#r4 <- read.csv(f4)
	#r5 <- read.csv(f5)


#### process 
	d1 <- data.frame(
		trial_id = r1$UniqueID,
		year = r1$Year,
		adm1 = r1$Location,
		location = r1$Site,
		plot_id = as.character(r1$PlotNo),
		crop_system = r1$Cropsystem,
		herbicide1 = r1$Pretreat,
		herbicide2 = r1$Posttreat,
		treatment = r1$Treatment,
		herbicide_timing = as.character(r1$PostApplWAT),
		latitude = r1$Latitude,
		longitude = r1$Longitude,
		fwy_stems = r1$Wtstemtha*1000,
		fwy_roots = r1$RootYldtha*1000,
		yield = r1$RootYldtha*1000 ,
		planting_date = as.character(as.Date(r1$Date_Planted_Cas, "%m/%d/%Y")),
		harvest_date= as.character(as.Date(r1$Date_Harvested_Cas, "%m/%d/%Y")),
		soil_pH = r1$pH_H2O,
		soil_SOC = r1$OC,
		soil_N = r1$N*10000,
		soil_P = r1$P,
		soil_Ca_exch= r1$Ca,
		soil_Mg_exch = r1$Mg,
		soil_K_exch = r1$K,
		soil_sand = r1$SAND_pct,
		soil_silt = r1$SILT_pct,
		soil_clay = r1$CLAY_pct,
		country = "Nigeria", 
		on_farm = TRUE, 
		is_survey = FALSE, 
		crop = "cassava", 
		yield_part = "roots", 
		yield_moisture = as.numeric(NA), 
		yield_isfresh = TRUE,
		irrigated = NA, 
		geo_from_source = TRUE
	)

	d1$intercrops <- ifelse(grepl("Intercrop", d1$crop_system), "maize", "none") 
	
	### process weed species

	d2 <- data.frame(
	   record_id = as.integer(1:nrow(r2)),
		trial_id = r2$UniqueID,
		year = r2$Year,
		adm1 = r2$Location,
		location = r2$Site,
		plot_id = as.character(r2$PlotNo),
		crop_system = r2$Cropsystem,
		herbicide1 = r2$Pretreat,
		herbicide2 = r2$Posttreat,
		treatment = r2$Treatment,
		weed_species = r2$WeedSpecies
	)

	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	
	d <- d[!duplicated(d$record_id),]
	
	### drop one row with NA in record_id 
	d <- d[!is.na(d$record_id),]
	
	### drop rows with NA in yield 
	d <- d[!is.na(d$yield),]
	
	### fixing herbicide product names
	P <- carobiner::fix_name(d$herbicide1)
	P <- gsub("Movon", "diflufenican", P)
	P <- gsub("Lagon", "isoxaflutole", P)
	P <- gsub("Gardoprim", "terbuthylazine", P)
	P <- gsub("Primextra", "s-metolachlor", P)
	P <- gsub("Fierce", "flumioxazin", P)
	P <- gsub("NoPre", "none", P)
	P <- gsub("FarmerPractise", "none", P)
	P <- gsub("MerlinTotal", "isoxaflutole", P)
	P <- gsub("SencorPlus", "metribuzin", P)
	d$herbicide1 <- P
	
	
	P <- carobiner::fix_name(d$herbicide2)
	P <- gsub("Longhandledhoe", "none", P)
	P <- gsub("Maister61WG", "foramsulfuron;iodosulfuron-methyl-sodium", P)
	P <- gsub("MaisterPower", "iodosulfuron-methyl-sodium", P)
	P <- gsub("SmallMantis", "none", P)
	P <- gsub("Shorthandledhoe", "none", P)
	P <- gsub("ZeroPost", "none", P)
	P <- gsub("FarmerPractise", "none", P)
	P <- gsub("Fusilade\\+Cobra", "fluazifop-p-butyl", P)
	P <- gsub("Glyphosate", "glyphosate", P)
	P <- gsub("SelectMax\\+Cobra", "clethodim", P)
	d$herbicide2 <- P
	
	d$herbicide_product <- gsub("none;none", "none", paste(d$herbicide1, d$herbicide2, sep = ";")) 
	d$herbicide_used <- !grepl("^none$", d$herbicide_product) 
	
	d$year <- d$crop_system <- d$herbicide1 <- d$herbicide2 <- NULL
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
  ## Fixing geo-coordinate errors 
  
	i <- grepl("Otukpo_Ogboju", d$location) & grepl("Benue", d$adm1)
	d$longitude[i] <- 8.133
	d$latitude[i] <- 7.1909
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("RemoNorth_OdeRemo", d$location) & grepl("Ogun", d$adm1)
	d$longitude[i] <- 3.682
	d$latitude[i] <- 6.965
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("RemoNorth_Odogbolu", d$location) & grepl("Ogun", d$adm1)
	d$longitude[i] <- 3.768
	d$latitude[i] <- 6.836 
	d$geo_from_source[i] <- FALSE
	

	carobiner::write_files(path, meta, d)

}

