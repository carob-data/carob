# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava Weed Management Data - On farm trials 2018

'The ‘Sustainable Weed Management Technologies for Nigeria’ was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.

Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.

Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria. IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.

Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria.'
"

	uri <- "doi:10.25502/n3j7-1e64/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		carob_date = "2025-11-25",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_used",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "onfarm2018_all_locations_datafile_rft.csv"]
	#f2 <- ff[basename(ff) == "metadata_onfarm2018_all_locations_datafile_rft.csv"]

	r <- read.csv(f)
	#r2 <- read.csv(f2)


	d1 <- data.frame(
		trial_id = r$UniqueID,
		year = r$Year,
		adm1 = r$Location,
		location = r$Site,
		crop_system = r$Cropsystem,
		plot_id = as.character(r$PlotNo),
		herbicide1 = r$Pretreat,
		herbicide2 = r$Posttreat,
		treatment = r$Treatment,
		herbicide_timing = as.character(r$PostApplWAT),
		latitude = r$Latitude,
		longitude = r$Longitude,
		fwy_stems_c = r$Wtstemtha*1000,
		fwy_roots_c = r$RootYldtha*1000,
		yield_c = r$RootYldtha*1000 ,
		yield_moisture = as.numeric(NA),
		yield_isfresh = TRUE,
		planting_date_c = as.character(as.Date(r$Date_Planted_Cas, "%m/%d/%Y")),
		harvest_date_c = as.character(as.Date(r$Date_Harvested_Cas, "%m/%d/%Y")),
		yield_m= r$MzGrainYld_tha*1000,
		planting_date_m = as.character(as.Date(r$Date_Planted_Mz, "%m/%d/%Y")),
		harvest_date_m = as.character(as.Date(r$Date_Harvested_Mz, "%m/%d/%Y")),
		fwy_stems_m = NA,
		fwy_roots_m = NA,
		country = "Nigeria", 
		on_farm = TRUE, 
		is_survey = FALSE, 
		crop = "cassava", 
		irrigated = NA, 
		geo_from_source = TRUE
		
	)

	d <- reshape(d1, varying = list(c("yield_c", "yield_m"), c("fwy_stems_c", "fwy_stems_m"), c("fwy_roots_c", "fwy_roots_m"), c("planting_date_c", "planting_date_m"), c("harvest_date_c", "harvest_date_m")),
	             v.names = c("yield", "fwy_stems", "fwy_roots", "planting_date", "harvest_date"),
	             timevar = "crop",
	             time= c("cassava", "maize"),
	             direction = "long")
	
	d$intercrops <- ifelse(grepl("cassava", d$crop) & grepl("Intercrop", d$crop_system), "maize", 
	                       ifelse(grepl("cassava", d$crop) & grepl("Monocrop", d$crop_system), "none", "cassava"))
	d$yield_part <- ifelse(grepl("cassava", d$crop), "roots", "grain")
	d$yield_moisture <- ifelse(grepl("maize", d$crop), 12, d$yield_moisture)
	d$yield_isfresh <- !grepl("maize", d$crop)  
	### drop rows with NA in yield 
	d <- d[!is.na(d$yield),]
	
	### Fixing herbicide product
	
	P <- carobiner::fix_name(d$herbicide1)
	P <- gsub("Lagon", "isoxaflutole", P)
	P <- gsub("Primextra", "s-metolachlor", P)
	P <- gsub("HoeWeeding", "none", P)
	P <- gsub("Untreated", "none", P)
	P <- gsub("FarmerPractice", "none", P)
	d$herbicide1 <- P
	
	P <- carobiner::fix_name(d$herbicide2)
	P <- gsub("1HW", "unknown", P)
	P <- gsub("MaisterPower", "iodosulfuron-methyl-sodium", P)
	P <- gsub("ZeroPost|Untreated|FarmerPractice", "none", P)
	P <- gsub("HW_4_8_12WAP", "unknown", P)
	P <- gsub("Roundup", "glyphosate", P)
	P <- gsub("Fusilade\\+lifelin", "fluazifop-p-butyl", P)
	d$herbicide2 <- P
	
	d$herbicide_product <- gsub("none;none", "none", paste(d$herbicide1, d$herbicide2, sep = ";")) 
	d$herbicide_used <- !grepl("^none$", d$herbicide_product) 
	
	d$year <- d$crop_system <- d$herbicide1 <- d$herbicide2 <- d$id <- NULL
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	
carobiner::write_files(path, meta, d)
}


