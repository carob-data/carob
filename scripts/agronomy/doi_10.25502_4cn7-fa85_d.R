# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava Weed Management Data - Herbicide Screening 2014

The ‘Sustainable Weed Management Technologies for Nigeria’ was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.
Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.
Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria. IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.
Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria.
"

	uri <- "doi:10.25502/4cn7-fa85/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "AfricaRice",
		publication = "doi:10.1017/wet.2020.26",
		project = NA,
		carob_date = "2025-10-29",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_used",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "herbscreen2014_weedspp_first_second_seasons_all_loc_datafile_rft.csv"]
	f2 <- ff[basename(ff) == "herbscreen2014_yield_first_second_seasons_all_loc_datafile_rft.csv"]
	#f3 <- ff[basename(ff) == "metadata_herbscreen2014_weedspp_first_second_seasons_all_loc_datafile_rft.csv"]
	#f4 <- ff[basename(ff) == "metadata_herbscreen2014_yield_first_second_seasons_all_loc_datafile_rft.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	#r4 <- read.csv(f4)


	### process weed species 
	
	d1 <- data.frame(
	   trial_id = r1$UniqueID,
	   year = r1$Year,
	   season = r1$Season,
	   adm1 = r1$Location,
	   location = carobiner::fix_name(r1$Site, "title"),
	   plot_no = r1$PlotNo,
	   rep = r1$Rep,
	   treatment = r1$Treatment,
	   weed_species = r1$WeedSpecies
	   #weed_type = r1$weedtype
	)
	
	### process yield data 
	
	d2 <- data.frame(
		trial_id = r2$UniqueID,
		year = r2$Year,
		season = r2$Season,
		adm1 = r2$Location,
		location = carobiner::fix_name(r2$Site, "title") ,
		rep = r2$Rep,
		treatment = r2$Treatment,
		latitude = r2$Latitude,
		longitude = r2$Longitude,
		yield = r2$WtTRootthaNt*1000,
		yield_marketable = r2$WtOkRootthaNt*1000,
		weeding_done = TRUE,
		planting_date = as.character(as.Date(r2$Date_Planted_Cas, "%m/%d/%Y")),
		soil_pH = r2$pH_H2O,
		soil_SOC = r2$OC,
		soil_N = r2$N*10000,
		soil_P = r2$P,
		soil_Ca_exch= r2$Ca,
		soil_Mg_exch = r2$Mg,
		soil_K_exch = r2$K,
		soil_sand = r2$SAND_pct,
		soil_silt = r2$SILT_pct,
		soil_clay = r2$CLAY_pct,
		crop = "cassava",
		country = "Nigeria",
		geo_from_source = TRUE,
		on_farm = TRUE, 
		is_survey = FALSE,
		yield_part = "roots", 
		yield_moisture = as.numeric(NA), 
		irrigated = NA,
		plot_no = r2$PlotNo,
		plot_area = 16 # m2
	)
	
	
	### merge da and d2
	
 d	<- merge(d1, d2, intersect(names(d1), names(d2)), all.x = TRUE)
 
 ## remove rows with NA in yield
 d <- d[!is.na(d$yield),] 	
 
 ### Adding herbicide product and amount

   d$herbicide_amount <- gsub("[^0-9\\.]", "", d$treatment)
   d$herbicide_amount[d$herbicide_amount==""] <- NA
   d$herbicide_amount <- as.numeric(d$herbicide_amount)
	d$herbicide_product <- gsub("[0-9\\.]+L", "", d$treatment) 
	
	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("Authority", "sulfentrazone", P)
	P <- gsub("Callisto", "mesotrione", P)
	P <- gsub("Metric", "metribuzin", P)
	P <- gsub("Vigon", "flufenacet;diflufenican;flurtamone", P)
	P <- gsub("Wing-P", "pendimethalin;dimethenamid-p", P)
	P <- gsub("Gardoprim", "terbuthylazine", P)
	P <- gsub("Merlin75WG", "isoxaflutole", P)
	P <- gsub("SencorPlus", "metribuzin", P)
	P <- gsub("Stallion", "clomazone", P)
	P <- gsub("Challenge", "aclonifene", P)
	P <- gsub("MerlinFlexx", "isoxaflutole", P)
	P <- gsub("Bullet", "tebuthiuron", P)
	P <- gsub("Lagon", "isoxaflutole", P)
	P <- gsub("GoalTender", "oxyfluorfen", P)
	P <- gsub("CodalGold", "metolachlor", P)
	P <- gsub("Movon", "flufenacet;diflufenican;flurtamone", P)
	P <- gsub("PrimextraG", "s-metolachlor", P)
	P <- gsub("Liberator|Liberator1.5", "flufenacet;diflufenican", P)
	P <- gsub("Fierce0.48kg|Fierce0.32kg", "flumioxazin", P)
	P <- gsub("MerlinTotal", "isoxaflutole", P)
	P <- gsub("HoeWeeded plot|Unweeded plot", "none", P)
	P <- gsub("Sencor", "metribuzin", P)
	d$herbicide_product <- P
	
d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
d$season <- d$plot_no <- d$year <- NULL	
d$herbicide_used <- !grepl("none", d$herbicide_product)	

carobiner::write_files(path, meta, d)

}

