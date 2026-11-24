# R script for "carob"
# license: GPL (>=3)


## Notes
#New variable added:
  
# biochar_used: The raw data indicate whether biochar was used, recorded as a logical variable. The amount of biochar applied was not reported.

## Issues
# Some rows indicate the use of both inorganic and organic fertilizers (Inorganic + organic), but the application rates for either fertilizer were not provided.
## missing data in treatment variables ( N_fertilizer, Cover_crop, and land_prep_method)

carob_script <- function(path) {

"
Soil nitrous oxide emissions from global specialty crop systems

The data were extracted from published studies. All references and data sources are listed.   We conducted an exhaustive literature search using Google Scholar and Web of Science databases. The searched keywords were 'soil nitrous oxide emission', 'soil greenhouse gas emission', and 'soil trace gas emission' respectively combined with each specialty crop. We searched for the common specialty crops listed by USDA, including fruits, tree nuts, and vegetables (https://www.ams.usda.gov/services/grants/scbgp/specialty-crop). The most recent search was in November 2023. For each article returned by the search, we evaluated whether it reported field observations of annual cumulative N2O emissions. Laboratory and greenhouse studies were excluded. This process identified 1137 observations from 114 studies. These studies covered six continents and four main climate types (Köppen climate classification). Area-scaled N2O emissions were reported as annual cumulative emissions (kg N2O-N ha−1 year−1).  Yield-scaled N2O emissions were calculated by dividing area-scaled N2O emissions by fresh weight yield (kg N2O-N Mg fresh yield−1). The emission factor (EF) for annual cumulative N2O emission was calculated by EF (%) = (EN − E0)/Nrate. The EN and E0 are the N2O emissions (kg N2O-N ha−1 year−1) with and without N fertilizer, respectively. The Nrate is the N fertilizer application rate (kg N ha−1 year−1). Soil properties and environmental factors were also listed if they were avalable. Empty cells in the datasheet indicate no data avalable for that category or variable.
"

	uri <- "doi:10.5061/dryad.tht76hf5z"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "COLSU",
		publication = "doi:10.1111/gcb.17233",
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "N_fertilizer;N_organic;irrigated;land_prep_method;cover_crop",
		response_vars = "yield;emission_N2O", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-11-07",
		carob_completion = 100,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "meta.xlsx"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, fix_names = TRUE)
	

	d <- data.frame(
	  emission_N2O = r1$Annual.cumulative.N2O.N.kg.ha.1.year.1,
		yield = r1$Annual.crop.yield.fresh.weight.Mg.ha.1.year.1*1000,
		#r1$Yield.scaled.N2O.N.kg.N.Mg.yield.1,
		N_organic = ifelse(grepl("^Organic$", r1$Fertilizer.type), r1$Fertilizer.rate.kg.N.ha.1.year.1, 0),
		N_fertilizer =ifelse(grepl("^Inorganic$", r1$Fertilizer.type), r1$Fertilizer.rate.kg.N.ha.1.year.1, 0),
		type = r1$Fertilizer.type,
		OM_used = grepl("Organic", r1$Fertilizer.type),
		#irrigation_rate = r1$Irrigation.rate,
		irrigation_method = tolower(r1$Irrigation.type),
		land_prep_method = gsub("conventional tillage", "conventional", tolower(r1$Tillage)),
		cover_crop = ifelse(!is.na(r1$Cover.crop), ifelse(grepl("Legume", r1$Cover.crop), "legume",
		             ifelse(grepl("Non-legume", r1$Cover.crop), "unknown", "none")), NA) ,
		biochar_used = ifelse(!is.na(r1$Biochar), grepl("Yes", r1$Biochar), NA)  ,
		temp = r1$Mean.annual.temperature.C,
		rain = r1$Mean.annual.precipitation.mm,
		irrigated = grepl("Irrigated", r1$Water.regime) ,
		soil_SOC = r1$SOC.pct,
		soil_texture = tolower(r1$Soil.texture),
		soil_pH = r1$Soil.pH,
		crop_rotation = gsub(", ", ";", tolower(r1$Crop)),
		crop = tolower(trimws(sapply(strsplit(r1$Crop, ","), `[`, 1))),
		latitude = round(r1$Latitude, 3),
		longitude = round(r1$Longitude,3),
		country = trimws(r1$Site),
		reference = r1$Reference,
		trial_id  = as.character(match(r1$Site, c("Spain", "U.S.A.", "China", "Canada","Australia","Japan","Brazil", "Costa Rica", "Israel","Cameroon", "Indonesia ", "Ecuador", "Kenya","Philippines", "Burkina Faso", "Italy", "Germany", "Niger" , "U.K.", "New Zealand")))
		
	)
	
	
	### drop rows with "Inorganic+Organic" fertilizer type which there is rate of Inorganic and organic fertilizer used
	
	d <- d[!grepl("Inorganic\\+Organic", d$type),]
	d$type <- NULL
	
	## Not sure 
	#cols <- c("land_prep_method", "cover_crop")
	#d[cols] <- lapply(d[cols], function(col) {
	  #col[is.na(col)] <- "none"
	  #col
	#})
	#d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
	
	#### Fixing crop names 
	
	P <-  carobiner::fix_name(d$crop)
	P <- gsub("bare fallow", "none", P)
	P <- gsub("cacao", "cocoa", P)
	P <- gsub("carrots", "carrot", P)
	P <- gsub("^choy$", "pak choi", P)
	P <- gsub("choy sum", "pak choi", P)
	P <- gsub("garland chrysanthemum", "tonghao", P)
	d$crop <- P
	
	P <-  carobiner::fix_name(d$crop_rotation)
	P <- gsub("bare fallow", "none", P)
	P <- gsub("cacao", "cocoa", P)
	P <- gsub("carrots", "carrot", P)
	P <- gsub("chili", "chili pepper", P)
	P <- gsub("capsicum", "chili pepper", P)
	P <- gsub("choy sum", "pak choi", P)
	P <- gsub("bok choy", "pak choi", P)
	P <- gsub("tung choy", "pak choi", P)
	P <- gsub("baby bok choy", "pak choi", P)
	P <- gsub("^choy", "pak choi", P)
	P <- gsub("coriander herb", "cilantro", P)
	P <- gsub("garland chrysanthemum", "tonghao", P)
	P <- gsub("glory", "water spinach", P)
	P <- gsub("green beans", "green bean", P)
	P <- gsub("onions", "onion", P)
	P <- gsub("pumpkin,", "pumpkin", P)
	P <- gsub("vegetable rape", "vegetable", P)
	P <- gsub("baby pak choi", "pak choi", P)
	P <- gsub(";;", ";", P)
	d$crop_rotation <- P
	
	d$country <- gsub("U.S.A.", "United States", d$country)
	d$country <- gsub("U.K.", "United Kingdom", d$country)
	d$country[grepl("Indonesia", d$country)] <- "Indonesia"
	
	d$is_survey <- FALSE
	d$on_farm <- NA
	d$yield_moisture <- NA
	d$yield_isfresh <- TRUE
	d$yield_part <- "none"
	d$geo_from_source <- TRUE
	d$planting_date <- NA
	d$harvest_date <- NA
	
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)  
	
	# Set the longitude and latitude coordinates that are in conflict to NA, as there is no location information available to correct them.
	x <- which((d$longitude %in% c(119.633, 12.683, 119.767, 119.167, 139.567)) & (d$latitude %in% c(49.567, 22.683, -31.233, 48.500, 34.8)))
	d$longitude[x] <- NA
	d$latitude[x] <- NA
	
	## drop duplicate rows (3 rows)
	d <- unique(d)

	carobiner::write_files(path, meta, d)
}


