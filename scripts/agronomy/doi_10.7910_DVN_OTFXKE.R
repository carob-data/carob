# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. Missing data(NA) from raw dataset in fertilizers,yield,longitude,latitude.
carob_script <- function(path) {


"This dataset contains soil health data set from multiple agronomic trials conducted in different regions in Kenya. It captures information on crop yield responses to cropping systems, field management practices, fertilizer regimes, manure, crop residues, and lime, across different agro-ecological conditions, seasons, and years. The data is suitable for soil health related analysis, yield modelling, and evaluation of sustainable land management practices."


	uri <- "doi:10.7910/DVN/OTFXKE"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-05-31",
		notes = NA, 
		design = NA
	)
	
	f <- ff[basename(ff) == "02a. Kenya soil health data.csv"]
	r <- read.csv(f, sep = ";")
	r[r == ""] <- NA
	r <- as.data.frame(lapply(r, function(x) {
	  if (is.character(x)) gsub(",", ".", x) else x}))
	
	
	d <- data.frame(
		country = "Kenya",
		adm3=r$Area,
		location=r$location,
		planting_date=r$planting_date,
		harvest_date=r$harvest_date,
		season=r$Season,
		rep=as.integer(r$rep),
		crop=tolower(r$crop),
		variety=r$variety,
		intercrops=r$intercrops,
		previous_crop=r$previous_crop,
		crop_rotation=r$crop_rotation,
		yield=r$Grain_yield.kg.ha.,
		fertilizer_type=r$fertilizer_type,
		N_fertilizer=r$N_fertilizer,
		P_fertilizer=r$P_fertilizer,
		K_fertilizer=r$K_fertilizer,
		inoculated=r$inoculated,
		irrigated=r$irrigated,
    land_prep_method=r$land_prep_method,
		mulch=r$mulch,
		mulch_type=r$mulch_type,
		soil_texture=r$soil_texture,
		soil_pH=r$soil_pH)
		

	#cleaning location names
	location_lookup <- c(
	  "MachangÂ’a"= "Machang’a",
	  "Kand"= "Kandara",
	  "Sidi"= "Sidindi",
	  "Nai Farm"= "Naivasha",
	  "Bondo (Nyanza)"= "Bondo",
	  "Kakamega (Western)"= "Kakamega",
	  "Migori (Nyanza)"= "Migori",
	  "Teso (Western)"= "Teso",
	  "Bungoma (Western)"= "Bungoma",
	  "Butere (Western)"= "Butere",
	  "Kakamega South (Western)"= "Kakamega South",
	  "Bungoma (Western Kenya)"= "Bungoma",
	  "Butula (Western)"= "Butula",
	  "Rarieda (Nyanza)"= "Rarieda",
	  "Kisumu West (Nyanza)"= "Kisumu West",
	  "Gem (Nyanza)"= "Gem",
	  "Teso North (Western)"= "Teso North",
	  "Bungoma (Bungoma)"= "Bungoma",
	  "Webuye (bungoma)"= "Webuye",
	  "Migori (South Nyanza)"= "Migori",
	  "Mumias (Western)"= "Mumias",
	  "Migori (Western)"= "Migori",
	  "Migori (Rangenya)"= "Migori",
	  "Migori (Uriri)"= "Uriri",
	  "Mumias (Isongo)"= "Mumias",
	  "KALRO. Embu"= "Embu",
	  "Kari"= "Kabete",       
	  "Kyeni-Mweru"= "Kyeni",
	  "Mworoga-Mariani"= "Mariani",
	  "Madeya "= "Madeya"
	)
	
	d$location <- trimws(d$location)
	d$location <- ifelse(d$location %in% names(location_lookup),
	                           location_lookup[d$location],d$location)
	
	loc=data.frame(
	  location= c("Machang’a","Nyabeda","Kirege","Kigogo","Kandara","Chuka","Embu",
	              "Kyeni","Mariani","Kanduyi","Ugunja","Kabete","Asembo","Sidindi",
	              "Bondo","Kakamega","Migori","Teso","Bungoma","Butere","Kakamega South",
	              "Butula","Rarieda","Kisumu West","Gem","Teso North","Webuye","Mumias","Uriri",
	              "Kiboko","Nyamninia","Cox","Davidson","Hulme","Kiminini","Leys","Naivasha",
	              "Russell","Sabwani","Strong","Menengai","Nandi Hills","Tinderet","Chesumei","Madeya"),
	  longitude=c(37.6586, 34.4032, 37.6227, 34.9286, 37.0021, 37.6546, 37.4596, 37.5828, 37.7557, 34.5750, 
	              34.2966, 36.7129, 34.3838, 34.3894, 34.2742, 34.7520, 34.4753, 34.1114, 34.5584, 34.4919, 
	              34.7520, 34.3352, 34.3532, 34.6635, 34.6082, 34.1114, 34.7796, 34.4877, 34.4420, 37.7231, 
	              34.5182, 34.8900, 35.0000, 34.9500, 34.9259, 34.9200, 36.4330, 35.1000, 34.8855, 34.9500, 
	              36.0953, 35.1764, 35.3466, 35.1047, 34.3986 ),
	  latitude=c(-0.7777, 0.1272, -0.3379, 0.7895, -0.8964, -0.3229, -0.5388, -0.4036, -0.3187, 0.5421,  # Kanduyi
	             0.1811, -1.2188, -0.1794, 0.1542, -0.0998, 0.2829, -1.0707, 0.4607, 0.5695, 0.2198, 0.2000,  # Kakamega South (approx)
	             0.3416, -0.1389, -0.0768, -0.6122, 0.4607, 0.5992, 0.3332, -0.9522, -2.2103, 0.1106, 1.0200,  # Cox (approx)*
	             0.9800, 1.0000, 0.8935, 0.9500, -0.7194, 0.5000, 1.0597, 0.9700, -0.2261, 0.1031, -0.0304,  # Tinderet
	             0.2035, 0.1444))
	
	d <- merge(d,loc,by="location", all.x = TRUE)
	
#cleaning the planting date column
	clean_date <- function(x) {
	  x[x == ""] <- NA
	  
	  year_only <- grepl("^\\d{4}$", x) & !is.na(x)
	  x[year_only] <- paste0("01/01/", x[year_only])
	  
	  year_month <- grepl("^\\d{4}-\\d{2}$", x) & !is.na(x)
	  x[year_month] <- paste0("01/", sub("\\d{4}-(\\d{2})", "\\1", x[year_month]), 
	                          "/", sub("(\\d{4})-\\d{2}", "\\1", x[year_month]))
	  
	  as.Date(x, format = "%d/%m/%Y")}
	
	d$planting_date <- clean_date(d$planting_date)
	d$harvest_date <-clean_date(d$harvest_date)
	d$planting_date <- as.character(d$planting_date)
	d$harvest_date <- as.character(d$harvest_date)
	
	
	d$trial_id <- paste0(d$location,d$planting_date, sep="_")
	d$lime <- r$lime.t.ha.
	d$lime[d$lime==500] <- 0.5
	d$lime <- as.numeric(d$lime) * 1000
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- TRUE
	d$yield_isfresh <- TRUE
	d$yield_moisture <- as.numeric(NA)
	d$season <- ifelse(d$season=="SR","short rains","long rains")
	d$crop <- gsub("vegetables","vegetable",d$crop)
	d$crop_rotation[d$crop_rotation == "s"] <- NA
	d$yield <- gsub(",",".",d$yield)
	d$adm3 <- trimws(d$adm3)
	d$soil_pH <- as.numeric(d$soil_pH)
	d$N_fertilizer <- as.numeric(d$N_fertilizer)
	d$P_fertilizer <- as.numeric(d$P_fertilizer)
	d$K_fertilizer <- as.numeric(d$K_fertilizer)
	d$yield <- as.numeric(d$yield)
	
	crop_yield_part <- c(
	  "common bean"   = "seed",
	  "soybean"       = "seed",
	  "maize"         = "grain",
	  "wheat"         = "grain",
	  "sweetpotato"   = "tubers",
	  "sorghum"       = "grain",
	  "groundnut"     = "seed",
	  "cassava"       = "roots",
	  "millet"        = "grain",
	  "lablab"        = "seed",
	  "cowpea"        = "seed",
	  "finger millet" = "grain",
	  "sugarcane"     = "stems",
	  "tobacco"       = "leaves",
	  "kale"          = "leaves",
	  "potato"        = "tubers",
	  "vegetables"    = "leaves"
	)
	
	d$yield_part <- crop_yield_part[d$crop]
	d$yield_part[is.na(d$yield_part)] <- "none"
  
	d <- unique(d)
	carobiner::write_files(path, meta, d)
}

