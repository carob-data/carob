# R script for "carob"

## these data are derived from CIMMYT wheat trials already processed.

carob_script <- function(path) {
  
"Genotype x environment (G x E) interaction can be studied through multienvironment trials used to select wheat (Triticum aestivum L.) lines. We used spring wheat yield data from 136 international environments to evaluate the predictive ability (PA) of different models in diverse environments by modeling G X E using the pedigree-derived additive relationship matrix (A matrix)."
  
	uri <- "hdl:11529/10831"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		publication= "doi:10.2135/cropsci2016.06.0558",
		data_organization = "CIMMYT", 
		carob_contributor="Cedric Ngakou", 
		data_type="on-station experiment",
		project=NA,
		response_vars = "yield",
		treatment_vars = "variety",
		carob_date="2023-10-19"		
	)
 
	f1 <- ff[basename(ff) == "1SATYN.csv"] 
	f2 <- ff[basename(ff) == "1WYCYT.csv"]
	f3 <- ff[basename(ff) == "2SATYN.csv"]
	f4 <- ff[basename(ff) == "2WYCYT.csv"]
	f5 <- ff[basename(ff) == "3SATYN.csv"]
	f6 <- ff[basename(ff) == "3WYCYT.csv"]
	f7 <- ff[basename(ff) == "4SATYN.csv"]
	
	# read the dataset
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)

	r3$SubBlock <- NULL
	r6$SubBlock <- NULL
	
	# process file(s)
	r <- rbind(r1, r2, r3, r4, r5, r6)
	
## RH: we need to know the names/codes for the varieties. 
## data has not much use without that. 	
	d <- data.frame(
		trial_id = r$code,
		rep = r$Rep,
		yield = as.numeric(r$YLD)*1000, 
		variety = r$Entry
	)
 

	location <- data.frame(
		trial_id = c("BGLD D", "BGLD J", "BGLD R", "China L", "Croatia O", "Egypt A", 
				"Egypt G", "Egypt N", "Egypt S", "Egypt Si", "Egypt SK", "India D", "India H", 
				"India I", "India K", "India L", "India U", "India V", 
				"Iran D", "Iran DZ", "Iran K", "Iran Z", "Mex BC", "Mex CM", "Mex JAL", "Mex SIN", 
				"Mex SON", "Mex-Baj", "Nepal B", "Pak B", "Pak F", "Pak I", "Pak P", "Romania I", "SA B"),
 
		location = c("Dinajpur", "Joydebpur", "Rajshahi", "Laomancheng", "Osijek", "Assiut", "Gemmeiza", "Nubaria", "Sohag", "Sids", "Sakha", "Delhi", "Dharwad", "Indore", "Karna", "Ludhiana", "Ugar Khurd", "Varanasi", "Darab-hassan-abad", "Dezfoul", "Kara", "Zargan", "Mexicali Baja California", "Cd Obregon, Sonora", "Tepatitlan Jalisco", "Valle del Fuerte, Sinaloa", "Valle del Yaqui", "Bajio", "Bhairahawa", "Bahawalpur", "Faisalabad", "Islamabad", "Pirsabak", "Fundulea", "Bethlehem"),
 
		longitude = c(88.63, 90.42, 89.037, NA, 18.69, 31.20, NA, 30.07, 32.09, 30.93, 30.94, 77.22, NA, 75.86, 77.47, 75.85, 74.82, 83.007, 54.54, 48.40, 51.38, 52.72, -115.47, -99.19, -102.75, -115.44, -98.75, -99.21, 83.45, 71.66, 73.09, 73.06, NA, 26.50, 28.30),
 
		latitude = c(25.62, 23.99, 24.62, NA, 45.55, 27.17, NA, 30.66, 26.76, 28.90, 31.09, 28.65, NA, 22.72, 18.70, 30.90, 16.67, 25.33, 28.75, 32.37, 35.80, 29.77, 32.64, 19.38, 20.81, 32.63, 20.10, 19.38, 27.51, 29.39, 31.42, 33.69, NA, 44.46, -28.23),
 
		country = c("Bangladesh", "Bangladesh", "Bangladesh", "China", "Croatia", "Egypt", "Egypt", "Egypt", "Egypt", "Egypt", "Egypt", "India", "India", "India", "India", "India", "India", "India", "Iran", "Iran", "Iran", "Iran", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Nepal", "Pakistan", "Pakistan", "Pakistan", "Pakistan", "Romania", "South Africa")
	)
  
	# Add location and country in the dataset 
	d <- merge(d, location, by="trial_id", all.x=TRUE)
	d$geo_from_source <- FALSE
	
	#fixes
	d$longitude[d$location=="Dharwad"] <- 75.0066516
	d$latitude[d$location=="Dharwad"] <- 15.4540505
	d$longitude[d$location=="Pirsabak"] <- 72.0393338
	d$latitude[d$location=="Pirsabak"] <- 	34.0258704
	d$longitude[d$location=="Gemmeiza"] <- 24.2037306
	d$latitude[d$location=="Gemmeiza"] <- 	12.6666536 
	d$country[d$location=="Gemmeiza"] <- "Sudan" #
	d$location[d$location=="Pirsabak"] <- "Pir Sabak" 
	
	d$trial_id <- d$code
	
	d$crop <- "wheat"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- FALSE

	d$trial_id <- "1"
	d$yield_moisture <- K_fertilizer <- N_fertilizer <- P_fertilizer <- NA
	d$planting_date <- NA
	
	#data type
	d$yield_part <- "grain"
	carobiner::write_files(meta, d, path=path)
}

