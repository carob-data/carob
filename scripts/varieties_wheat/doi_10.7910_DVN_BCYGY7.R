# R script for "carob"


carob_script <- function(path) {
  
"Wheat is the most widely grown cereal in Ethiopia next to tef and maize. The residue from wheat is mainly used as livestock feed. In addition to the grain yield, the quantity and quality of wheat straw produced is very important for smallholders and affect variety selection and adoption by farmers. Variations in food-feed traits of different bread wheat cultivars released for the highlands of Ethiopia were evaluated across different districts. This dataset provides grain yield, straw yield, and fodder traits of the straw for different wheat varieties in different districts over two growing seasons."

	uri <- "doi:10.7910/DVN/BCYGY7"
	group <- "varieties_wheat"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		project=NA,
		publication= NA,
		data_organization = "ILRI",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		carob_contributor="Fredy Chimire",
		carob_effort = NA,
		carob_date="2024-03-10"
	)
  
	f1 <- ff[basename(ff) == "001_data-wheat-yield-Ethiopia_Debrezeit_year1.csv"]
	f2 <- ff[basename(ff) == "002_data-wheat-yield-Ethiopia_Kulumsa_year1.csv"]
	f3 <- ff[basename(ff) == "003_data-wheat-yield-Ethiopia_multipleLocations_year2.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r3 <- r3[r3$Site!="", ]

	r12 <- rbind(r1, r2)	

	d1 <- data.frame(
		location=r12$Location, 
		variety=r12$Wheat.Variety, 
		fwy_residue=r12$Straw.Yield..ton.ha.*1000, 
		yield = r12$Grain.yield..ton.ha. * 1000,
		planting_date = "2016"  # from metadata but not sure
	)
	d3 <- data.frame(
		location=r3$Site, 
		variety=r3$Wheat.Variety, 
		fwy_residue=r3$Straw.yield..ton.ha. * 1000, 
		yield=r3$Grain.yield..ton.ha. * 1000,
		planting_date = 2017
	)

	d <- rbind(d1, d3) 
	d$trial_id <- as.character(as.integer(as.factor(paste(d$year, d$planting_date))))
	
	d$crop <- "wheat"
	d$planting_date
	d$country <- "Ethiopia"
	d$location <- trimws(d$location)
	d$is_survey <- FALSE
	d$yield_part <- "grain"
  
	coord <- data.frame(
		location = c("Debre Ziet", "Kulumsa", "Asasa", "Dawa Busa", "Bekoji"), 
		longitude = c(38.9978, 39.1603, 39.2012, 38.0116, 39.2539), 
		latitude = c(8.7657, 8.0199, 7.1076, 8.7771, 7.5267),
		geo_from_source = FALSE
	)
	d <- merge(d, coord, by="location", all.x=TRUE)

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$yield_moisture <- d$yield_isfresh <- NA
	d$irrigated <- NA
	d$on_farm <- NA
	
	carobiner::write_files(path, meta, d)
}

