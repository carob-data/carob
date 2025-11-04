# R script for "carob"


carob_script <- function(path) {

" The experiment was conducted in 2016 and 2017 in three regions in Oaxaca, Mexico. The design of the study was to evaluate fifteen combinations of tillage and weed management practices at each of the three sites, with each trial consisting of three blocks with a different type of tillage: zero tillage (ZT), minimum tillage (MT) and conventional tillage (CT). The first site is in the “Sitio Experimental Mixteca” research station in Santo Domingo Yanhuitlán, the Mixteca Region, located at 2195 m above sea level (masl), it has Vertisol soils, a temperate subhumid climate. The second site is in the town of San Felipe Zihualtepec in the municipality of San Juan Cotzocón, Papaloapan Region, located at 60 masl and has Luvisol soils, a hot humid climate. The third location was in the town of Ciénega de Zimatlán in the municipality with the same name, Valles Centrales Region. The original site was changed in 2017 to a nearby field, because the collaborating farmer did not want to continue the trial. Both fields are located at 1552 masl, have Vertisol soils, a hot semi-arid climate. Weed management treatments were conducted similarly in all tillage treatments and reflected common local practices and available herbicides or equipment, as well as weather, soil moisture and weed species and comprised combinations of the following: 1) MEC, mechanical control. Weeds were mechanically controlled after reaching 20 cm in height approximately 20-25 days after sowing (DAS), as per the common practice. Weeding was carried out with a hand hoe by 4 to 10 workers per hectare, depending on the quantity of weeds, or using a tractor-drawn cultivator. 2)PRE, pre-emergent herbicide. Only a pre-emergent herbicide with residual effect was applied before sowing. 3) POST, post-emergent herbicide. When weeds reached 5-10 cm in height and based on soil moisture conditions and the types of weeds present, a selective herbicide or direct contact herbicide was applied. 4) PRE+POST, integrated weed management. A pre-emergent herbicide was applied, followed by post-emergence control as necessary, using either selective herbicides or manual controls. 5) CONT, control: No weed management was practiced. The dataset contains the data on maize yield, weed density, weed species (broadleaf or narrowleaf) and weed biomass from the experiment. (2021-04-08)"

	uri <- "hdl:11529/10548568"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "land_prep_method;weeding_method", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-07-11"
	)

	f <- ff[basename(ff) == "PUB-DAT-WeedsOaxaca.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Yield data")
	r1$record_id <- as.integer(1:nrow(r1))

	r2 <- carobiner::read.excel(f, sheet = "Weed data", na=c("", "NA"))
	r2 <- unique(r2)

	r <- merge(r2, r1, by.x=c("Site", "Year", "Tillage", "Treatment", "Rep"), 
			by.y=c("Site", "Year", "Tillage", "Weed Management", "Rep"), all.x=TRUE)	

	d <- data.frame(
		record_id = r$record_id,
		location = r$Site,
		rep= as.integer(r$Rep),
		plant_height=r$`Plant height (m)` * 100,
		yield=r$`Yield (t/ha 14% moisture)`*1000,
		land_prep_method=r$Tillage,
		weeding_method=r$Treatment,
		weed_biomass=r$FW ,
		planting_date=as.character(r2$`Sowing date`),
		date = r$`Sampling Date`
	)

	
	#fixing names
	d$land_prep_method <- gsub("CT", "conventional", d$land_prep)
	d$land_prep_method <- gsub("ZT", "none", d$land_prep) # zero-tillage
	d$land_prep_method <- gsub("MT", "reduced tillage", d$land_prep) # minimal-tillage
	
	d$weeding_method <- gsub("CONT", "none", d$weeding_method) # control
	d$weeding_method <- gsub("MEC", "mechanical", d$weeding_method)
	d$weeding_method <- gsub("POST", "herbicide, post-emergence", d$weeding_method)
	d$weeding_method <- gsub("PRE", "herbicide, pre-emergence", d$weeding_method)
	d$weeding_method <- gsub("PRE+POST", "herbicide, pre- and post-emergence", d$weeding_method)

	d$trial_id <- "1"

	geo <- data.frame(
		location = c("Mixteca", "Papaloapan", "Valles Centrales"), 
		site = c("Sitio Experimental Mixteca", "San Juan Cotzocón", "Ciénega de Zimatlán"),
		elevation = c(2195, 60, 1552),
		soil_type = c("vertisol", "luvisol", "vertisol"),
		longitude = c(-96.8578, -96.094722199, -96.48651), 
		latitude = c(16.9294, 18.1591666, 16.92554),
	  geo_from_source = TRUE
	)

		d <- merge(d, geo, by="location", all.x=TRUE)


	d$on_farm <- TRUE
	d$country <- "Mexico"
	d$adm1 <- "Oaxaca"
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d <- unique(d)
	
	x <- unique(d[, c("record_id", "date", "weed_biomass")])
	x <- na.omit(x)
  x$date <- as.character(x$date)
    
	d$date <- d$weed_biomass <- NULL
	d <- unique(d)
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d, long=x)
}

