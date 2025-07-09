# R script for "carob"

carob_script <- function(path) {

"The authors acknowledge and thank the South Africa Agricultural Research Council (ARC) for making the original data available for this study. ARC data in this repository can be used to replicate a forthcoming study on wheat yields and climate change in South Africa. Should the data be used beyond replication, ARC must be acknowledged."

	uri <- "doi:10.7910/DVN/8Y6Q7F"
	group <- "varieties_wheat"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project=NA,
		publication="doi:10.1038/s41467-020-18317-8",
		data_organization = "ARSU;KSU;UAR;SARC",
   		data_type="experiment", 
		response_vars = "yield",
		treatment_vars = "variety",
		carob_contributor="Blessing Dzuda",
		carob_date="2024-01-24"
	)

	f <- ff[basename(ff) == "RegressionDataFinal.dta"]
	r <- haven::read_dta(f)

	d <- data.frame(
		location=r$Site, 
		adm1=r$Province, 
		variety=r$Cultivar, 
		variety_release_year=as.integer(r$Release_Year),
		rep=as.integer(r$Replicate), 
		planting_date=r$plant_date, 
		flowering_date=r$Flower_Date, 
		harvest_date=r$Harvest_date,
		yield = r$Yield*1000, 
		rain=r$prec,
		crop="wheat", 
		yield_part="grain", 
		country="South Africa"
	)
	d$trial_id <- as.character(as.integer(as.factor(paste(r$Site, r$year))))
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

## Location
	#g <-unique(d[,c("country","location")])
	#g1 <- carobiner::geocode(country = g$country, location = g$location, service = "nominatim")
	#dput(g1)

	gg <- data.frame(
		location=c("LANGGEWENS", "RIEBEEKWES", "TYGERHOEK", 
			"ALPHA","ARLINGTON", "BETHLEHEM", "BULTFONTEIN", "CLARENS", "EXCELSIOR", 
            "HOPEFIELD", "KLIPDALE", "MALMESBURY", "MOORREESBURG", 
            "NAPIER", "PANORAMA", "PHILADELPHIA", "PIKETBERG", "POOLS", 
            "PORTERVILLE", "PROTEM", "REITZ", "SENEKAL", "WESSELSBRON"),
        longitude=c(18.7035, 18.8687, 19.90131, 
			31.0052, 27.8531, 28.3089, 26.7755, 28.4194, 
	        27.0611, 18.3498, 19.9658, 18.7231, 18.6564, 19.8919, 
	        18.5738, 18.581, 18.7575, 31.2203, 18.9957, 28.5669, 
	        28.4275, 27.6326, 26.3647),
		latitude=c(-33.277, -33.33947, -34.14854, 
			-27.7555, -28.0286, -28.2308, -29.1681, -28.5137, -28.9414, 
			-33.0634, -34.305, -33.4582, -33.1553, -34.4715, -33.8795, 
	        -33.6666, -32.9042, -29.515, -33.0141, -25.7003, 
	        -27.8006, -28.3288, -27.8539),
		geo_from_source = FALSE
	)
			
	d <- merge(d, gg, by="location", all.x=TRUE)
	d$location <- carobiner::fix_name(d$location, "title")
	d$variety <- tolower(d$variety)

	d$planting_date <- as.character(d$planting_date)
	d$harvest_date  <- as.character(d$harvest_date)
	d$flowering_date <- as.character(d$flowering_date)

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


