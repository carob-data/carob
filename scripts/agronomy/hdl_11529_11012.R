# R script for "carob"

carob_script <- function(path) {
  
"Performance trials (N=52) in two zones (West Shewa and Jimma) in Ethiopia. Trials comprise four nutrient management treatments, namely control with zero fertilizer ; and three fertilizer recommendations to achieve the same  target yield based on regional fertilizer recommendation, a Nutrient Expert (IPNI software) based recommendation and a soil-test NE based recommendation. Trials were conducted on-farm with four plots per farm. Observations  include biomass and grain yields, as well as pre-sowing pH, nitrogen and phosphorus levels. Some N & K data are missing."

	uri <- "hdl:11529/11012"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project="TAMASA",
		publication=NA,
		data_organization = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Mary Njogu",
		carob_date="2023-02-20",
		modified_by = "Eduardo Garcia Bendito",
		last_modified = "2024-03-05",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield"
	)

  
	f <- ff[basename(ff) == "TAMASA_ET_PT_2016F.xlsx"]
	r <- carobiner::read.excel(f, sheet = 5, fix=TRUE)
	tr <- carobiner::read.excel(f, sheet = 6, fix=TRUE)[,c(1,4,8:11)]
	
	# process treatment levels
	vars <- c('Control', 'NE.Recomm', 'Regional.Recom', 'Soil.Test.Based.Recom')
	dtr <- reshape(tr, direction = "long", varying = list(vars))[,1:4]
	colnames(dtr) <- c("location", "soil_P_available", "treatment", "fertilizer")
	rownames(dtr) <- NULL

	dtr$treatment <- c("Control", "NE Recommendation", "Regional Recommendation", "Soil Test Based Recommendation")[dtr$treatment]
	
	dtr <- cbind(dtr[1:3], do.call(rbind, strsplit(dtr$fertilizer, "-", fixed = TRUE)))
	colnames(dtr) <- c("location", "soil_P_available", "treatment", "N_fertilizer", "P_fertilizer", "K_fertilizer")
	dtr$location <- carobiner::fix_name(dtr$location, case = "title")
	dtr$N_fertilizer <- as.numeric(dtr$N_fertilizer)
	dtr$P_fertilizer <- as.numeric(dtr$P_fertilizer)
	dtr$K_fertilizer <- as.numeric(dtr$K_fertilizer)

  
	d <- data.frame( 
			trial_id = as.character(rep(1:(nrow(r)/4), each = 4)),
	        country = "Ethiopia", 
			adm1 = r$Region, adm2 = r$Zone, adm3 = r$Districts,
	        location = r$Location, elevation = r$Altitude, 
	        treatment = r$Treatments,
	        crop = "maize",  variety_type = "hybrid", yield_part = "grain",
	        soil_pH = r$pH
		)
		
	d$soil_pH[d$soil_pH == "."] <- NA
	d$soil_pH <- as.numeric(d$soil_pH)
	
	d$yield <- r$Kernel.yield.kg.ha
	d$dmy_total <- (as.numeric(r$Total.Biomass.Weight.kg)/(18/10000))*0.875
	# d$fwy_residue <- as.numeric(r$Field.Weight.of.subsample.3.stalk.kg)/(18/10000)

	r$TN.pct[r$TN.pct == "."] <- NA
	d$soil_N <- (as.numeric(r$TN.pct) * 10000)
	d$soil_P_available <- (as.numeric(r$Av.P.ppm))
		
	d$location <- carobiner::fix_name(d$location, case = "title")

	svars <- c("elevation", "soil_pH", "soil_N", "soil_P_available")
	x <- d[, c("trial_id", svars)]
	x <- x[!is.na(x$elevation), ]
	i <- match(d$trial_id, x$trial_id)
	d[, svars] <- x[i, svars]

	d <- merge(d, dtr, by = c("location", "treatment", "soil_P_available"), all.x = TRUE)
	
	## see below for coordinates source
	geo <- data.frame(  
			adm2 = c("West Showa", "Jimma", "Jimma", "West Showa", "Jimma", 
				"Jimma", "West Showa", "West Showa", "West Showa", "West Showa", 
				"Jimma", "West Showa", "Jimma", "Jimma", "Jimma", "Jimma", "West Showa", 
				"West Showa", "West Showa", "West Showa", "West Showa", "West Showa", 
				"West Showa", "Jimma", "West Showa", "Jimma", "Jimma"),
	        adm3 = c("Ilu galen", "Kersa", "Tiro Afeta", "Bako Tibe", "Sekoru", "Sekoru", "Bako Tibe", 
				"Bako Tibe", "Bako Tibe", "Gobu sayo", "Kersa", "Bako Tibe", 
				"Tiro Afeta", "Tiro Afeta", "Sekoru", "Kersa", "Bako Tibe", "Bako Tibe", 
				"Bako Tibe", "Gobu sayo", "Bako Tibe", "Gobu sayo", "Bako Tibe", 
				"Kersa", "Bako Tibe", "Omo Nada", "Sekoru"),
	        location = c("Ale wara ilu",
				"Babo", "Babo", "Bachera Oda Gibe", "Bore", "Chala", "Dambi Dima",
				"Dambi Gobu", "Gajo Kuyi", "Gambela Tare", "Ganda Girma", "Gudina Walkite",
				"Hajelo", "Hejelo", "Liben", "Merewa", "Oda Gibe", "Oda Haro", 
				"Oda Korma", "Ongobo Bakanisa", "Saden Kite", "Sombo Kejo", "Tarkanfata Gibe",
				"Tikurbalto", "Tulu Sangota", "Waktola", "Walmara"),
	        longitude = c(37.19, 35.1962, 35.1962, 37.156, 38.6222, 37.4179, 38.9667, 36.603, 37.032,
				34.57, 37.073, 37.218, 37.329, 37.329, 40.7091, 36.9167, 37.126, 35.522,
				37.156, 36.897, 37.156, 37.031, 37.517, 37.073, 37.156, 37.194, 38.5769),
	        latitude = c(9.13, 9.4744, 9.4744, 9.0662, 6.3596, 7.9257, 3.5667, 10.15, 9.175, 8.248,
				7.748, 9.021, 7.916, 7.916, 4.5484, 7.6833, 9.05, 8.543, 9.0662, 9.086, 9.0662, 
				9.779, 9.753, 7.748, 9.0662, 7.726, 9.0572),
			geo_from_source = FALSE
		)

	geo$location <- carobiner::fix_name(geo$location, case = "title")
	
	d <- merge(d, geo, by = c("adm2", "adm3", "location"), all.x = TRUE)
	
	# metadata specifies Start: 2016-05-01 ; End: 2016-12-01
	# but that cannot be the planting and harvest dates for all sites
	d$planting_date <- "2016"
	d$harvest_date <- "2016"
	d$treatment <- tolower(d$treatment)

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


# # Geocoding
# h <- unique(d[,c("adm2", "adm3", "location")])
# h1 <- carobiner::geocode("Ethiopia", location = unique(h$location))
# h1$df$lon[h1$df$location == "Dambi Gobu"] <- 36.603 # https://maps.app.goo.gl/bnZNwBHM1mFQjg4E8
# h1$df$lat[h1$df$location == "Dambi Gobu"] <- 10.150 # https://maps.app.goo.gl/bnZNwBHM1mFQjg4E8
# h1$df$lon[h1$df$location == "Gajo Kuyi"] <- 37.032 # https://maps.app.goo.gl/JdbaNsGfPTn4yqTA7
# h1$df$lat[h1$df$location == "Gajo Kuyi"] <- 9.175 # https://maps.app.goo.gl/JdbaNsGfPTn4yqTA7
# h1$df$lon[h1$df$location == "Gambela Tare"] <- 34.570 # https://maps.app.goo.gl/wSeHyDFAgeniKV8h6
# h1$df$lat[h1$df$location == "Gambela Tare"] <- 8.248 # https://maps.app.goo.gl/wSeHyDFAgeniKV8h6
# h1$df$lon[h1$df$location == "Gudina Walkite"] <- 37.218 # https://maps.app.goo.gl/okrr4vUVazjjjeNV8
# h1$df$lat[h1$df$location == "Gudina Walkite"] <- 9.021 # https://maps.app.goo.gl/okrr4vUVazjjjeNV8
# h1$df$lon[h1$df$location == "Gudina Walkite"] <- 37.218 # https://maps.app.goo.gl/okrr4vUVazjjjeNV8
# h1$df$lat[h1$df$location == "Gudina Walkite"] <- 9.021 # https://maps.app.goo.gl/okrr4vUVazjjjeNV8
# h1$df$lon[h1$df$location == "Oda Gibe"] <- 37.126 # https://www.google.com/maps/place/Oda+Gibe/@9.0506397,37.126713,14.26z
# h1$df$lat[h1$df$location == "Oda Gibe"] <- 9.050 # https://www.google.com/maps/place/Oda+Gibe/@9.0506397,37.126713,14.26z
# h1$df$lon[h1$df$location == "Oda Haro"] <- 35.522 # https://maps.app.goo.gl/W67nSSnLZDeA8rFV7
# h1$df$lat[h1$df$location == "Oda Haro"] <- 8.543 # https://maps.app.goo.gl/W67nSSnLZDeA8rFV7
# h1$df$lon[h1$df$location == "Ongobo Bakanisa"] <- 36.897 # https://maps.app.goo.gl/9fH2B3hCghGCp3cE9
# h1$df$lat[h1$df$location == "Ongobo Bakanisa"] <- 9.086 # https://maps.app.goo.gl/9fH2B3hCghGCp3cE9
# h1$df$lon[h1$df$location == "Sombo Kejo"] <- 37.031 # https://maps.app.goo.gl/Zuk8hj339tSxX8nJ6
# h1$df$lat[h1$df$location == "Sombo Kejo"] <- 9.779 # https://maps.app.goo.gl/Zuk8hj339tSxX8nJ6
# h1$df$lon[h1$df$location == "Tarkanfata Gibe"] <- 37.517 # https://maps.app.goo.gl/b46n2LvwbncCVFdDA
# h1$df$lat[h1$df$location == "Tarkanfata Gibe"] <- 9.753 # https://maps.app.goo.gl/b46n2LvwbncCVFdDA
# h1$df$lon[h1$df$location == "Waktola"] <- 37.194 # https://maps.app.goo.gl/uv8k7479pavNWfXr8
# h1$df$lat[h1$df$location == "Waktola"] <- 7.726 # https://maps.app.goo.gl/uv8k7479pavNWfXr8
# h2 <- carobiner::geocode("Ethiopia", location = unique(h$adm3))
# colnames(h2$df) <- c("country", "adm3", "lon", "lat")
# h2$df$lon[h2$df$adm3 == "Kersa"] <- 37.073 # https://maps.app.goo.gl/qMChAtRzCXMSuq2f8
# h2$df$lat[h2$df$adm3 == "Kersa"] <- 7.748 # https://maps.app.goo.gl/qMChAtRzCXMSuq2f8
# h2$df$lon[h2$df$adm3 == "Tiro Afeta"] <- 37.329 # https://maps.app.goo.gl/dVsYaiuqfxRPxBAb8
# h2$df$lat[h2$df$adm3 == "Tiro Afeta"] <- 7.916 # https://maps.app.goo.gl/dVsYaiuqfxRPxBAb8
# h2$df$lon[h2$df$adm3 == "Omo Nada"] <- 37.242 # https://maps.app.goo.gl/CoL4LfWjyzxBSbz4A
# h2$df$lat[h2$df$adm3 == "Omo Nada"] <- 7.501 # https://maps.app.goo.gl/CoL4LfWjyzxBSbz4A
# h3 <- carobiner::geocode("Ethiopia", location = unique(h$adm2))
# colnames(h3$df) <- c("country", "adm2", "lon", "lat")
# h3$df$lon[h3$df$adm2 == "West Showa"] <- 37.19 # https://maps.app.goo.gl/XHsYDMABLYU1fuKj7
# h3$df$lat[h3$df$adm2 == "West Showa"] <- 9.13 # https://maps.app.goo.gl/XHsYDMABLYU1fuKj7 
# h4 <- merge(h, h3$df, by = "adm2", all.x = TRUE)
# h5 <- merge(h4, h2$df, by = "adm3", all.x = TRUE)
# h6 <- merge(h5, h1$df, by = "location", all.x = TRUE)
# h6$lon <- ifelse(is.na(h6$lon), h6$lon.y, h6$lon)
# h6$lat <- ifelse(is.na(h6$lat), h6$lat.y, h6$lat)
# h6$lon <- ifelse(is.na(h6$lon), h6$lon.x, h6$lon)
# h6$lat <- ifelse(is.na(h6$lat), h6$lat.x, h6$lat)
# h <- h6[,c("country", "adm2", "adm3", "location", "lon", "lat")]
# colnames(h) <- c("country", "adm2", "adm3", "location", "longitude", "latitude")
# dput(h)
