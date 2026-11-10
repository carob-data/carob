# R script for "carob"
# license: GPL (>=3)

#ISSUES
#Coordinate typos in ET_SoilLabels_wGPS_AEZ.csv, corrected against neighbouring plots in the same kebele

carob_script <- function(path) {
  
"Improving Measurements of Agricultural Productivity through Methodological validation and Research (LSMS-ISA)"
  
	uri <- "doi:10.34725/DVN/28457"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "ICRAF",
		publication= "doi:10.5194/soil-2020-69",
		project="LSMS-ISA",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_effort = NA,
		carob_date="2024-06-11"
	)
  
	f1 <- ff[basename(ff) == "CN_Texture_3611.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "ET_SoilLabels_wGPS_AEZ.csv"]
	r2 <- read.csv(f2)
  
	# coordinate typos corrected against neighbouring plots in the same kebele
	r2$N_Degree[grepl("^206\\.", r2$Code)] <- 5        # was 3 (Kenya); plots 201-212 are 5deg 07'
	r2$E_Minute[grepl("^410\\.", r2$Code)] <- 1.044    # was 41.044 (Kenya); plots 401-412 are 39deg 1'
	r2$E_Degree[grepl("^203\\.", r2$Code)] <- 37       # was 39deg 71.312' (invalid minutes); plots 201-212 are 37deg 10'
	r2$E_Minute[grepl("^203\\.", r2$Code)] <- 10.312
	r2$E_Degree[is.na(r2$E_Degree)] <- 37              # 7409.4.1 (E. Wellega); sibling plot 7409.7.1 is 37deg 1.254'
	
	r2$latitude <- r2$N_Degree + r2$N_Minute/60
	r2$longitude <- r2$E_Degree + r2$E_Minute/60
	
	r <- merge(r1, r2, by="Code")
  # Replace longitude with NA
  # carobiner::geocode("Ethiopia", "Borena")
  
	#r$longitude[is.na(r$longitude)] <- 37.9743 + (1.108/60)
	d <- data.frame(
		country = r$Country,
		location = r$Site,
		longitude = r$longitude,
		latitude = r$latitude,
		geo_from_source= TRUE,
		#trial_id = r$SSN,
		##depth_top = r$Depth_top, has a few errors
		##depth_bottom = r$Depth_bottom,
		soil_C = r$Total.Carbon,
		soil_SOC = r$AcidCarbon,
		soil_N = r$Total.Nitrogen,
		soil_clay = r$Clay,
		soil_silt = r$Silt,
		soil_sand = r$Sand
	)
	
	depth <- do.call(rbind, strsplit(gsub("cm", "", r$Depth.range), "-"))
	depth_top <- as.numeric(depth[,1])
	depth_bottom <- as.numeric(depth[,2])

	d <- unique(d)
	carobiner::write_files(path, meta, d)
}

