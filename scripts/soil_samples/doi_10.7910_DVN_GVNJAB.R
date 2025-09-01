# R script for "carob"


carob_script <- function(path) {
  
"Physical topsoil properties in Murugusi, Western Kenya"
  
	uri <- "doi:10.7910/DVN/GVNJAB"
	group <- "soil_samples"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		project= NA,
		publication= NA,
		data_organization = "CIAT",
		data_type = "survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Andrew Sila", 
		carob_date="2023-09-28"
	)
  
  # No need to read the table with MIR data
	f <- ff[basename(ff) == "02 soil_samples-data.xlsx"]
	r <- data.frame(carobiner::read.excel(f))

	d <- carobiner::change_names(r[, -c(2:3)], c("Lab_id", "bd_020","TC_020", "clay_020", "sand_020"), 
			c('trial_id', 'soil_bd','soil_total_carbon', 'soil_clay', 'soil_sand'))
					
	v <- terra::vect(as.matrix(r[, c("POINT_X_utm36N", "POINT_Y_utm36N")]), 
							crs="+proj=utm +zone=36")
	v <- terra::project(v, "+proj=longlat") |> terra::crds()
	# Extract latitude and longitude values
	d$latitude <- v[,2]
	d$longitude <- v[,1]
	d$geo_from_source <- TRUE
	d$country <- "Kenya"
	
	r$depth_top <- 0
	r$depth_bottom <- 20
	
	carobiner::write_files(meta, d, path=path)
}



