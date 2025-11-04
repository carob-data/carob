# R script for "carob"


carob_script <- function(path) {
   
" Agronomic data for a trial in which newly released varieties of groundnuts were intercropped with pigeonpea. The data will help to determine the suitability of planting the new varieties with different pigeonpea varieties. The pigeonpea varieties includes long, medium and short duration."
   
   uri <- "doi:10.21421/D2/MHOUWW"
	group <- "agronomy" 
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
		data_organization = "ICRISAT", 
		publication =NA, 
		project = NA, 
		data_type = "on-station experiment", 
		response_vars = "yield;dmy_total;fwy_total",
		treatment_vars = "variety;intercrops", 
		carob_contributor = "Cedric Ngakou", 
		carob_date = "2024-08-27"
	)

	f <- ff[grep(".xlsx", basename(ff))]
	
		r <- carobiner::read.excel(f,  sheet = "Cropping patterns")
		
		d <- data.frame(
			location= r$Site,
			rep= as.integer(r$Replication),
			plot_nr= r$`Plot No`,
			variety= r$`Groundnut variety`,
			plot_area= r$`plot size`,
			plant_height= r$`Plant height`,
			disease_incidence= r$DI,
			disease_severity= as.character(r$DS),
			fwy_total= r$FBH,
			intercrops= ifelse(grepl("no intercrop",r$`Pigeopea intercrop variety`),"none","pigeon pea"),
			crop= "groundnut",
			seed_weight= (r$SM*r$`plot size`)/1000 # 1000 seed in g 
		)
	
		r1 <- carobiner::read.excel(f,  sheet = "Intercropping")
		
		d1 <- data.frame(
		   location= r1$Site,
		   rep= as.integer(r1$Replication),
		   plot_nr= r1$`Plot No`,
		   dmy_total= r1$DBH,
		   yield= r1$GY,
		   shelling_percentage= r1$`Shelling ratio`
		   
		)
		
    d <- merge(d, d1, by= c("location","rep","plot_nr"), all.x = TRUE)

    d$plot_nr <- NULL
		
	d$country <- "Malawi"
	d$adm1 <- "Central Region"
	d$adm2 <- "Lilongwe"  
	d$longitude <- 33.6538127
	d$latitude <- -13.9788154
	d$geo_from_source <- FALSE
	d$planting_date <- "2018"
	d$trial_id <- "1"
	d$irrigated <- NA
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "grain"

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files (path, meta, d)
	
}

