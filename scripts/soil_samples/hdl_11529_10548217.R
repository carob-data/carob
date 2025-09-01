# R script for "carob"

carob_script <- function(path) {

"Soil analysis from 0-20 cm depth from Nutrient Ommission Trials (NOTS) sites conducted in two regions in Tanzania in 2015/ 2016 season."

	uri <- "hdl:11529/10548217"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "experiment",
		response_vars = "none",
		treatment_vars = "none", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-06-13"
	)

	f <- ff[basename(ff) == "TAMASA_NOT_Soil_Data_2015-2016.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Revised_Data")
	r <- r[!is.na(r$Country), ]
	r <- r[r$Country == "Tanzania", ]


	d <- data.frame(
		country=r$Country,
		location=r$Zone,
		site=r$Region,
		adm1=r$District,
		adm2=r$Ward,
		adm3=r$Village,
		latitude=r$Latitude,
		longitude=r$Longitude,
		elevation=r$Altitude,
		geo_from_source= TRUE,
#		record_id=r$Fcode,
		soil_C=r$C,
		soil_pH=r$pH,
		soil_Al = r$Al,
		soil_Ca = r$Ca,
		soil_EC=r$EC.S*0.1,
		soil_S = r$S,
		soil_Mn = r$Mn,
		soil_P = r$P,
		soil_Zn = r$Zn,
		soil_K = r$K,
		soil_Mg=r$Mg,
		soil_Na = r$Na,
		soil_Fe = r$Fe,
		soil_B = r$B,
		soil_N = r$N * 10
	)

	d$depth_top <- 0
	d$depth_bottom <- 20

	soilmeta <- data.frame(
		variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_P", "soil_Zn"),
		method = "Mehlich3 extraction"
	)


##### Time #####
	#start and end dates were obtained from the metadata sheet in the data set file
	#start_date <- 2016-01-05
	#end_date <- 2016-01-12
	
	d$date <- "2016-01"	
	carobiner::write_files(path, meta, d, var_meta=soilmeta)
}



