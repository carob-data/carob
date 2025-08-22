# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"This data covers the crop aspect of the Agronomy Panel Survey (APS) implemented in 99 selected communities across 17 LGAs in 22 randomly selected 10km x 10 km sampling grids from Kano, Kaduna and Katsina States. The XLS file has tabs for Metadata, Variables, Data and Data history. (2017-12-07)"

	uri <- "hdl:11529/11154"
	group <- "survey"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-10-31",
		notes = NA, 
		design = NA
	)
	

	f <- ff[basename(ff) == "NG_APS_CC_Yield_2016.xlsx"]

	r <- carobiner::read.excel(f, sheet = "Data")

	d <- data.frame(
		location = r$ward,
		adm1 = r$state,
		adm2 = r$village,
		elevation =r$`_gps_altitude`,
		longitude = r$`_gps_longitude`,
		latitude = r$`_gps_latitude`,
		yield= r$`12%_yield(kg/ha)`,
		yield_moisture=12,
		seed_weight=r$`graiwt(kg)`,
		row_spacing=r$`rowdist(cm)`,
		plant_spacing=r$`plantdis(cm)`,
		trial_id=r$barcode_ccut
	)
	
	d$country <- "Nigeria"	
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <-FALSE
	d$geo_from_source <- TRUE

	d$planting_date <- "2016"
	d$harvest_date  <- "2016"
  
	d$adm1<- gsub("KD", "Kano", d$adm1)
	d$adm1<- gsub("KT", "Katsina", d$adm1)
	d$adm1<- gsub("KN", "Kaduna", d$adm1)
	
	d$N_fertilizer <- d$P_fertilizer  <- d$K_fertilizer <- as.numeric(NA)
	#d$plant_spacing[d$plant_spacing %in% c(116, 230)] <- NA

	carobiner::write_files(path, meta, d)
}



