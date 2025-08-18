# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"Data of soil samples (0-20, 20-50cm) as part of the Agronomy Panel Survey (APS) implemented in the Southern Highlands, Northern and Eastern Zones of Tanzania. Farmers maize fields were randomly selected three 1 X 1 km areas from 22 10km x 10 km sampling grids. Replicated crop cuts were also made along with household, focal plot and community surveys. These can be found under TAMASA_TZ_APS_xxxxx_2016 (2018-02-18)"

	uri <- "hdl:11529/10548272"
	group <- "soil_samples"

 
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2025-08-18",
		notes = NA,
		design = NA
	)

	f <- ff[basename(ff) == "TAMASA_TZ_APS_Soil_2016.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Data", n_max = 1155)
	#r <- carobiner::read.excel(f,sheet = "Data")

	d <- data.frame(
		country = r$Country,
		adm1 = r$Region,
		adm2 = r$District,
		adm3 = r$Ward,
		location = r$Village,
		geo_from_source = TRUE,
		latitude = r$Latitude,
		longitude = r$Longitude,
		elevation = r$Altitude,
		depth = r$Depth,
		soil_SOC = r$C,
		soil_pH = r$pH,
		soil_Al = r$Al,
		soil_Ca = r$Ca,
		soil_EC = r$EC.S * 100,
		soil_S = r$S,
		soil_Mn = r$Mn,
		soil_P_total = r$P,
		soil_Zn = r$Zn,
		soil_K = r$K,
		soil_ex_Mg = r$Mg,
		soil_ex_Na = r$Na,
		soil_Fe = r$Fe,
		soil_B = r$B,
		soil_N = r$N * 10000,
		crop= "maize"
	)
	
	d$treatment <- NA
	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$planting_date <- NA
	d$harvest_date  <- NA
  d$P_fertilizer <- NA
  d$K_fertilizer <-NA
  d$N_fertilizer <- NA
  d$S_fertilizer <- NA
  d$lime <- NA
  d$fertlizer_type <- NA
	d$yield <- NA
	d$yield_part <- "grain"
	
	d$soil_Al[d$soil_Al > 200] <- NA
	d$soil_N[d$soil_N > 10000] <- NA
	d$soil_EC[d$soil_EC > 20] <- NA
	d$soil_ex_Mg[d$soil_ex_Mg > 200] <- NA
	d$soil_ex_Na[d$soil_ex_Na > 200] <- NA
	
	# Split depth column into two parts
	depths <- do.call(rbind, strsplit(as.character(d$depth), "-"))
	
	# Add them as new numeric columns
	d$depth_top <- as.numeric(depths[,1])
	d$depth_bottom <- as.numeric(depths[,2])
	
	d$depth <- NULL
	
	carobiner::write_files(path, meta, d)
}

