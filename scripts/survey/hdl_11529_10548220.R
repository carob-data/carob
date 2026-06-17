# R script for "carob"

carob_script <- function(path) {
  
"Description: Survey at multiple locations in Tanzania (600 to 2100 m) to establish baseline yields at farm level. (2016)"
  
	uri <- "hdl:11529/10548220"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project=NA,
		publication=NA,
		data_organization = "CIMMYT",
		data_type="crop-cuts", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Fredy Chimire",
		carob_date="2024-04-06",
		carob_effort = NA
	)
  
	f <- ff[basename(ff) == "TZ_TAMASA_APS_2016_Yield_MetaData.xlsx"]
	r <- carobiner::read.excel(f,sheet = "Corrected-Raw-Data", n_max=1840)
	d <- data.frame(
		country=r$Country,
		crop="maize", 
		yield_part="grain",
		yield= as.numeric(r$`Grain yield (kg/ha@12.5%)`), 
        adm1 = r$Zone,
		adm2 = r$Region,
		adm3 = r$District,
		adm4=r$Ward,
		location=r$Village,
		latitude= r$Latitude,
		longitude=r$Longitude,
		geo_from_source = TRUE,
		elevation=r$Altitude
	)
    
	d$is_survey <- TRUE

	d$planting_date <- "2016"
	d <- d[!is.na(d$yield), ]
	d$trial_id <- as.character(1:nrow(d))
	d <- d[!is.na(d$yield), ]

	d$on_farm <- TRUE
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

  
	carobiner::write_files(meta, d, path=path)
}
