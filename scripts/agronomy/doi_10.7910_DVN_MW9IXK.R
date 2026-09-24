# R script for "carob"
# license: GPL (>=3)

## ISSUES
## There were some 0 values on row_spacing which resulted in out of bounds warning
## There is a row where the subsample_dw_grain is larger than subsample_fw_grain creating a negative value, creating out of bounds warning for the yield_moisture
## There were some missing values for subsample_fw_grain and subsample_dw_grain creating NA values on yield_moisture

carob_script <- function(path) {

"
Evaluation of technologies through onfarm bean mother demos

This dataset contains raw and extrapolated yield data from on farm mother demos of beans for evaluating performance of technologies in Southern Highlands of Tanzania. Only 57 of on-farm demos qualified for the evaluation where the technologies evaluated were improved bean varieties compared to local, dressed and undressed bean varieties, and the use of inorganic fertilizer.  The demos were planted in 2016/2017 planting seasons and evaluated in 2017
"

	uri <- "doi:10.7910/DVN/MW9IXK"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT; KSU",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "fertilizer_used;variety",
		response_vars = "yield", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-09-17",
		carob_completion = 90,	
		carob_effort = 5)
	

	f1 <- ff[basename(ff) == "Mother Bean Demos with Yield assessment in 2017.xlsx"]

	r1 <- carobiner::read.excel(f1)


	d <- data.frame(
	  country = "Tanzania",
		adm2 = r1$District,
		latitude = r1$Latitude,
		longitude = r1$Longitude,
		location_id = as.character(r1$`ID (Location, Treatment)(Put this ID on sample bags)`),
		fertilizer_used = grepl("Fertilizer|Fert", r1$Treatment),
		plot_id = as.character(r1$`Plot number(see map)`),
		variety = r1$Variety,
		rep = as.integer(r1$Rep),
		row_spacing = r1$`If row, Average Row spacing from three measurements`*100,
		subsample_fw_grain = as.numeric(r1$`Subsample of Beans fresh weight  from trial (approx. 200 g) (sample to be dried in the sun or oven)`),
		subsample_dw_grain = as.numeric(r1$`Subsample of Bean Dry Weight Use to calculate moisture content (one sample per field trial)`),
		yield = as.numeric(r1$`Yield per Hactare`)
	)
	
	
	d$trial_id <- "1"
	d$on_farm <- TRUE      #based on data description
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- TRUE


	d$planting_date <- "2016"  #The demos were planted in 2016/2017 planting seasons and evaluated in 2017-based on the description
	d$harvest_date  <- NA

  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- NA
  d$inoculated <- NA
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(((d$subsample_fw_grain - d$subsample_dw_grain) / d$subsample_fw_grain) * 100)
	d$crop <- "common bean"


	carobiner::write_files(path, meta, d)
}

