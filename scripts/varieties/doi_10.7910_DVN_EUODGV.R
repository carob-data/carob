# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Selected white hybrids Mieso 2015

Data on agronomic traits of maturity, grain yield and plant aspect score collected for 8 selected white hybrids evaluated at Mieso (Western Hararghe, Ethiopia) in 2015
"
   
	uri <- "doi:10.7910/DVN/EUODGV"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		carob_date = "2025-09-03",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Selected white hybrids Mieso 2015.xlsx"]

	r <- carobiner::read.excel(f)


	d <- data.frame(
		location = r$Site,
		plot_id = as.character(r$Plot),
		variety_type = r$Type,
		rep = as.integer(r$Replicate),
		planting_date = as.character(as.Date(r$Sown, "%d/%m/%Y")),
		flowering_days = r$DTF,
		variety = r$Genotype,
		variety_pedigree = r$Pedigree,
		maturity_days = r$DTM,
		harvest_date= as.character(r$DateHarvest),
		yield = r$`YieldKg/Ha`,
		longitude = 40.754 ,
		latitude = 9.2330,
		country = "Ethiopia",
		crop = "sorghum",
		yield_part = "grain",
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = FALSE,
		trial_id = "1",
		yield_moisture = as.numeric(NA),
		irrigated = NA
		
		)

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
	

carobiner::write_files(path, meta, d)

}



