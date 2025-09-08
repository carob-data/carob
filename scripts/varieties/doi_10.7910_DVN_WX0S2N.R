# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Drought genetics at Mieso 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 408 entries under drought genetic study conducted at Mieso (Western Hararghe, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/WX0S2N"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		carob_date = "2025-09-08",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code;variety_pedigree",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Drought genetics at Mieso 2014.xlsx"]

	r <- carobiner::read.excel(f, fix_names = TRUE)

	d <- data.frame(
		location = r$Site,
		variety_type= r$Type,
		rep = as.integer(r$Replicate),
		variety_code = as.character(r$Genotype),
		variety_pedigree = r$Pedigree,
		planting_date = as.character(r$Sown),
		flowering_days = r$DTF,
		plant_height = r$PHTMean,
		plot_length = r$PlotLength,
		plot_width = r$PlotWidth,
		plot_area = r$PlotArea,
		maturity_days = r$DTM,
		harvest_date = as.character(r$DateHarvest),
		plot_id = as.character(r$Plot),
		yield = r$YieldKg.Ha,
		stress= "drought",
		longitude = 40.7538  ,
		latitude = 9.2333 ,
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



