# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
High yield potential hybrids at Mieso 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 7 selected high yield potential hybrids tested compared to a local hybrid check at Mieso (Western Hararghe, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/PICJCW"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PU", #Purdue University
		publication = NA,
		project = NA,
		carob_date = "2025-08-22",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "High yield potential hybrids at Mieso 2014.xlsx"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
		location = r$Site,
		plot_id = as.character(r$Plot),
		variety_type= r$Type,
		rep= as.integer(r$Replicate),
		variety_code= r$Genotype,
		variety_pedigree= r$Pedigree,
		planting_date= as.character(r$Sown),
		flowering_days= r$DTF,
		plot_area= r$PlotArea,
		plant_height= r$PHTMean,
		plot_length= r$PlotLength,
		plot_width= r$PlotWidth,
		maturity_days= r$DTM,
		harvest_date= as.character(r$DateHarvest),
		plant_density= (r$StandAtHarv/r$PlotArea)*10000,
		yield = r$`YieldKg/Ha`,
		crop= "sorghum",
		country= "Ethiopia",
		longitude= 40.755 ,
		latitude= 9.232 ,
		yield_part= "grain",
		is_survey= FALSE,
		on_farm= TRUE,
		irrigated= NA,
		yield_moisture= as.numeric(NA),
		geo_from_source= FALSE,
		trial_id= "1"
		
	)

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
		
	
carobiner::write_files(path, meta, d)

}


