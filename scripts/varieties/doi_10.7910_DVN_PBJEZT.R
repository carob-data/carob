# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Drought genetics at Kobo 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 408 entries under drought genetic study conducted at Kobo (North Wello, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/PBJEZT"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		carob_date = "2025-09-03",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code;variety_pedigree",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
   	completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Drought genetics at Kobo 2014.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="Drought genetics kobo", fix_names = TRUE)
	#r2 <- carobiner::read.excel(f1, sheet="Sheet2")
	#r3 <- carobiner::read.excel(f1, sheet="Sheet3")

	d <- data.frame(
	   location = r1$Site,
		plot_id = as.character(r1$Plot),
		variety_type = r1$Type,
		rep = as.integer(r1$Replicate),
		variety_code = as.character(r1$Genotype),
		variety_pedigree = r1$Pedigree,
		planting_date = as.character(r1$Sown),
		flowering_days = r1$DTF,
		plant_height = r1$PHTMean,
		plot_length = r1$PlotLength,
		plot_width = r1$PlotWidth,
		plot_area = r1$PlotArea,
		bird_damage = as.character(r1$BirdDamage),
		maturity_days = r1$DTM,
		harvest_date = as.character(r1$DateHarvest),
		yield = r1$YieldKg.Ha,
		stress= "drought",
		longitude = 41.473 ,
		latitude = 9.3785 ,
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
	
	### drop rows with missing yield
	d <- d[!is.na(d$yield),] 
	

	carobiner::write_files(path, meta, d)
}


