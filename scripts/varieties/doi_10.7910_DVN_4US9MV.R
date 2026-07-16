# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
B-line observation Mieso 2015

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 59 selected B lines evaluated at Mieso (Western Hararghe, Ethiopia) in 2015
"

	uri <- "doi:10.7910/DVN/4US9MV"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield; plant_height;maturity_days;spike_density;plant_density;flowering_days", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-15",
		carob_completion = 100,	
		carob_effort = 4
	)
	

	f <- ff[basename(ff) == "B-line observation Mieso 2015.xlsx"]

	r <- carobiner::read.excel(f)


	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "Hararghe",
	  location = r$Site,
	  treatment = r$Genotype,
	  rep = as.integer(r$Replicate),
	  emergence_days = as.numeric(r$DTE),
	  planting_date = as.character(as.Date(r$Sown, format = "%d/%m/%Y")),
	  harvest_date = as.character(as.Date(r$DateHarvest, format = "%d/%m/%Y")),
	  plant_height = r$PHTMean,
	  yield = r$`YieldKg/Ha`,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  plot_id = as.character(r$Plot),
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  plot_area = as.numeric(r$PlotArea),
	  plant_density = 10000 * r$StandAtHarv / r$PlotArea,
	  spike_density = 10000 * r$`Heads/Plot` / r$PlotArea,
	  crop = "sorghum"
	)
	
	d$trial_id <- "1"
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	

	d$longitude <- 40.5638
	d$latitude <- 9.1779
	geo_uncertainty = 51603
	geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- FALSE
	
	
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$fertilizer_type <- NA

	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	
	carobiner::write_files(path, meta, d)
}




