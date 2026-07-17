# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
High yield potential hybrids at Shoarobit 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for selected high yield potential hybrids evaluated at Shoarobit (North Shewa, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/GM4ZN4"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety_code",
		response_vars = "yield;plant_height;flowering_days;maturity_days;plant_density",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 90,
		carob_effort = 0.1
	)

	f <- ff[basename(ff) == "High yield potential hybrids at Shoarobit 2014.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "Ethiopia",
		adm2 = "North Shewa",
		adm3 = "Kewet",
		location = r$Site,
		plot_id = as.character(r$Plot),
		variety_type = "high yield potential hybrid",
		rep = as.integer(r$Replicate),
		variety_code = r$Genotype,
		variety_pedigree = r$Pedigree,
		planting_date = as.character(as.Date(r$Sown)),
		harvest_date = as.character(as.Date(r$DateHarvest)),
		flowering_days = r$DTF,
		plot_area = as.numeric(r$PlotArea),
		plot_length = as.numeric(r$PlotLength),
		plot_width = as.numeric(r$PlotWidth),
		plant_height = r$PHTMean,
		maturity_days = r$DTM,
		plant_density = 10000 * r$StandAtHarv / r$PlotArea,
		yield = r$`YieldKg/Ha`,
		drought_stress = as.character(r$DroughtScore),
		disease_severity = as.character(r$DiseaseScore),
		pest_severity = as.character(r$InsectScore),
		crop = "sorghum"
	)

	## local hybrid check
	d$variety_type[r$Genotype == "ESH-3"] <- "hybrid"

	d$trial_id <- r$Type
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$longitude <- 39.897
	d$latitude <- 9.995
	d$geo_uncertainty <- 25628
	d$geo_source <- "Google Maps"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}
