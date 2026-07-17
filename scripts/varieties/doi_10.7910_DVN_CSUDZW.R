# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
Red sorghum hybrids at Kobo 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for experimental red hybrids evaluated at Kobo (North Wollo, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/CSUDZW"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;plant_height;flowering_days;maturity_days",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 90,
		carob_effort = 0.1
	)

	f <- ff[basename(ff) == "Red sorghum hybrids at Kobo 2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "North Wollo",
	  adm3 = "Kobo",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = as.character(as.Date(r$Sown)),
	  harvest_date = as.character(as.Date(r$DateHarvest)),
	  plot_area = as.numeric(r$PlotArea),
	  treatment = r$Genotype,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  variety_type = "red hybrid",
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`YieldKg/Ha`,
	  pest_severity = as.character(r$InsectScore),
	  disease_severity = as.character(r$DiseaseScore),
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  crop = "sorghum"
	)

	## checks: one hybrid check (ESH-3) and one OPV check (Dekeba)
	d$variety_type[d$variety == "ESH-3"] <- "hybrid"
	d$variety_type[d$variety == "Dekeba"] <- "OPV"

	d$trial_id <- paste(d$location, d$planting_date, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$longitude <- 39.643
	d$latitude <- 12.1038
	d$geo_uncertainty <- 36657
	d$geo_source <- "GADM 4.1, adm3"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA

	d$yield_part <- "grain"
	d$yield_isfresh <- NA
	d$yield_moisture <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
