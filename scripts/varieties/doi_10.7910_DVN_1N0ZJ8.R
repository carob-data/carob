# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
R lines observation nursery at Mieso 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for selected R lines evaluated at Mieso (Western Hararghe, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/1N0ZJ8"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;flowering_days;maturity_days;plant_density",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 80,
		carob_effort = 0.1
	)

	f <- ff[basename(ff) == "R lines observation nursery at Mieso 2014.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "West Hararghe",
	  adm3 = "Mieso",
	  location = r$Site,
	  treatment = r$Genotype,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  variety_type = "R-line (hybrid parent)",
	  rep = as.integer(r$Replicate),
	  plot_id = as.character(r$Plot),
	  planting_date = as.character(as.Date(r$Sown)),
	  harvest_date = as.character(as.Date(r$DateHarvest)),
	  plot_area = as.numeric(r$PlotArea),
	  plant_height = r$PHTMean,
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  yield = r$`YieldKg/Ha`,
	  plant_density = 10000 * as.numeric(r$StandAtHarv) / as.numeric(r$PlotArea),
	  spike_density = 10000 * as.numeric(r$`Heads/Plot`) / as.numeric(r$PlotArea),
	  pest_severity = as.character(r$InsectScore),
	  disease_severity = as.character(r$DiseaseScore),
	  drought_stress = as.character(r$DroughtScore),
	  crop = "sorghum"
	)

	d$trial_id <- r$Type
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$longitude <- 40.5638
	d$latitude <- 9.1779
	d$geo_uncertainty <- 51603
	d$geo_source <- "GADM 4.1, adm3"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$plant_height[d$plant_height == 0] <- NA

	carobiner::write_files(path, meta, d)
}
