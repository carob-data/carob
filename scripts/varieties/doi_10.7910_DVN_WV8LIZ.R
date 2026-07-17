# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Stay-green hybrids Shiraro 2015

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for experimental stay-green hybrids evaluated at Shiraro (Western Tigray, Ethiopia) in 2015
"

	uri <- "doi:10.7910/DVN/WV8LIZ"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height;maturity_days;spike_density;plant_density",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 90,
		carob_effort = 0.1
	)

	f <- ff[basename(ff) == "Stay-green hybrids Shiraro 2015.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "Western Tigray",
	  adm3 = "Tahtay Adiyabo",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = as.character(as.Date(r$Sown, "%d/%m/%Y")),
	  harvest_date = as.character(as.Date(r$DateHarvest, "%d/%m/%Y")),
	  plot_area = as.numeric(r$PlotArea),
	  treatment = r$Genotype,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  variety_type = "stay-green hybrid",
	  rep = as.integer(r$Replicate),
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`YieldKg/Ha`,
	  plant_density = 10000 * as.numeric(r$StandAtHarv) / as.numeric(r$PlotArea),
	  spike_density = 10000 * as.numeric(r$`Heads/Plot`) / as.numeric(r$PlotArea),
	  drought_stress = as.character(r$DroughtScore),
	  crop = "sorghum"
	)

	d$trial_id <- r$Type
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$longitude <- 37.773
	d$latitude <- 14.396
	d$geo_uncertainty <- 49096
	d$geo_source <- "GADM 4.1, adm3"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA

	d$yield_part <- "grain"
	d$yield_isfresh <- NA
	d$yield_moisture <- as.numeric(NA)

	## records with no variety (the treatment) and no yield
	d <- d[!is.na(d$variety), ]

	carobiner::write_files(path, meta, d)
}
