# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Selected white hybrids Shiraro 2015

Data on agronomic traits of maturity, grain yield and plant aspect score collected for selected white hybrids evaluated at Shiraro (Western Tigray, Ethiopia) in 2015
"

	uri <- "doi:10.7910/DVN/0OLIJM"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;maturity_days;plant_height",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 90,
		carob_effort = 1
	)

	f <- ff[basename(ff) == "Selected white hybrids Shiraro 2015.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "Ethiopia",
		adm2 = "Western Tigray",
		adm3 = "Tahtay Adiyabo",
		location = r$Site,
		plot_id = as.character(r$Plot),
		variety_type = "white hybrid",
		rep = as.integer(r$Replicate),
		planting_date = as.character(as.Date(r$Sown, "%d/%m/%Y")),
		harvest_date = as.character(as.Date(r$DateHarvest, "%d/%m/%Y")),
		flowering_days = r$DTF,
		maturity_days = r$DTM,
		variety = r$Genotype,
		variety_pedigree = r$Pedigree,
		plant_height = r$PHTMean,
		yield = r$`YieldKg/Ha`,
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

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}
