# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Line x tester (combining ability) design. "variety_type" distinguishes the
# experimental hybrids from their female/male parent lines and the checks. The
# identity of the specific parents used in each cross (Female/Male columns),
# the male-sterility reaction and panicle exsertion (PEMean) are available in
# the source data but are not standard carob variables and are not included here.


carob_script <- function(path) {

"
Combining ability study among key Ethiopian sorghum landraces collection

Data on days to flowering, plant height, plant aspect score and grain yield collected for hybrids from a line x tester combining ability study among key Ethiopian sorghum landraces evaluated at Mieso (Western Hararghe, Ethiopia) in 2020
"

	uri <- "doi:10.7910/DVN/KEZ8CS"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = "line x tester",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 80,
		carob_effort = 1
	)

	f <- ff[basename(ff) == "Combining ability among key Ethiopian sorghum landraces at Mieso 2020.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "West Hararghe",
	  adm3 = "Mieso",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  rep = as.integer(r$Rep),
	  treatment = as.character(r$Genotypes),
	  variety = as.character(r$Genotypes),
	  variety_pedigree = as.character(r$Pedigree),
	  flowering_days = as.numeric(r$DTF),
	  plant_height = as.numeric(r$Phmean),
	  yield = as.numeric(r$`Yield Kg/Ha`),
	  crop = "sorghum"
	)

	## entry type from the "Remark" column: experimental hybrids, the female and
	## male parent lines used in the crosses, and checks (C)
	d$variety_type <- c(Hybrid="hybrid", Female="female parent",
		Male="male parent", C="check")[as.character(r$Remark)]

	d$trial_id <- r$TrialID
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$location[d$location == "MS"] <- "Mieso"

	d$planting_date <- "2020"
	d$harvest_date <- "2020"

	d$longitude <- 40.5638
	d$latitude <- 9.1779
	d$geo_uncertainty <- 51603
	d$geo_source <- "GADM 4.1, adm3"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}
