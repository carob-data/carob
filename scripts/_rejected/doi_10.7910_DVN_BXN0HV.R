# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Grain yield in the source data is reported as "Yield g/plant" and cannot be
# converted to kg/ha without a plant density


carob_script <- function(path) {

"
Replication Data for: SMIL Core Subset evaluation for drought and Striga resistance

Data on days to flowering, days to maturity, plant height and plant aspect score collected for the SMIL sorghum core subset evaluated for drought and Striga resistance at Melkassa and Mieso (Ethiopia) in 2020
"

	uri <- "doi:10.7910/DVN/BXN0HV"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "flowering_days;maturity_days;plant_height",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-16",
		carob_completion = 70,
		carob_effort = 2
	)

	ff <- ff[grepl("\\.xlsx$", basename(ff))]

	process <- function(f) {
		r <- carobiner::read.excel(f)
		data.frame(
			location = as.character(r$Site),
			plot_id = as.character(r$Plot),
			rep = as.integer(r$Replicate),
			treatment = as.character(r$Genotype),
			variety = as.character(r$Genotype),
			flowering_days = as.numeric(r$DTF),
			maturity_days = as.numeric(r$DTM),
			plant_height = as.numeric(r$PHTMean),
			crop = "sorghum"
		)
	}

	d <- do.call(rbind, lapply(ff, process))

	d$location[d$location == "MK"] <- "Melkassa"
	d$location[d$location == "MS"] <- "Mieso"

	d$yield <- as.numeric(NA)   # source yield is g/plant (see ISSUES)
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	d$country <- "Ethiopia"
	d$trial_id <- d$location
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$planting_date <- "2020"
	d$harvest_date <- "2020"

	geo <- data.frame(
		location = c("Melkassa", "Mieso"),
		longitude = c(39.326, 40.5638),
		latitude = c(8.417, 9.1779),
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by="location", all.x=TRUE)

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
