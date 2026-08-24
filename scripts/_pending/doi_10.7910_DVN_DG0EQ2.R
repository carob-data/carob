# R script for "carob"
# license: GPL (>=3)

## REJECTED
# This dataset is a crossing block / germplasm list (selected landraces and
# elite lines for heterotic pool determination). It has the names of the
# landraces and their collection origin (region/zone/woreda) and diversity /
# sterility group. 

carob_script <- function(path) {

"
A crossing block for determining heterotic pools among Ethiopian sorghum landraces

List of selected Ethiopian sorghum landraces and elite lines assembled as a crossing block to determine heterotic pools in the SMIL Phase II project. The dataset records the landrace names, their collection origin (region, zone, woreda) and diversity/sterility group; it does not contain agronomic measurements.
"

	uri <- "doi:10.7910/DVN/DG0EQ2"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-16",
		carob_completion = 20,
		carob_effort = 2
	)

	f <- ff[basename(ff) == "Crossing block for Ethiopian sorghum landraces.xlsx"]
	r <- as.data.frame(readxl::read_excel(f, sheet="Sheet2", skip=3))

	d <- data.frame(
		country = "Ethiopia",
		adm1 = as.character(r$Region),
		adm2 = as.character(r$Zone),
		adm3 = as.character(r$`Woreda/District`),
		variety = as.character(r$Name),
		variety_type = "landrace",
		crop = "sorghum"
	)

	d <- d[!is.na(d$variety) & d$variety != "", ]
	d <- unique(d)

	d$trial_id <- "1"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)

	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- FALSE

	d$yield <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
