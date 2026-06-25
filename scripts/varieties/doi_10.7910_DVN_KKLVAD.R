# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Dual Purpose hybrids Mieso 2017

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for 16 experimental dual purpose hybrids evaluated against 4 OPV checks at Mieso (Western Hararghe, Ethiopia) in 2017
"

	uri <- "doi:10.7910/DVN/KKLVAD"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		carob_effort = NA,
		carob_date = "2026-05-29",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_completion = 100,	
		notes = NA
	)
	
	f <- ff[basename(ff) == "Dual Purpose hybrids Mieso 2017.csv"]
	r <- read.csv(f)

	d <- data.frame(
		country="Ethiopia",
		crop="sorghum",
		location = r$Site,
		plot_id = as.character(r$Plot),
		rep=as.integer(r$Replicate),
		variety_pedigree=r$Pedigree,
		variety=r$Genotype,
		planting_date=r$Sown,
		flowering_days=as.numeric(r$DTF),
		plant_height=as.numeric(r$PHTMean),
		drought_stress=as.character(r$DroughtScore),
		maturity_days=as.numeric(r$DTM),
		yield=as.numeric(r$YieldKgHa)
	)
	
	d$planting_date <- as.character(as.Date(d$planting_date, format = "%d/%m/%Y"))
	d$trial_id <- paste0(d$location,d$planting_date, sep="_")
	d$on_farm <- FALSE #dataset says "...4 OPV checks at Mieso", not in Mieso, so assumption is the experiment was conducted at Mieso Agricultural Research Sub-Center
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
	d$longitude <- 40.750
	d$latitude <- 9.233
	d$geo_from_source <- FALSE
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d <- d[!is.na(d$plot_id), ]
	d[d == ""] <- NA

	carobiner::write_files(path, meta, d)
}
