# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The previous draft asked what the location codes DT, OPT, LN and HN mean, and
# assigned coordinates that are not in the file: Minjibir 12.177, 8.659; Ikenne
# 6.901, 3.698; Kadawa 11.633, 8.434; Mokwa 9.29, 5.06; Ile-Ife 6.7604, 6.207;
# Abuja 9.064, 7.489. Confirm them before using them.
# YIELD units are not stated in the column name.
# str_co, str_rat, pollen, silking, plant and ear aspect, and lodging columns
# from BA15188 ACROSS.csv are not yet mapped to current names.
# The associated paper is doi:10.1038/s41598-019-50345-3. A RIS file has not been added.
# The year below is taken from the location code (15 = 2015), as in the previous draft.


carob_script <- function(path) {

"
Availability of multiple-stress tolerant maize is critical for improvement in maize production in West and Central Africa. Two hundred and five testcrosses of early maturing yellow maize inbreds were grown with five hybrid checks under Striga, low soil nitrogen, drought and optimal conditions in Nigeria in 2015 and 2016.
"

	uri <- "doi:10.25502/K7Q1-BW26/D"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IITA",
		publication = "doi:10.1038/s41598-019-50345-3",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_completion = 25,
		carob_effort = 0,
		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		notes = "",
		
	)

	r <- read.csv(ff[basename(ff) == "BA15188 ACROSS.csv"])

	d <- data.frame(
		location = r$LOC,
		rep = r$Rep,
		variety_code = as.character(r$Entry),
		variety = r$Pedigree,
		yield = r$YIELD,
		plant_height = r$PLHT,
		ear_height = r$EHT,
		asi = r$ASI,
		country = "Nigeria",
		crop = "maize",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,
		yield_part = "grain",
		borer_trial = FALSE,
		striga_trial = grepl("STR", r$LOC),
		striga_infected = grepl("STR", r$LOC)
	)

	d$trial_id <- paste(r$ID, d$location, sep = "-")
	d$planting_date <- "2015"
	d$planting_date[grepl("16", d$location)] <- "2016"
	d$planting_date[grepl("KDW16DT", r$LOC)] <- "2015"
	d$planting_date[grepl("IK15DT|MI15DT", r$LOC)] <- "2014"

	d$location[grepl("MI15DT", r$LOC)] <- "Minjibir"
	d$location[grepl("^IK1", r$LOC)] <- "Ikenne"
	d$location[r$LOC == "KDW16DT"] <- "Kadawa"
	d$location[grepl("^MK1", r$LOC)] <- "Mokwa"
	d$location[grepl("^IF1", r$LOC)] <- "Ile-Ife"
	d$location[grepl("^AB1", r$LOC)] <- "Abuja"

	carobiner::write_files(path, meta, d)
}
