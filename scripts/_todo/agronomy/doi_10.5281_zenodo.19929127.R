# R script for "carob"

## NOTES
# Three-season maize field trial on an Oxisol in Siaya, Kenya (2023-2025). Yields in t/ha converted to kg/ha.
# Treatment N:P:K and amendment rates are from the dataset README (not the CSV).
# Zero Control is N/P/K = 0. Biochar/basanite rates in t/ha converted to kg/ha.
# P-sorption CSV is an in-vitro batch experiment and was omitted.

## ISSUES
# Planting and harvest dates are not in the files (only season 1/2/3).
# Location is "Siaya, western Kenya" only; human must georeference.
# In-situ soil pH has a subgroup; not averaged. Root morphology, Striga counts, TFP/TPP not mapped.
# P in N:P:K is treated as elemental P as written in the README.
# write_files: missing latitude/longitude, planting_date, harvest_date, yield_moisture; org names HGU / Ithaka Institute / Offenburg UAS not in vocab.

## NEW VARIABLES
# biochar_amount_: biochar applied (kg/ha)
# rock_powder_amount_: basanite/rock powder applied (kg/ha)

carob_script <- function(path) {

"
Data for: Effect of biochar and basanite on maize yield, fertilizer efficiency and phosphorus sorption in a tropical Oxisol (PyMiCCS Kenya field trial)

Field trial and in-vitro batch experiment data. This dataset contains the raw experimental data underlying the manuscript 'Effect of biochar and basanite on maize yield, fertilizer efficiency and phosphorus sorption in a tropical Oxisol' by Meyer zu Drewer, Roobroeck, Maritim, Lotz, Kammann, Hartmann, Broszat, Schmidt, and Hagemann. The data were collected during a three-season maize field trial on an Oxisol in Siaya, western Kenya (2023-2025), and during a complementary six-week in-vitro phosphorus sorption batch experiment.
"

	uri <- "doi:10.5281/zenodo.19929127"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "HGU; Offenburg UAS; Ithaka Institute",
		publication = NA,
		project = "PyMiCCS",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;biochar_amount_;rock_powder_amount_",
		response_vars = "yield;dmy_residue",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "Data_Meyer_zu_Drewer_Kenya.csv"]
	r <- read.csv(f, sep=";", skip=1, stringsAsFactors=FALSE)
	r$value <- as.numeric(r$value)

	gy <- r[r$parameter == "Grain Yield 1st season t ha", ]
	sy <- r[r$parameter == "Stover Yield 1st season t ha", ]
	d1 <- data.frame(
		treatment = gy$treatment,
		rep = as.integer(gy$replicate),
		yield = gy$value * 1000,
		dmy_residue = sy$value[match(paste(gy$treatment, gy$replicate), paste(sy$treatment, sy$replicate))] * 1000,
		season = "1"
	)

	gy <- r[r$parameter == "Grain Yield 2nd season t ha", ]
	sy <- r[r$parameter == "Stover Yield 2nd season t ha", ]
	d2 <- data.frame(
		treatment = gy$treatment,
		rep = as.integer(gy$replicate),
		yield = gy$value * 1000,
		dmy_residue = sy$value[match(paste(gy$treatment, gy$replicate), paste(sy$treatment, sy$replicate))] * 1000,
		season = "2"
	)

	gy <- r[r$parameter == "Grain Yield 3rd season t ha", ]
	sy <- r[r$parameter == "Stover Yield 3rd season t ha", ]
	d3 <- data.frame(
		treatment = gy$treatment,
		rep = as.integer(gy$replicate),
		yield = gy$value * 1000,
		dmy_residue = sy$value[match(paste(gy$treatment, gy$replicate), paste(sy$treatment, sy$replicate))] * 1000,
		season = "3"
	)

	d <- rbind(d1, d2, d3)

	# README rates. Biochar treatments: 90:0:0 in season 1, 90:10:20 in seasons 2-3.
	d$N_fertilizer <- ifelse(d$treatment == "Zero Control", 0, 90)
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	d$P_fertilizer[d$treatment == "PK-half"] <- 10
	d$K_fertilizer[d$treatment == "PK-half"] <- 20
	d$P_fertilizer[d$treatment == "PK-full"] <- 20
	d$K_fertilizer[d$treatment == "PK-full"] <- 40
	# rock-enhanced / co-app / biochar / rock powder: season 1 = N only; later seasons PK-half
	bio <- d$treatment %in% c("RE-BC_5", "RE-BC_10", "Co-Ap_10", "BC_5", "RP_5")
	d$P_fertilizer[bio & d$season != "1"] <- 10
	d$K_fertilizer[bio & d$season != "1"] <- 20

	d$biochar_amount_ <- 0
	d$rock_powder_amount_ <- 0
	d$biochar_amount_[d$treatment == "RE-BC_5"] <- 5000
	d$rock_powder_amount_[d$treatment == "RE-BC_5"] <- 0
	d$biochar_amount_[d$treatment == "RE-BC_10"] <- 10000
	d$biochar_amount_[d$treatment == "Co-Ap_10"] <- 5000
	d$rock_powder_amount_[d$treatment == "Co-Ap_10"] <- 5000
	d$biochar_amount_[d$treatment == "BC_5"] <- 5000
	d$rock_powder_amount_[d$treatment == "RP_5"] <- 5000

	d$OM_used <- d$biochar_amount_ > 0
	d$OM_type <- ifelse(d$biochar_amount_ > 0, "biochar", NA)
	d$OM_amount <- d$biochar_amount_

	d$crop <- "maize"
	d$country <- "Kenya"
	d$adm1 <- "Siaya"
	d$location <- "Siaya"
	d$trial_id <- paste("Siaya", d$season, sep="_")
	d$season <- NULL
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "grain"
	d$geo_from_source <- FALSE
	# latitude/longitude not in source; human must georeference

	carobiner::write_files(path, meta, d)
}
