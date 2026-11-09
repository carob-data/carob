# R script for "carob"
# license: GPL (>=3)

## NOTES
# Management options trial at Maradi, Niger, rainy season 2015. Factorial RCBD with 3 replications (Block = rep).
# Factors: seed treatment (Apron or none), organic manure (200 g per hill, broadcast, or none),
#   NPK (3 g per hill, 6 g per hill, recommended rate, or none) and genotype.
# The manure level (hill or broadcast) is only in "treatment"; the NPK level is in N_fert_level.
# ICSV 1049, Kapelga and Sariaso 11 are sorghum varieties, so crop is "sorghum".
# Plant height converted from m to cm. Harvest index converted from fraction to %.
# Stover yield is taken as dry matter (dmy_residue). Yields are in kg/ha.

## ISSUES
# if we knew how many hills there were per ha, we could compute fertilizer dosage, and striga density. 


carob_script <- function(path) {

"
Management options to improve millet and sorghum productivity, rainy season 2015.
"

	uri <- "doi:10.21421/D2/TVF4M5"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "factorial RCBD with 3 replications",
		data_type = "experiment",
		treatment_vars = "seed_treatment;OM_used;fertilizer_used;N_fert_level;variety",
		response_vars = "yield;dmy_residue",
		notes = NA,
		carob_date = "2026-11-08",
		carob_contributor = "Ajith S",
		carob_completion = 75,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data file of Managment options to improve Millet and Sorghum productivity rainy season 2015.xlsx"]
	r <- carobiner::read.excel(f, sheet="Managment options", na=c("", "NA"))
	r2 <- carobiner::read.excel(f, sheet="Productivity rainy season", na=c("", "NA"))
	names(r) <- trimws(names(r))

	fert <- trimws(r[["Minerals l"]])

	d <- data.frame(
		trial_id = "Maradi_2015",
		country = "Niger",
		adm1 = "Maradi",
		location = "Maradi",
		latitude = 13.4957,
		longitude = 7.1060,
		geo_from_source = TRUE,
		crop = "sorghum",
		planting_date = "2015",
		harvest_date = as.character(NA),
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,
		rep = as.integer(r$Block),
		plot_id = as.character(r$PlotNo),
		variety = trimws(r[["Millet_genotype l"]]),
		seed_treatment = ifelse(trimws(r[["Seed treatment l"]]) == "No Apron", "none", "Apron"),
		OM_used = trimws(r[["Organic amend l"]]) != "No Org Man",
		fertilizer_used = trimws(r[["Minerals l"]]) != "No Min fert",
		N_fert_level = fert,
		N_fertilizer = as.numeric(NA),
		P_fertilizer = as.numeric(NA),
		K_fertilizer = as.numeric(NA),
		emergence_days = as.numeric(r$DEm_Est_JDay),
		heading_days = as.numeric(r$HA_50),
		flowering_days = as.numeric(r$DF_50),
		maturity_days = as.numeric(r$DM_50),
		plant_height = as.numeric(r[["Hauteur moyenne plants"]]) * 100,
		striga_count = r2[["No of hills with striga"]][match(r$PlotNo, r2$PlotNo)],
		dmy_residue = as.numeric(r$StoHvYld_M_gPlot),
		dmy_total = as.numeric(r[["Biomass total"]]),
		yield = as.numeric(r$GYld_E_1to5),
		yield_part = "grain",
		yield_moisture = as.numeric(NA),
		yield_isfresh = NA,
		harvest_index = as.numeric(r$HI_C_pct) * 100
	)

	carobiner::write_files(path, meta, d)
}
