# R script for "carob"
# license: GPL (>=3)

## ISSUES
# column names are inconsistent between the three site files for the same variable e.g. "Total_dry_matter_kg_per_ha" (buk.csv) vs "Total_dry_matter_g_per_ha" (tofa.csv, tudun-wada.csv)and "HI_ha" (buk.csv, tudun-wada.csv) vs "HI" (tofa.csv) and "Date_to_50_percent_Tasseling" (buk.csv) vs "Date_of_50_percent_Tasseling" (tofa.csv) vs "Days_to_50_percent_anthesis" (tudun-wada.csv, a different trait: anthesis, not tasseling date)

# "Total_dry_matter_g_per_ha" values in tofa.csv and tudun-wada.csv are in kg despite the "_g_per_ha" name (they are ~10x the corresponding *_g_per_m2 columns

# coordinates are not provided in the raw data; longitude/latitude were estimated from cgiar.org location pages (buk.csv) or via carobiner::adm_pointRadius() (tofa.csv, tudun-wada.csv).

# this experiment was conducted in 2023 at three locations in Kano state, Nigeria. the experiment was a split plot design with sowing window as the main plot and varieties as the sub plot. Six varieties representing early, medium and late maturity groups were used. six sowing windows with a week interval were used in the experiment.


carob_script <- function(path) {

"
EiA SAA Usecase Nigeria, maize sowing window validation experiment dataset, 2023.

The dataset was obtained from 3 locations (BUK, TOFA, and Tudunwada) in Kano state, Nigeria. The dataset consists of a split plot experiment with sowing window as the main plot and varieties as the sub plot. 6 varieties were used for the experiment representing early, medium and late maturity groups. Six sowing windows with a week interval was used in the experiment.
"

	uri <- "doi:10.25502/4473-kb25/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "EiA",
		design = "split plot experiment with sowing window as the main plot and varieties as the sub plot",
		data_type = "experiment",
		treatment_vars = "planting_date;variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-08-26",
		carob_completion = 70,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "buk.csv"]
	f2 <- ff[basename(ff) == "tofa.csv"]
	f3 <- ff[basename(ff) == "tudun-wada.csv"]

	r1 <- read.csv(f1, na.strings = c("NA", "."))
	r2 <- read.csv(f2, na.strings = c("NA", "."))
	r3 <- read.csv(f3, na.strings = c("NA", "."))

	d1 <- data.frame(
		trial_id = "buk",
		plot_id = as.character(r1[["Plot_no"]]),
		treatment = r1[["Planting_window"]],
		planting_date = r1[["Date_of_planting"]],
		variety = r1[["Varieties"]],
		maturity_days = r1[["Days_to_95_percent_physiological_maturity"]],
		yield = r1[["Grain_yield_kg_per_ha"]],
		yield_moisture = r1[["Grains_moisture_percent"]],
		dw_leaves = r1[["Total_leaf_dry_wt_g_Quadrat"]],
		dw_stems = r1[["Total_stem_dry_wt_g_Quadrat"]],
		dmy_total = r1[["Total_dry_matter_kg_per_ha"]],
		tassling_days = r1[["Date_to_50_percent_Tasseling"]],
		silking_days = r1[["Date_of_50_percent_Silking"]],
		harvest_index = r1[["HI_ha"]],

# coordinates obtained from https://www.cgiar.org/locations/nigeria-icrisat-kano-research-field-buk
		longitude = 8.4173,
		latitude = 11.9756
	)

	d2 <- data.frame(
		trial_id = "tofa",
		plot_id = as.character(r2[["Plot_no"]]),
		treatment = r2[["Planting_window"]],
		planting_date = r2[["Date_of_planting"]],
		variety = r2[["Varieties"]],
		maturity_days = r2[["Days_to_95_percent_physiological_maturity"]],
		yield = r2[["Grain_yield_kg_per_ha"]],
		yield_moisture = r2[["Grains_moisture_percent"]],
		dw_leaves = r2[["Total_leaf_dry_wt_g_Quadrat"]],
		dw_stems = r2[["Total_stem_dry_wt_g_Quadrat"]],
		dmy_total = r2[["Total_dry_matter_g_per_ha"]],
		tassling_days = r2[["Date_of_50_percent_Tasseling"]],
		silking_days = r2[["Date_of_50_percent_Silking"]],
		harvest_index = r2[["HI"]],
# coordinates obtained via carobiner::adm_pointRadius
		longitude = 8.3107,
		latitude = 11.9957
	)

	d3 <- data.frame(
		trial_id = "tudunwada",
		plot_id = as.character(r3[["Plot_no"]]),
		treatment = r3[["Planting_window"]],
		planting_date = r3[["Date_of_planting"]],
		variety = r3[["Varieties"]],
		maturity_days = r3[["Days_to_95_percent_physiological_maturity"]],
		yield = r3[["Grain_yield_kg_per_ha"]],
		yield_moisture = r3[["Grains_moisture_percent"]],
		dw_leaves = r3[["Total_leaf_dry_wt_g_Quadrat"]],
		dw_stems = r3[["Total_stem_dry_wt_g_Quadrat"]],
		dmy_total = r3[["Total_dry_matter_g_per_ha"]],
		tassling_days = r3[["Days_to_50_percent_anthesis"]],
		silking_days = r3[["Days_to_50_percent_Silking"]],
		harvest_index = r3[["HI_ha"]], 
# coordinates obtained via carobiner::adm_pointRadius
		longitude = 8.5570,
		latitude = 11.2511
	)

	d <- rbind(d1, d2, d3)
	d$country <- "Nigeria"
	d$planting_date <- format(as.Date(d$planting_date, format = "%d/%m/%Y"), "%Y-%m-%d")
	d$harvest_date <- as.character(as.Date(d$planting_date) + d$maturity_days)
	d$variety <- gsub("^20.. ", "", d$variety)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA # not stated but likely rainfed during months 06-09 (jun - sep)

    d$crop <- "maize"
	d$geo_from_source <- FALSE
	d$yield_part <- "grain"

	carobiner::write_files(path, meta, d)
}


