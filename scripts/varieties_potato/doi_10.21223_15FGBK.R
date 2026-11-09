# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The description says three repetitions; the Fieldbook and Installation sheet have 5 (15 clones x 5 = 75 plots).
# 2. Only 5 plants per plot were harvested (NOTES 1); yield from 5 plants (0.875 m2) is less precise.

## NOTES
# 1. Each plot had 30 plants (5.25 m2), but only 5 plants per plot were harvested
#    (NPH is at most 5). TTWP, MTWP and TTWPL (= TTWP / NPH) refer to these plants, and
#    TTYNA in the fieldbook (TTWP / 5.25 m2) does not match a sample of 5 plants.
#    Yield is TTWP / (5 x 0.25 m x 0.7 m), the area of the harvested plants (approach
#    confirmed by V. Hualla, CIP). TTYA is not used.
# 2. Daily weather from the "Weather_data" sheet (the same site). Solar radiation (PAR, uE) is not processed.
# 3. Kok Saroy is in Tashkent region; the Minimal sheet gives "Tashkent" for Admin1-3, so only adm1 is kept.
# 4. No fertilizer or irrigation information (summer crop in an arid region; probably irrigated, not stated).
#    Soil_analysis and Crop_management sheets are empty templates (dates only).
# 5. PH (plant height, cm), NS (stems per plant) and PC (canopy cover, %) were recorded on 2008-09-30 (Crop_management).
#    Units of PH and PC are not in the data dictionary (values 42-79 and 60-100). stem_density = NS x plant_density.

## NEW VARIABLES
# (no yield_adj_: the number of plants harvested is the same in all plots, so it equals yield)
# yield_marketable_c1_, yield_marketable_c2_: fresh weight of marketable tubers of size category I
#   and II (kg/ha); MTWCI and MTWCII. The size limits of the categories are not documented.
# tuber_density_marketable_: number of marketable tubers per ha (NMTP)
# tuber_density_c1_, tuber_density_c2_: number of marketable tubers of size category I and II per ha
# Not included because they can be computed from the variables above: per-plant values (TTWPL,
#   MTWPL, TNTPL, NMTPL), non-marketable weight and number, average tuber weight (ATW), TTYNA, MTYNA, MTYA.

carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2008)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2008. In this study, 15 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/15FGBK"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "Enhanced food and income security in SWCA through potato varieties with improved tolerance to abiotic stress (07.7860.5-001.00)",
		# design as reported in the "Installation" sheet
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Cursor Grok 4.7; Claude Opus 5.5",
		carob_date = "2026-11-09",
		carob_completion = 85,
		carob_effort = 1
	)

	## standard CIP fieldbook (sheets "Minimal", "Installation" and "Fieldbook"),
	## processed with the shared function; variety names and parents come from
	## the material list (text file)
	process <- carobiner::get_function("process_cip_lbvars", path, group)

	f <- ff[basename(ff) == "PTYL200807_TASHKENT_HEAT.xls"]
	d <- process(f)

	## Only 5 of the 30 plants of each plot were harvested (NOTES 1).
	## TTWP and MTWP are the tuber weights (kg) of these plants, so yield is
	## expressed per area of the harvested plants (5 x 0.25 m x 0.7 m),
	## not adjusted for missing plants.
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	harvest_area <- 5 * 0.25 * 0.7
	d$yield <- as.numeric(r$TTWP) * 10000 / harvest_area
	d$yield_marketable <- as.numeric(r$MTWP) * 10000 / harvest_area
	d$plot_area <- 5.25
	d$tuber_density <- as.numeric(r$TNTP) * 10000 / harvest_area
	## plant height (cm) measured during the season
	d$plant_height <- as.numeric(r$PH)

	## other plot measurements (NEW VARIABLES); per-plot values per ha of harvested area
	d$yield_marketable_c1_ <- as.numeric(r$MTWCI) * 10000 / harvest_area
	d$yield_marketable_c2_ <- as.numeric(r$MTWCII) * 10000 / harvest_area
	d$tuber_density_marketable_ <- as.numeric(r$NMTP) * 10000 / harvest_area
	d$tuber_density_c1_ <- as.numeric(r$NMTCI) * 10000 / harvest_area
	d$tuber_density_c2_ <- as.numeric(r$NMTCII) * 10000 / harvest_area
	## NS: mean number of stems per plant
	d$stem_density <- as.numeric(r$NS) * d$plant_density
	## PC: plant canopy cover (%)
	d$ground_cover <- as.numeric(r$PC)

	## variety names in upper case in the material list
	d$variety[d$variety == "REICHE"] <- "Reiche"
	d$variety[d$variety == "BASADRE"] <- "Basadre"

	## Minimal sheet: Admin1, 2 and 3 are all "Tashkent" (NOTES 3)
	d$adm1 <- "Toshkent"
	d$location <- "Kok Saroy"
	d$adm2 <- NULL
	d$adm3 <- NULL

	## irrigation and fertilizer not reported in the fieldbook; no dry matter.
	## The shared function adds empty columns for these; they are removed
	d$irrigated <- NA
	d$yield_moisture <- d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- NULL

	## daily weather at the site (NOTES 2)
	w <- carobiner::read.excel(f, sheet="Weather_data")
	wth <- data.frame(
		country = d$country[1],
		location = d$location[1],
		latitude = d$latitude[1],
		longitude = d$longitude[1],
		date = as.character(as.Date(w[["Date of weather observation"]])),
		prec = as.numeric(w[["Rainfall (mm)"]]),
		temp = as.numeric(w[[grep("^Average temperature", names(w))]]),
		tmin = as.numeric(w[[grep("^Minimum temperature", names(w))]]),
		tmax = as.numeric(w[[grep("^Maximum temperature", names(w))]]),
		rhum = as.numeric(w[[grep("^Relative humidity", names(w))]])
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
