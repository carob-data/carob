# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The description says 16 clones; the Fieldbook has 13 (39 plots).
# 2. NPE (plants emerged) was not recorded, so plant_density is not available.

## NOTES
# 1. Each plot had 30 plants (5.25 m2), but only 8 plants per plot were harvested
#    (NPH is at most 8). TTWP, MTWP and TTWPL (= TTWP / NPH) refer to these plants, and
#    TTYNA in the fieldbook (TTWP / 5.25 m2) does not match a sample of 8 plants.
#    Yield is TTWP / (8 x 0.25 m x 0.7 m), the area of the harvested plants (approach
#    confirmed by V. Hualla, CIP). TTYA is not used.
# 2. Daily weather from the "Weather_data" sheet (the same site). Solar radiation (PAR, uE) is not processed.
# 3. Kok Saroy is in Tashkent region; the Minimal sheet gives "Tashkent" for Admin1-3, so only adm1 is kept.
# 4. No fertilizer or irrigation information (summer crop in an arid region; probably irrigated, not stated).
#    Soil_analysis and Crop_management sheets are empty templates (dates only).
# 5. PH (plant height, cm), NS (stems per plant) and PC (canopy cover, %) were recorded on 2009-10-10 (Crop_management).

## NEW VARIABLES
# stems_per_plant_: mean number of main stems per plant (NS); plants emerged not recorded,
#   so stem_density cannot be computed
# Not included (as in doi:10.21223/15FGBK): yield adjusted for missing plants (TTYA, MTYA;
#   plants planted and harvested are recorded instead) and the weight and number of marketable
#   tubers by size category (MTWCI, MTWCII, NMTCI, NMTCII; limits not documented).
# Not included because they can be computed from the variables above: per-plant values (TTWPL,
#   MTWPL, TNTPL, NMTPL), non-marketable weight and number, average tuber weight (ATW), TTYNA, MTYNA, MTYA.

carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2009)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2009. In this study, 16 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/NKYYIW"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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

	f <- ff[basename(ff) == "PTYL200907_TASHKENT_HEAT.xls"]
	d <- process(f)

	## Only 8 of the 30 plants of each plot were harvested (NOTES 1).
	## TTWP and MTWP are the tuber weights (kg) of these plants, so yield is
	## expressed per area of the harvested plants (8 x 0.25 m x 0.7 m),
	## not adjusted for missing plants.
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	harvest_area <- 8 * 0.25 * 0.7
	d$yield <- as.numeric(r$TTWP) * 10000 / harvest_area
	d$yield_marketable <- as.numeric(r$MTWP) * 10000 / harvest_area
	d$plot_area <- 5.25
	d$tuber_density <- as.numeric(r$TNTP) * 10000 / harvest_area
	## plant height (cm) measured during the season
	d$plant_height <- as.numeric(r$PH)

	## other plot measurements (NEW VARIABLES); per-plot values per ha of harvested area
	#d$yield_marketable_c1_ <- as.numeric(r$MTWCI) * 10000 / harvest_area
	#d$yield_marketable_c2_ <- as.numeric(r$MTWCII) * 10000 / harvest_area
	d$tuber_density_marketable <- as.numeric(r$NMTP) * 10000 / harvest_area
	#d$tuber_density_c1_ <- as.numeric(r$NMTCI) * 10000 / harvest_area
	#d$tuber_density_c2_ <- as.numeric(r$NMTCII) * 10000 / harvest_area
	d$stems_per_plant_ <- as.numeric(r$NS)
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
