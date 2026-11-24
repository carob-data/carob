# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The description says 16 clones; the Fieldbook has 21 (63 plots).
# 2. Planting date 2010-07-01 (Minimal sheet); the weather records start on 2010-07-12. All Crop_management dates are 2010-07-01 (template).
# 3. Installation sheet: 10 plants per plot and 2.7 m2, but NTP (tubers planted) is 30; we assume 30 plants (8.1 m2) and a harvest of 10 plants.

## NOTES
# 1. Each plot had 30 plants (8.1 m2), but only 10 plants per plot were harvested
#    (NPH is at most 10). TTWP, MTWP and TTWPL (= TTWP / NPH) refer to these plants, and
#    TTYNA in the fieldbook (TTWP / 2.7 m2) does not match a sample of 10 plants (it does for the 2.7 m2 used here).
#    Yield is TTWP / (10 x 0.3 m x 0.9 m), the area of the harvested plants (approach
#    confirmed by V. Hualla, CIP). TTYA is not used.
# 2. Daily weather from the "Weather_data" sheet (the same site). Solar radiation (PAR, uE) is not processed.
# 3. Kok Saroy is in Tashkent region; the Minimal sheet gives "Tashkent" for Admin1-3, so only adm1 is kept.
# 4. No fertilizer or irrigation information (summer crop in an arid region; probably irrigated, not stated).
#    Soil_analysis and Crop_management sheets are empty templates (dates only).
# 5. The material list has Windows-1252 characters (non-breaking space after "Serrana-INTA"); converted to UTF-8.

## NEW VARIABLES
# Not included (as in doi:10.21223/15FGBK): yield adjusted for missing plants (TTYA, MTYA;
#   plants planted and harvested are recorded instead) and the weight and number of marketable
#   tubers by size category (MTWCI, MTWCII, NMTCI, NMTCII; limits not documented).
# Not included because they can be computed from the variables above: per-plant values (TTWPL,
#   MTWPL, TNTPL, NMTPL), non-marketable weight and number, average tuber weight (ATW), TTYNA, MTYNA, MTYA.

carob_script <- function(path) {

"
Dataset for: Evaluation of Lowland Tropic Virus-Resistant (LTVR) Potato Varieties in Uzbekistan (2010)

In this trial, 16 potato clones were assessed using a randomized complete block design (RCBD) with three repetitions during 2010 in Tashkent, Uzbekistan. These clones belong to the Lowland Tropic Virus Resistant Population (LTVR).
"

	uri <- "doi:10.21223/XB6H0A"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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

	f <- ff[basename(ff) == "PTYL201007_TASHKENT-LTVR.xls"]
	d <- process(f)

	## Only 10 of the 30 plants of each plot were harvested (NOTES 1).
	## TTWP and MTWP are the tuber weights (kg) of these plants, so yield is
	## expressed per area of the harvested plants (10 x 0.3 m x 0.9 m),
	## not adjusted for missing plants.
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	harvest_area <- 10 * 0.3 * 0.9
	d$yield <- as.numeric(r$TTWP) * 10000 / harvest_area
	d$yield_marketable <- as.numeric(r$MTWP) * 10000 / harvest_area
	d$plot_area <- 8.1
	## the Installation sheet gives the harvested area (2.7 m2) as plot size
	d$plant_density <- as.numeric(r$NPE) * 10000 / 8.1

	## other plot measurements (NEW VARIABLES); per-plot values per ha of harvested area
	## yield adjusted for missing plants: tuber weight per harvested plant x plants planted per ha
	#d$yield_adj_ <- as.numeric(r$TTWP) / as.numeric(r$NPH) * d$seed_density
	#d$yield_marketable_c1_ <- as.numeric(r$MTWCI) * 10000 / harvest_area
	#d$yield_marketable_c2_ <- as.numeric(r$MTWCII) * 10000 / harvest_area

	## the material list has Windows-1252 characters (e.g. "Craig's Defiance",
	## non-breaking spaces); convert to UTF-8
	i <- sapply(d, is.character)
	d[i] <- lapply(d[i], function(x) {
		x <- iconv(x, "CP1252", "UTF-8")
		x <- gsub("\u00a0", " ", x)
		trimws(gsub("\u2019", "'", x))
	})

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
