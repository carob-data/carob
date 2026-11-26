# R script for "carob"
# license: GPL (>=3)

## NOTES
# Drought trial at Jodhpur, Rajasthan, India. Processed using shared CIP HIDAP helper process_cip_lbvars(), then augmented with irrigation_number (8 vs 5 irrigations), TTYNA yield (t/ha -> kg/ha), plant_height, plants_emerged_, leaf_number_, and densities per 6 m2 plot.
# Planting day in the description is 5 November 2012. End date 2014 in Minimal is the project window, not a harvest date, so harvest_date is omitted.

## ISSUES
# The description calls the layout a strip plot; Installation says Split Plot with Plots in RCBD (SPRCBD).
# No fertilizer rate and no harvest date.

## NOT PROCESSED
# Organoleptic.xlsx (panelist class counts, not plot scores), Summary sheet, Dictionary.xls, NTP (all 50 tubers planted).

## NEW VARIABLES
# plants_emerged_: percent of plants emerged (%).
# leaf_number_: leaves per plant at 60 days.


carob_script <- function(path) {

"
Dataset for: Replication data for: Evaluation of advanced potatoes clones for drought tolerance in arid zone in Rajasthan, India.

Eight CIP-bred potato clones (V1 = 392745.7, V2 = 392780.1, V3 = 397006.18, V4 = 399101.1, V5 = 301029.18, V6 = 380583.8, V7 = 388972.22, V8 = 391580.30) were evaluated along with two control varieties Kufri Pukhraj (early bulking) and Kufri Surya (heat tolerant). Sprouted seed tubers of 40-45 mm size were planted on 5 November 2012 at 60 x 20 cm distance in a plot size of 4.8 × 1.2 m2. The field experiment was laid out in strip plot design with two irrigation regimes: I1 = Normal Irrigation (eight irrigations) and I2 = Deficit irrigation (five irrigations) as horizontal factor and ten clones/varieties as vertical factor. Organoleptic evaluation by panels: to evaluate the appearance, taste and texture of boiled samples by clone and control variety. There were total 58 participants: 40 men and 18 women.
"

	uri <- "doi:10.21223/P3/T41TZD"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Split Plot with Plots in RCBD (SPRCBD)",
		data_type = "experiment",
		treatment_vars = "variety;irrigation_number",
		response_vars = "yield;plant_height;yield_moisture",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0
	)

	proc <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "PTDrought012012_JODHPUR.xlsx"]
	d <- proc(f)
	r <- carobiner::read.excel(f, sheet="Fieldbook")

	factor <- trimws(gsub("\u00a0", " ", as.character(r$FACTOR), fixed=TRUE))
	nph <- as.numeric(r$NPH)
	plot_area <- 6

	d$treatment <- paste(d$variety, factor)
	d$irrigation_number <- ifelse(factor == "normal irrigation", 8L, 5L)
	d$irrigated <- TRUE
	d$yield <- as.numeric(r$TTYNA) * 1000
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$plant_height <- as.numeric(r$PLAHE_EV1)
	d$plants_emerged_ <- as.numeric(r$PPE)
	d$leaf_number_ <- as.numeric(r$LEAF_NUMBER_AT_60_DAYS)
	d$plant_density <- nph / plot_area * 10000
	d$stem_density <- as.numeric(r$SNPP_EV1) * nph / plot_area * 10000
	d$tuber_density <- as.numeric(r$TNTPL) * nph / plot_area * 10000
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$location <- "Jodhpur"
	d$planting_date <- "2012-11-05"
	d$harvest_date <- NULL
	d$on_farm <- FALSE

	carobiner::write_files(path, meta, d)
}
