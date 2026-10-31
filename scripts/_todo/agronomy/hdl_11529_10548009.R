# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm validation of zero tillage against farmer practice in Purnea, rabi 2015-16 and 2016-17.
# The column ctry contains the state Bihar, not a country. dist is the district Purnea. geographicCoverage is empty.
# gy and bm are t/ha. Harvest index equals gy/bm. Yield is stored as kg/ha. Moisture is not reported.
# ZT is zero tillage (land_prep_method "none"). FP is the farmers' practice the description calls CT, stored as conventional.
# irri-cm is centimetres of irrigation, stored as mm.
# The 2016-17 column ds is an Excel date serial (origin 1899-12-30). The 2015-16 file has no sowing date.
# Node spellings differ by year: PuraniGarel/Puranigarel, Dogachhi/Dogachi, Tikapatti/Tikapati.

## ISSUES
# No coordinates. No country column.
# Fertilizer rates are not in this summary. Energy, cost, and emission columns are calculated and not stored.
# Biomass is not labeled fresh or dry.
# write_files: missing variables: yield_moisture, country, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: biomass_kg_ha_. contributor:

## NEW VARIABLES
# biomass_kg_ha_: bm in t/ha, stored as kg/ha. Fresh or dry is not stated.

## NOT PROCESSED
# Tillage and irrigation hours, fuel, labor, costs, gross return, BCR, and energy columns.
# Diesel and total emission columns in the 2016-17 file. Farmer names.


carob_script <- function(path) {

"
6.3-Rabi (winter) crops-all nodes- Validation trials -Purnea-Bihar

Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548009"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Validation trials, Purnea, Bihar",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
	)

	one <- function(f) {
		r <- as.data.frame(carobiner::read.excel(f, sheet="Sheet1"))
		r <- r[!is.na(r$gy), ]
		loc <- tolower(trimws(r$node))
		loc[loc %in% c("puranigarel", "puranigare")] <- "puranigarel"
		loc[loc == "dogachhi"] <- "dogachi"
		loc[loc == "tikapatti"] <- "tikapati"
		loc <- carobiner::fix_name(loc, "title")

		crop <- tolower(trimws(r$crop))
		crop[crop == "rabi maize"] <- "maize"
		rot <- ifelse(r$sys == "R-W", "rice;wheat", ifelse(r$sys == "R-M", "rice;maize", NA))
		prep <- ifelse(r$trt == "ZT", "none", ifelse(r$trt == "FP", "conventional", NA))

		sown <- rep(NA_character_, nrow(r))
		if ("ds" %in% names(r)) {
			sown <- as.character(as.Date(as.numeric(r$ds), origin="1899-12-30"))
		}
		hi <- as.numeric(r$hi)
		hi <- ifelse(!is.na(hi) & hi <= 1, hi * 100, hi)
		water <- as.numeric(r[["irri-cm"]]) * 10

		data.frame(
			adm1 = "Bihar",
			adm2 = carobiner::fix_name(r$dist, "title"),
			location = loc,
			season = "rabi",
			trial_id = paste(loc, r$yr),
			plot_id = paste(loc, r[["Far-id"]], crop, r$trt),
			treatment = r$trt,
			land_prep_method = prep,
			crop = crop,
			crop_rotation = rot,
			yield_part = "grain",
			maturity_days = as.numeric(r[["crop-duration"]]),
			planting_date = sown,
			yield = as.numeric(r$gy) * 1000,
			harvest_index = hi,
			biomass_kg_ha_ = as.numeric(r$bm) * 1000,
			irrigation_amount = water,
			irrigated = !is.na(water) & water > 0,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	d <- lapply(ff, one)
	d <- do.call(rbind, d)
	d$seq <- ave(seq_len(nrow(d)), d$plot_id, FUN=seq_along)
	d$plot_id <- paste(d$plot_id, d$seq)
	d$seq <- NULL
	carobiner::write_files(path, meta, d)
}
