# R script for "carob"
# license: GPL (>=3)

## NOTES
# Four Kenya maize household surveys, 1992-2013. Geographic coverage is Kenya. The file has no coordinates.
# MAIZE_Y_KH is maize yield in kg/ha. MAIZE_A_HA is maize area in ha.
# TOTAL_PR is labeled total annual precipitation in mm. One elevation is 19080 m and some annual totals exceed 2500 mm. Those source values were left as recorded.
# Sublocation is stored as location. It is not treated as an administrative unit.

## ISSUES
# write_files: missing variables: latitude, longitude out of bounds: elevation (11, 19080), rain (366.12, 7951.944) contributor:

## NOT PROCESSED
# Seasonally weighted yield, production, area, and fertilizer. Those columns are weighted estimates.
# Survey weights, variety-area percentages, food-insecurity scores, credit, extension, education, and livestock.
# Fertilizer dose is kg/ha of product, not elemental N.


carob_script <- function(path) {

"
Varietal turn-over and their effect on yield and food security – evidence from 20 years of household surveys in Kenya

The International Maize and Wheat Improvement Centre (CIMMYT), in collaboration with the Kenya Agriculture and Livestock Research Organization (KALRO) (previously the Kenya Agricultural Research Institute or KARI), conducted four nationally representative household surveys in the major maize growing areas during the last 30 years. All surveys used the same two-stage stratified design with maize production zones or agroecological zones (AEZs) as strata, census clusters or sublocations as primary sampling units, and maize-growing households as secondary sampling units.
"

	uri <- "hdl:11529/10548862"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Kenya maize household surveys",
		design = "two-stage stratified sample",
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("\\.sav$", basename(ff), ignore.case=TRUE)]
	r <- haven::read_sav(f)
	txt <- function(x) {
		z <- trimws(as.character(haven::as_factor(x)))
		z[z %in% c("", "NA")] <- NA_character_
		z
	}
	num <- function(x) {
		as.numeric(haven::zap_labels(x))
	}

	hh <- txt(r$UNID)
	yr <- txt(r$YEAR)
	loc <- carobiner::fix_name(txt(r$SUBLOCAT), "title")
	d <- data.frame(
		country = "Kenya",
		location = loc,
		hhid = hh,
		trial_id = paste(hh, yr),
		crop = "maize",
		yield = num(r$MAIZE_Y_KH),
		yield_part = "grain",
		field_size = num(r$MAIZE_A_HA),
		farmland_owned = num(r$LAND_OWN),
		hh_size = num(r$HH_SIZE),
		elevation = num(r$ELEVATIO),
		rain = num(r$TOTAL_PR),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d <- d[!is.na(d$hhid), ]
	carobiner::write_files(path, meta, d)
}
