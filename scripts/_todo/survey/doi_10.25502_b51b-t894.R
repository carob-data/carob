# R script for "carob"
# license: GPL (>=3)

## NOTES
# ISFM end-of-project household survey, Eastern Province, Zambia (Petauke, Katete and Chipata are named in the abstract; the microdata have no district or GPS column).
# Production is the wide block hh_e10*: soybean and maize area in hectares and production in kilograms for the 2015/16 (_16) and 2016/17 (_17) seasons (metadata descriptions). Reshaped to one row per household x crop x season.
# Yield (kg/ha) = kilograms / area where area > 0. planting_date is the first year of the season label (2015 for _16, 2016 for _17).
# hh_e10*fert_* is kilograms of mineral fertiliser (metadata), kept as fertilizer_product_kg_. Product flags hh_e14* use 1 = yes. 0 and 2 are not treated as a named product (2 is the most common code and is likely "not applicable"). Elemental N/P/K are not calculated because a row can list more than one product against one kilogram total.
# Rows with area, production and fertiliser all zero or missing are omitted (the crop was not reported for that season).

## ISSUES
# No coordinates and no household district. Georeference to Eastern Province, Zambia (Petauke, Katete, Chipata). adm1 is set to Eastern from the project description, not from a column.
# Confirm that hh_e10*yield_* is total kilograms (the label says "Yield (in Kgs)") rather than kg/ha. Area outliers reach 1000 ha (soy) and 4000 ha (maize).
# Fertiliser product codes: confirm 1 = yes. soymix and "other" have no nutrient analysis. Basal/top-dress quantities hh_e13* and hh_f3* are a second fertiliser block and were not added, to avoid double counting.
# hh_e1* area uses a unit code (hh_e1mzunit) that is not documented, so that block was not used.
# No publication DOI.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude; unknown fertilizer_product_kg_; invalid fertilizer_type other, soymix; high yield maize 31707 and soybean 20000 kg/ha; NA yield.

## NEW VARIABLES
# fertilizer_product_kg_: mineral fertiliser amount in kilograms (metadata), not split into N/P/K and not confirmed as kg/ha

## NOT PROCESSED
# Demographics hh18-hh25, land tenure hh_d*, seed sources and payments hh_e5* to hh_e7*, cultivation method and "others" hh_e10*cultmethod and hh_e10*others, reasons hh_e11*, preference hh_e16*, rotation module hh_f*, harvest use hh_f4*, group and training hh_g* hh_h*, decisions and income hh_i*, crop sales and purchases hh_j1* hh_j2* (prices and transport), livestock hh_j3*, assets hh_j5*, farmer group hh_k*.
# hh_e8mzharv / hh_e8soyharv: another harvest field without a year label in the column name.
# hh_e13* and hh_f3* fertiliser quantities (see ISSUES).
# Metadata file, aside from the descriptions used above.


carob_script <- function(path) {

"Integrated Soil Fertility Managent (ISFM) end of project evaluation survey

The integrated soil fertility management (ISFM) framework provides an opportunity to select sustainable practices and customize them to local farmer conditions. Integrated soil fertility management (ISFM) can be defined as “the application of a range of technologies that support key soil processes and optimal management of organic resources, mineral fertility inputs and soil organic matter that drive productivity. The Scaling Out Integrated Soil Fertility Management Technologies in Petauke, Katete and Chipata districts of Eastern province of Zambia was an intervention by Development Aid from People to People (DAPP) and International Institute of Tropical Agriculture (IITA) with support from AGRA which commenced in December 2014. The project aimed at contributing to increased and sustained agricultural productivity, food and income security of 20,000 smallholder farmers located in Petauke, Katete and Chipata districts of Eastern Province of Zambia  through dissemination and scaling out the uptake of improved and proven ISFM practices using a value-chain approach linking ISFM with better access to markets and credit.   A secondary target group includes local micro-entrepreneurs and agro-dealers. The project is focusing on key staple food crop (maize) to be rotated with soybeans.
"

	uri <- "doi:10.25502/b51b-t894"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ISFM",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	r <- read.csv(ff[basename(ff) == "20190109aao_iita_isfm_end-project-evaluation-survey.csv"], stringsAsFactors=FALSE)

	prod_type <- function(can, dcomp, urea, mix, other) {
		n <- length(can)
		out <- rep(NA_character_, n)
		for (i in seq_len(n)) {
			lab <- character()
			if (isTRUE(can[i] == 1)) lab <- c(lab, "CAN")
			if (isTRUE(dcomp[i] == 1)) lab <- c(lab, "D-compound")
			if (isTRUE(urea[i] == 1)) lab <- c(lab, "urea")
			if (isTRUE(mix[i] == 1)) lab <- c(lab, "soymix")
			if (isTRUE(other[i] == 1)) lab <- c(lab, "other")
			if (length(lab)) out[i] <- paste(lab, collapse=";")
		}
		out
	}

	one <- function(crop, year, area, prod, fert, ftype) {
		keep <- (!is.na(area) & area > 0) | (!is.na(prod) & prod > 0) | (!is.na(fert) & fert > 0)
		data.frame(
			hhid = as.character(r$ID)[keep],
			crop = crop,
			planting_date = year,
			field_size = area[keep],
			yield = ifelse(!is.na(area[keep]) & area[keep] > 0, prod[keep] / area[keep], NA_real_),
			fertilizer_product_kg_ = fert[keep],
			fertilizer_type = ftype[keep],
			stringsAsFactors = FALSE
		)
	}

	soy_type <- prod_type(r$hh_e14can_soy, r$hh_e14dcomp_soy, r$hh_e14urea_soy, r$hh_e14soymix_soy, r$hh_e14other_soy)
	mz_type <- prod_type(r$hh_e14can_mz, r$hh_e14dcomp_mz, r$hh_e14urea_mz, r$hh_e14soymix_mz, r$hh_e14other_mz)

	d <- rbind(
		one("soybean", "2015", r$hh_e10soyarea_16, r$hh_e10soyyield_16, r$hh_e10soyfert_16, soy_type),
		one("soybean", "2016", r$hh_e10soyarea_17, r$hh_e10soyyield_17, r$hh_e10soyfert_17, soy_type),
		one("maize", "2015", r$hh_e10mzarea_16, r$hh_e10mzyield_16, r$hh_e10mzfert_16, mz_type),
		one("maize", "2016", r$hh_e10mzarea_17, r$hh_e10mzyield_17, r$hh_e10mzfert_17, mz_type)
	)
	d$yield_part <- ifelse(d$crop == "maize", "grain", "seed")
	d$country <- "Zambia"
	d$adm1 <- "Eastern"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
