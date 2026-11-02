# R script for "carob"
# license: GPL (>=3)

## NOTES
# SLU OrganicYieldsUP winter wheat variety trials 2015-2024. Yield from
# harvest_variety_view (t/ha). Fertilisation aggregated per site-year before
# merge so yield rows are not duplicated. Biofer 10-3-1 treated as 10% N,
# 3% P2O5, 1% K2O. trial_id is location × year, not per variety.

## ISSUES
# Script needs DBI and RSQLite. date_harvest is empty in the view.
# genetic_type is empty so variety_type not set. Ekoväx 8-3-5-3 oxide vs
# elemental not confirmed (OM_amount only). Town coordinates estimated.
# Daily weather (23k rows) included as wth. write_files: missing
# yield_moisture, harvest_date.


carob_script <- function(path) {

"
Long term trial on winter wheat

This dataset is part of the database compiled as an outcome of Work Area 1 in project OrganicYieldsUP. Variable definitions can be found here:  https://doi.org/10.5281/zenodo.15276082

Data on winter wheat from a long term trial on cereal production for the period 2015-2024 (available for researchers on request). The dataset comprises yield information on 35 varieties, fertilisation operations in eight localities (not all present all years: Tjärby, Berg, Uppsala, Malmö, Tygelsjö, Älvestad, Borrby, Tomelilla).
"

	uri <- "doi:10.5281/zenodo.17104684"
	group <- "varieties_wheat"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=8, minor=NA,
		data_organization = "SLU",
		publication = NA,
		project = "OrganicYieldsUP",
		design = "variety trial",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "SLU_variety_trial_Winter_wheat_WHE_zenodo_2025_09_18.sqlite"]
	con <- DBI::dbConnect(RSQLite::SQLite(), f)
	hv <- DBI::dbReadTable(con, "harvest_variety_view")
	fert <- DBI::dbReadTable(con, "farm_op_fertilisation_view")
	plant <- DBI::dbReadTable(con, "farm_operation_seedingplanting_tbl")
	qual <- DBI::dbReadTable(con, "harvest_quality_ind_tbl")
	w <- DBI::dbReadTable(con, "weather_data_daily_tbl")
	DBI::dbDisconnect(con)

	d <- data.frame(
		site_id = hv$fk_site_id,
		spatial_id = hv$pk_spatial_design_id,
		country = hv$country,
		location = trimws(hv$Town),
		crop = "wheat",
		variety = trimws(hv$variety_name),
		treatment = trimws(hv$variety_name),
		yield_part = "grain",
		yield = hv$yield_biomass * 1000,
		planting_date = NA_character_,
		trial_id = paste(hv$Town, hv$year, sep="_")
	)

	# grain protein from quality table
	prot <- qual[qual$quality_indicator == "crude protein content (%)", c("fk_spatial_design_id", "qi_value")]
	names(prot) <- c("spatial_id", "grain_protein")
	d <- merge(d, prot, by="spatial_id", all.x=TRUE)

	# Biofer 10-3-1: 10-3-1 as N-P2O5-K2O; quantity is t DM ha-1
	com <- as.character(fert$comment_operation)
	com[is.na(com)] <- ""
	is_biofer <- grepl("Biofer 10-3-1", com, fixed=TRUE)
	is_vinass <- grepl("Vinass 4-0-4", com, fixed=TRUE)
	qty <- as.numeric(fert$fertiliser_quantity)
	n_app <- ifelse(is_biofer, qty * 1000 * 0.10,
		ifelse(is_vinass, qty * 1000 * 0.04, NA_real_))
	p_app <- ifelse(is_biofer, qty * 1000 * 0.03 / 2.29, NA_real_)
	k_app <- ifelse(is_biofer, qty * 1000 * 0.01 / 1.2051,
		ifelse(is_vinass, qty * 1000 * 0.04 / 1.2051, NA_real_))
	om <- ifelse(!is.na(qty), qty * 1000, NA_real_)
	fa <- data.frame(
		site_id = fert$fk_site_id,
		N_fertilizer = n_app,
		P_fertilizer = p_app,
		K_fertilizer = k_app,
		OM_amount = om
	)
	# sum applications per site-year so yield is not duplicated
	sites <- unique(fa$site_id)
	fa2 <- data.frame(site_id=sites)
	for (v in c("N_fertilizer", "P_fertilizer", "K_fertilizer", "OM_amount")) {
		fa2[[v]] <- sapply(sites, function(s) {
			x <- fa[[v]][fa$site_id == s]
			if (all(is.na(x))) NA_real_ else sum(x, na.rm=TRUE)
		})
	}
	d <- merge(d, fa2, by="site_id", all.x=TRUE)
	d$OM_used <- !is.na(d$OM_amount) & d$OM_amount > 0

	plt <- plant[!is.na(plant$operation_date), c("fk_site_id", "operation_date")]
	plt <- plt[!duplicated(plt$fk_site_id), ]
	names(plt) <- c("site_id", "planting_date")
	plt$planting_date <- as.character(as.Date(plt$planting_date))
	d$planting_date <- NULL
	d <- merge(d, plt, by="site_id", all.x=TRUE)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$site_id <- NULL
	d$spatial_id <- NULL

	geo <- data.frame(
		location = c("Tjärby", "Berg", "Uppsala", "Malmö", "Tygelsjö", "Älvestad", "Borrby", "Tomelilla"),
		longitude = c(12.93, 15.90, 17.639, 13.000, 12.967, 15.267, 14.180, 13.955),
		latitude = c(56.633, 58.317, 59.858, 55.606, 55.517, 58.583, 55.457, 55.543),
		geo_uncertainty = c(5000, 25000, 8000, 8000, 3000, 5000, 2000, 3000),
		geo_source = "estimated from town name"
	)
	d <- merge(d, geo, by="location", all.x=TRUE)
	d$geo_from_source <- FALSE

	wth <- data.frame(
		date = as.character(as.Date(w$weather_date)),
		location = sub("^WHE_", "", w$weather_source_id),
		temp = w$temp_mean,
		tmin = w$temp_min,
		tmax = w$temp_max,
		prec = w$daily_precipitation,
		rhum = w$humidity_mean
	)
	wth <- merge(wth, geo, by="location", all.x=TRUE)
	wth$country <- "Sweden"
	wth$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d, wth=wth)
}
