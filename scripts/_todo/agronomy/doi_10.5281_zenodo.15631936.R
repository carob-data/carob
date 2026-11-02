# R script for "carob"

## NOTES
# OrganicYieldsUP sqlite: harvest_view (96 plot-years) at Trenthorst, Thünen Institute of Organic Farming.
# Two RCBD trials 2019-2021: CF1 undersown clover-grass; CF4 organic materials (champost, biogas manure, wood chips).
# yield_biomass is t/ha; residue_moisture is 0 so treated as dry matter. No synthetic N: N_fertilizer = 0.
# N_organic from slurry volume (m3/ha) times n_total_content (treated as kg N/m3). Extra packages: RSQLite, DBI.

## ISSUES
# Requires DBI + RSQLite.
# Coordinates not in sqlite (postal 23847 Trenthorst); hardcoded 53.786, 10.517 (geo_from_source=FALSE). Confirm.
# n_total_content units assumed kg N per m3; confirm against codebook doi:10.5281/zenodo.15276082.
# champost mapped to compost; biogas manure to liquid manure; wood chips has no OM vocab term (OM_type left as source string).
# Undersown species coded as clover (clover-grass in comments).
# Daily weather (Lübeck-Blankensee) and soil measurements not processed.
# seeding_planting_rate / variety from seeding table not fully joined (variety via harvest_view fk).
# write_files: Thünen not in org vocab; OM_type missing for undersown/control treatments (expected).

carob_script <- function(path) {

"
Yields and field management data of two adjacent field trials with different inputs of organic materials at the Thünen Institute of Organic Farming in North Germany

The dataset contains yields and field management data of two adjacent randomised block trials from 2019 to 2021 at fixed plot locations at the research station of the Thünen-Institute of Organic Farming in North Germany. Different techniques to enrich soil organic matter where applied in field beans, oats and silage maize (trial CF1) or in spring wheat, oats and silage maize (trial CF4). Both field trials followed spring wheat. In trial CF 1 undersown clover grass and harvest residues were incorporated or not. In trial CF4 different organic materials were applied to the soil before cultivation and seeding in spring. This dataset is part of the database compiled as an outcome of Work Area 1 in project OrganicYieldsUP. Variable definitions can be found here: https://doi.org/10.5281/zenodo.15276082
"

	uri <- "doi:10.5281/zenodo.15631936"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "Thünen",
		publication = NA,
		project = "OrganicYieldsUP",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "OM_type;cover_crop_used;N_organic",
		response_vars = "yield;dmy_residue",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 1.5
	)

	f <- ff[grepl("\\.sqlite$", ff)]
	con <- DBI::dbConnect(RSQLite::SQLite(), f)
	h <- DBI::dbReadTable(con, "harvest_view")
	fert <- DBI::dbReadTable(con, "farm_operation_fertilisation_tbl")
	seed <- DBI::dbReadTable(con, "farm_operation_seedingplanting_tbl")
	DBI::dbDisconnect(con)

	# Organic N from slurry events (quantity m3/ha * content kg/m3)
	fert$n_kg <- ifelse(!is.na(fert$fertiliser_quantity) & !is.na(fert$n_total_content),
		fert$fertiliser_quantity * fert$n_total_content, 0)
	nagg <- aggregate(list(N_organic=fert$n_kg),
		list(fk_treatment_id=fert$fk_treatment_id, year=fert$year, crop=fert$crop), sum, na.rm=TRUE)

	# Planting dates
	seed$operation_date <- as.character(seed$operation_date)
	skey <- paste(seed$fk_treatment_id, seed$year, seed$crop)
	# one seeding row per treatment-year-crop expected

	crop <- h$crop_species
	crop <- gsub("Broad and field beans", "faba bean", crop)
	crop <- gsub("Spring wheat", "wheat", crop)
	crop <- gsub("Oats", "oats", crop)
	crop <- gsub("Silage maize", "maize", crop)

	yield_part <- ifelse(h$marketable_yield_component == "grain", "grain", "aboveground biomass")

	d <- data.frame(
		trial_id = paste(h$fk_study_id, h$year, sep="_"),
		rep = as.integer(h$plot_nr),
		treatment = trimws(h$unique_treatment_description),
		crop = crop,
		variety = h$fk_variety_id,
		planting_date = seed$operation_date[match(paste(h$fk_treatment_id, h$year, h$crop_species), skey)],
		harvest_date = as.character(h$date_harvest),
		yield = h$yield_biomass * 1000,
		dmy_residue = h$residue_biomass * 1000,
		yield_part = yield_part,
		yield_moisture = 0,
		N_fertilizer = 0,
		P_fertilizer = 0,
		K_fertilizer = 0,
		country = h$country,
		location = h$Town,
		# Trenthorst research station (not in sqlite)
		latitude = 53.786,
		longitude = 10.517,
		geo_from_source = FALSE,
		soil_texture = "sandy loam",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$N_organic <- nagg$N_organic[match(paste(h$fk_treatment_id, h$year, h$crop_species),
		paste(nagg$fk_treatment_id, nagg$year, nagg$crop))]
	d$N_organic[is.na(d$N_organic)] <- 0

	d$cover_crop_used <- grepl("undersown crops", d$treatment) & d$treatment != "without undersown crops"
	d$cover_crop <- ifelse(d$cover_crop_used, "clover", NA)

	d$OM_used <- d$treatment %in% c("champost", "biogas manure", "wood chips") | d$N_organic > 0
	d$OM_type <- NA_character_
	d$OM_type[d$treatment == "champost"] <- "compost"
	d$OM_type[d$treatment == "biogas manure"] <- "liquid manure"
	d$OM_type[d$treatment == "without organic fertilisers"] <- NA
	d$OM_used[d$treatment == "without organic fertilisers"] <- FALSE
	# wood chips: no matching OM term in vocab
	d$OM_type[d$treatment == "wood chips"] <- NA

	carobiner::write_files(path, meta, d)
}
