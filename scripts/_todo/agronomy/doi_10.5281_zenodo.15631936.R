# R script for "carob"
# license: GPL (>=3)

## NOTES
# OrganicYieldsUP SQLite database (TI_TI-SOC-Enrichment_2025_06_13.sqlite) for two adjacent RCBD trials (CF1 undersown clover-grass; CF4 organic amendments: champost, biogas manure, wood chips) at Thuenen Institute of Organic Farming, Trenthorst, Schleswig-Holstein, Germany (2019-2021; 96 plot-years).
# yield_biomass and residue_biomass are dry matter in t/ha (residue_moisture = 0) -> multiplied by 1000 to kg/ha (yield_moisture = 0, yield_isfresh = FALSE). Organic farming system: N/P/K_fertilizer = 0, fertilizer_used = FALSE.
# N_organic is computed from slurry volume (m3/ha) * n_total_content (kg N/m3) summed by treatment-year-crop in farm_operation_fertilisation_tbl.
# Plot-level soil measurements (plot_soil_measurement_view, 5-10 cm: soil_SOC in %, soil_N_total from % * 10000 -> mg/kg) and weed biomass (weeds_view, kg/ha) are joined by spatial design ID and year.
# Previous crop and planting date come from farm_operation_seedingplanting_tbl.
# Daily weather (1096 rows, 2019-2021, weather_data_daily_tbl) is exported as wth.
# Estimated coordinates for Trenthorst research station (53.786, 10.517) with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Requires DBI and RSQLite packages to read the SQLite source file.
# "Thünen" is not in terminag values_organization.csv; "wood chips" has no exact term in values_OM.csv (OM_type left NA on wood chips rows).
# Confirm units of n_total_content (kg N/m3) and total_weed_biomass (kg/ha) against the OrganicYieldsUP codebook (doi:10.5281/zenodo.15276082).
# write_files: missing carob_contributor; invalid organization Thünen.

## NOT PROCESSED
# weather_data_monthly_tbl: monthly summary redundant with daily weather_data_daily_tbl.
# Tillage/transport/weed management operation logs without standardized numeric outputs.


carob_script <- function(path) {

"
Yields and field management data of two adjacent field trials with different inputs of organic materials at the Thünen Institute of Organic Farming in North Germany

The dataset contains yields and field management data of two adjacent randomised block trials from 2019 to 2021 at fixed plot locations at the research station of the Thünen-Institute of Organic Farming in North Germany. Different techniques to enrich soil organic matter where applied in field beans, oats and silage maize (trial CF1) or in spring wheat, oats and silage maize (trial CF4). Both field trials followed spring wheat. In trial CF 1 undersown clover grass and harvest residues were incorporated or not. In trial CF4 different organic materials were applied to the soil before cultivation and seeding in spring. This dataset is part of the database compiled as an outcome of Work Area 1 in project OrganicYieldsUP. Variable definitions can be found here: https://doi.org/10.5281/zenodo.15276082
"

	uri <- "doi:10.5281/zenodo.15631936"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "Th\u00fcnen",
		publication = NA,
		project = "OrganicYieldsUP",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "OM_type;cover_crop_used;N_organic",
		response_vars = "yield;dmy_residue;weed_biomass;soil_SOC",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "TI_TI-SOC-Enrichment_2025_06_13.sqlite"]
	con <- DBI::dbConnect(RSQLite::SQLite(), f)
	h <- DBI::dbReadTable(con, "harvest_view")
	fert <- DBI::dbReadTable(con, "farm_operation_fertilisation_tbl")
	seed <- DBI::dbReadTable(con, "farm_operation_seedingplanting_tbl")
	soil <- DBI::dbReadTable(con, "plot_soil_measurement_view")
	weeds <- DBI::dbReadTable(con, "weeds_view")
	wd <- DBI::dbReadTable(con, "weather_data_daily_tbl")
	DBI::dbDisconnect(con)

	# Organic N from slurry events (quantity m3/ha * content kg N/m3)
	fert$n_kg <- ifelse(!is.na(fert$fertiliser_quantity) & !is.na(fert$n_total_content),
		as.numeric(fert$fertiliser_quantity) * as.numeric(fert$n_total_content), 0)
	nagg <- aggregate(list(N_organic=fert$n_kg),
		list(fk_treatment_id=fert$fk_treatment_id, year=fert$year, crop=fert$crop), sum, na.rm=TRUE)

	map_crop <- function(x) {
		x <- gsub("Broad and field beans", "faba bean", x)
		x <- gsub("Spring wheat", "wheat", x)
		x <- gsub("Oats", "oats", x)
		x <- gsub("Silage maize", "maize", x)
		x
	}

	seed$operation_date <- as.character(seed$operation_date)
	skey <- paste(seed$fk_treatment_id, seed$year, seed$crop)
	hkey <- paste(h$fk_treatment_id, h$year, h$crop_species)
	si <- match(hkey, skey)

	crop <- map_crop(as.character(h$crop_species))
	prev_crop <- map_crop(as.character(seed$precrop_species[si]))
	yield_part <- ifelse(h$marketable_yield_component == "grain", "grain", "aboveground biomass")

	d <- data.frame(
		trial_id = paste(h$fk_study_id, h$year, sep="_"),
		plot_id = as.character(h$pk_spatial_design_id),
		block_id = as.character(h$block),
		rep = as.integer(h$plot_nr),
		treatment = trimws(as.character(h$unique_treatment_description)),
		crop = crop,
		previous_crop = prev_crop,
		variety = as.character(h$fk_variety_id),
		planting_date = seed$operation_date[si],
		harvest_date = as.character(h$date_harvest),
		yield = as.numeric(h$yield_biomass) * 1000,
		dmy_residue = as.numeric(h$residue_biomass) * 1000,
		yield_part = yield_part,
		yield_moisture = 0,
		yield_isfresh = FALSE,
		fertilizer_used = FALSE,
		N_fertilizer = 0,
		P_fertilizer = 0,
		K_fertilizer = 0,
		country = as.character(h$country),
		adm1 = "Schleswig-Holstein",
		location = as.character(h$Town),
		elevation = as.numeric(h$site_elevation),
		land_slope = as.numeric(h$slope),
		# Trenthorst research station (postal 23847)
		latitude = 53.786,
		longitude = 10.517,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		soil_texture = "sandy loam",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$N_organic <- nagg$N_organic[match(hkey, paste(nagg$fk_treatment_id, nagg$year, nagg$crop))]
	d$N_organic[is.na(d$N_organic)] <- 0

	d$cover_crop_used <- grepl("undersown crops", d$treatment) & d$treatment != "without undersown crops"
	d$cover_crop <- ifelse(d$cover_crop_used, "clover", NA_character_)

	d$OM_used <- d$treatment %in% c("champost", "biogas manure", "wood chips") | d$N_organic > 0
	d$OM_type <- NA_character_
	d$OM_type[d$treatment == "champost"] <- "compost"
	d$OM_type[d$treatment == "biogas manure"] <- "liquid manure"
	d$OM_used[d$treatment == "without organic fertilisers"] <- FALSE
	d$OM_type[d$treatment == "without organic fertilisers"] <- "none"

	# Join plot-level soil measurements (5-10 cm) by spatial_design_id and year
	pkey <- paste(h$pk_spatial_design_id, h$year)
	soil_key <- paste(soil$pk_spatial_design_id, soil$year)
	soi <- match(pkey, soil_key)
	d$depth_top <- as.numeric(soil$soil_layer_uper_limit[soi])
	d$depth_bottom <- as.numeric(soil$soil_layer_lower_limit[soi])
	d$soil_SOC <- as.numeric(soil$soil_organic_carbon[soi])
	# soil_total_nitrogen is in % (~0.10-0.12%) -> * 10000 = mg/kg
	d$soil_N_total <- as.numeric(soil$soil_total_nitrogen[soi]) * 10000

	# Join weed biomass (kg/ha) by spatial_design_id and year
	wkey <- paste(weeds$pk_spatial_design_id, weeds$year)
	d$weed_biomass <- as.numeric(weeds$total_weed_biomass[match(pkey, wkey)])

	wth <- data.frame(
		station_name = as.character(wd$name_weather_station),
		date = as.character(wd$weather_date),
		temp = as.numeric(wd$temp_mean),
		tmin = as.numeric(wd$temp_min),
		tmax = as.numeric(wd$temp_max),
		prec = as.numeric(wd$daily_precipitation),
		rhum = as.numeric(wd$humidity_mean),
		wspd = as.numeric(wd$daily_mean_wind_speed),
		country = "Germany",
		adm1 = "Schleswig-Holstein",
		location = "Trenthorst",
		latitude = 53.786,
		longitude = 10.517,
		geo_from_source = FALSE,
		geo_uncertainty = 2000
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
