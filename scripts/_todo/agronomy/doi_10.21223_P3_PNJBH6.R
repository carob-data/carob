# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato irrigation experiment at La Molina, Lima, Peru. The dataset geographic coverage is one west longitude and one south latitude.
# The abstract gives plots of 3.6 by 5 m and spacing of 0.3 m between plants and 1 m between furrows. Spacing is stored in cm. Plot area is 18 m2, which the irrigation description also states.
# The dictionary says fresh tuber biomass (column PF) is grams per plant, and nT is tubers per plant.
# Dictionary treatments: T1 full irrigation, T2 gs_max 0.15, T3 gs_max 0.05. The abstract numbers the thresholds the other way around and calls the frequent irrigation a control.
# Canopy temperature, air temperature, and the wet-surface temperature are degrees C. CWSI is the crop water stress index. Those readings are repeated by days after planting and hour, so they are in the long table.
# The citation is a Potato Research paper identified by ISSN, not a DOI.

## ISSUES
# No calendar planting or harvest date. Days after planting are not a date.
# No fertilizer rate.
# Fresh weight is grams per plant. It is not converted to kg/ha. The abstract says 60 plants were sown per plot and the harvest file has fewer plants per plot.
# Soil moisture, biomass, isotopes, and irrigation volumes are not in the plot table.
# missing variables: carob_contributor
# missing variables: yield, yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: irrigation_regime_
# unknown variable: n_tubers_ (fw_tubers?)
# unknown variables: hour_, tuber_fresh_g_plant_, cwsi_, canopy_temp_c_, air_temp_c_

## NOT PROCESSED
# Dry_biomass.xlsx: fresh and dry sample weights in g per plant, a different table from the harvest plants.
# Eco-physiological.xlsx and Tuber_isotopes.xlsx: conductance, water potential, and isotope values.
# Irrigation.xlsx: soil water and irrigation volume by day. Lr is mm.
# Data_dictionary.xls: the definitions used above.


carob_script <- function(path) {

"
Replication Data for: Analyze Crop Water Stress Index (CWSI) values for an optimum tuber yield in potatoes under humid conditions.

It developed a field experiment, with the aim to analyze Crop Water Stress Index (CWSI) under humid conditions with testing two irrigation timing treatments with pre-established gs_max threshold (0.15 [T1] and 0.050 [T2] mol H2O m−2 s−1) against a control (frequently irrigated). The experiment was conducted in a 416 m2 total area, divided in 12 plots (3.6×5 m2 each) and sixty plants per plot were sown in five rows with a plant and furrow distance of 0.3 and 1 m, respectively. Seven furrow irrigations were provided at 0, 4, 7, 11, 17, 22 and 30 days after planting watering every plot with approximately 67 mm per irrigation; after that, a drip irrigation system was set up independently for each plot.
"

	uri <- "doi:10.21223/P3_PNJBH6"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_regime_",
		response_vars = "tuber_fresh_g_plant_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	lab <- c(
		T1 = "full irrigation",
		T2 = "gs_max 0.15",
		T3 = "gs_max 0.05"
	)
	h <- carobiner::read.excel(ff[basename(ff) == "Harvest.xlsx"], na=c("", "NA"))
	names(h) <- trimws(names(h))
	trat <- trimws(as.character(h$trat))
	regime <- lab[trat]
	regime[is.na(regime)] <- trat[is.na(regime)]
	plots <- unique(data.frame(plot = h$plot, regime = regime))

	d <- data.frame(
		trial_id = "LaMolina_CWSI",
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		# dataset geographic coverage: one west longitude and one south latitude
		latitude = -12.08,
		longitude = -76.95,
		geo_from_source = TRUE,
		crop = "potato",
		plot_id = as.character(plots$plot),
		treatment = plots$regime,
		irrigation_regime_ = plots$regime,
		plot_area = 3.6 * 5,
		plant_spacing = 0.3 * 100,
		row_spacing = 1 * 100,
		irrigated = TRUE,
		yield_part = "tubers",
		on_farm = FALSE,
		is_survey = FALSE
	)

	cw <- carobiner::read.excel(ff[basename(ff) == "CWSI.xlsx"], na=c("", "NA"))
	names(cw) <- trimws(names(cw))
	hh <- cw$hour
	if (inherits(hh, "POSIXt")) {
		hh <- format(hh, "%H:%M", tz = "UTC")
	} else {
		hh <- sub(".* ([0-9]{2}:[0-9]{2}).*", "\\1", as.character(hh))
	}
	plants <- data.frame(
		plot_id = as.character(h$plot),
		order = as.integer(h$plant),
		DAP = NA_integer_,
		hour_ = NA_character_,
		tuber_fresh_g_plant_ = as.numeric(h$PF),
		n_tubers_ = as.numeric(h$nT),
		cwsi_ = NA_real_,
		canopy_temp_c_ = NA_real_,
		air_temp_c_ = NA_real_
	)
	stress <- data.frame(
		plot_id = as.character(cw$pot),
		order = NA_integer_,
		DAP = as.integer(cw$dap),
		hour_ = hh,
		tuber_fresh_g_plant_ = NA_real_,
		n_tubers_ = NA_real_,
		cwsi_ = as.numeric(cw$CWSI),
		canopy_temp_c_ = as.numeric(cw$Tf),
		air_temp_c_ = as.numeric(cw$Ta)
	)
	dlong <- rbind(plants, stress)
	dlong$trial_id <- "LaMolina_CWSI"
	dlong <- dlong[order(dlong$plot_id, dlong$DAP, dlong$order), ]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
