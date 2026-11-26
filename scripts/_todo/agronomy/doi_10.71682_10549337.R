# R script for "carob"
# license: GPL (>=3)

## NOTES
# Split nitrogen application on strong-gluten bread wheat (Cisne F2016, Alondra F2014, Luminaria F2012) under Conservation Agriculture at Irapuato and Pénjamo (Guanajuato, Bajío region, Mexico) across 2016-2017 and 2017-2018 fall-winter cycles.
# Experimental design stated in sheet "Distribución de tratamientos": split-plot RCBD with 3 replications (main plot = N splitting, subplot = variety; useful harvested area = 1.5 x 2 m = 3 m2).
# Variable definitions from sheet "Introducción": RA = grain yield (kg/ha); gpmc = grains/m2; epmc = spikes/m2 (x10000 -> spike_density /ha); gpe = grains/spike; mil = 1000-grain weight (g -> seed_weight); Bio = total biomass (t/ha -> x1000 y_total kg/ha); prot = grain protein (%); alt = plant height (cm); spad 1 & spad 2 = SPAD readings (65 DAS and second timing, preserved in long table linked via record_id).
# Treatments 2 and 4 are both split 33-33-33 at 210 and 300 kg N/ha respectively; Treatment 5 uses GreenSeeker optical-sensor N totals from sheet "Datos sensor" by location, variety, and harvest year; Treatment 6 is unfertilized control (0 kg N/ha).
# Estimated city coordinates added for Irapuato (20.677, -101.356) and Pénjamo (20.431, -101.722) with geo_from_source = FALSE and geo_uncertainty = 5000 m.

## ISSUES
# No exact calendar planting/harvest dates, P/K fertilizer rates, irrigation amounts, or grain yield moisture in the workbook.
# Sheet "Introducción" labels both spad 1 and spad 2 as 65 DAS (likely a copy-paste typo for spad 2); both readings are preserved in long with order = 1 (DAP = 65) and order = 2.
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, P_fertilizer, harvest_date; unknown grains_m2_.

## NOT PROCESSED
# Matriz CP and Matriz IGA descriptions in Introducción: derived analysis matrices (PCA/SREG) of the same Matriz ANOVA records.
# Field randomization layout map in Distribución de tratamientos.

## NEW VARIABLES
# grains_m2_: number of grains per square meter (count/m2).


carob_script <- function(path) {

"
Impact of nitrogen split application on wheat grain yield and protein content in Conservation Agriculture

Research on the effect of split nitrogen application on wheat grain yield and quality under conservation agriculture. The study was conducted during the 2016-2017 and 2017-2018 fall-winter cycles in the Bajío region of Guanajuato.
"

	uri <- "doi:10.71682/10549337"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = "split-plot RCBD",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;N_splits;variety;treatment",
		response_vars = "yield;y_total;grain_protein;plant_height;seed_weight;spike_density;grains_per_spike;SPAD",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "BD-TrigoFuerte-Baj-2024 b.xlsx"]
	r <- carobiner::read.excel(f, sheet="Matriz ANOVA")

	variety <- c("1"="Cisne F2016", "2"="Alondra F2014", "3"="Luminaria F2012")
	location <- c("1"="Irapuato", "2"="P\u00e9njamo")
	lat_map <- c("1"=20.677, "2"=20.431)
	lon_map <- c("1"=-101.356, "2"=-101.722)
	season <- c("1"="2016", "2"="2017")
	sensor_year <- c("1"="2017", "2"="2018")
	treat <- c(
		"1"="50-50-0",
		"2"="33-33-33 (210)",
		"3"="30-70-0",
		"4"="33-33-33 (300)",
		"5"="sensor",
		"6"="control"
	)
	n_fixed <- c("1"=210, "2"=210, "3"=210, "4"=300, "5"=NA_real_, "6"=0)
	n_splits <- c("1"=2L, "2"=3L, "3"=2L, "4"=3L, "5"=NA_integer_, "6"=0L)

	# Totals from Datos sensor, kg N/ha, for the optical-sensor treatment only.
	sensor <- data.frame(
		location = rep(c("Irapuato", "P\u00e9njamo"), each=6),
		year = rep(c("2017", "2018"), each=3, times=2),
		variety = rep(c("Cisne", "Alondra", "Luminaria"), times=4),
		n_sensor = c(185, 215, 203, 241, 266, 203, 275, 216, 218, 226, 213, 229)
	)

	loc_code <- as.character(r$Localidad)
	code <- as.character(r$Tratamiento)
	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = paste(unname(location[loc_code]), unname(season[as.character(r$Ciclo)]), sep="-"),
		country = "Mexico",
		adm1 = "Guanajuato",
		location = unname(location[loc_code]),
		latitude = unname(lat_map[loc_code]),
		longitude = unname(lon_map[loc_code]),
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		planting_date = unname(season[as.character(r$Ciclo)]),
		crop = "wheat",
		yield_part = "grain",
		variety = unname(variety[as.character(r$Genotipo)]),
		treatment = unname(treat[code]),
		rep = as.integer(r[["Repetici\u00f3n"]]),
		yield = as.numeric(r$RA),
		y_total = as.numeric(r$Bio) * 1000,
		grains_m2_ = as.numeric(r$gpmc),
		spike_density = as.numeric(r$epmc) * 10000,
		grains_per_spike = as.numeric(r$gpe),
		seed_weight = as.numeric(r$mil),
		grain_protein = as.numeric(r$prot),
		plant_height = as.numeric(r$alt),
		N_fertilizer = unname(n_fixed[code]),
		N_splits = unname(n_splits[code]),
		land_prep_method = "none",
		plot_area = 3,
		plot_length = 2,
		plot_width = 1.5,
		on_farm = FALSE,
		is_survey = FALSE
	)
	short <- sub(" F.*", "", d$variety)
	sy <- unname(sensor_year[as.character(r$Ciclo)])
	key <- paste(d$location, short, sy)
	skey <- paste(sensor$location, sensor$variety, sensor$year)
	d$N_fertilizer[d$treatment == "sensor"] <- sensor$n_sensor[match(key, skey)][d$treatment == "sensor"]
	d$fertilizer_used <- d$N_fertilizer > 0

	# Preserve both SPAD measurement timings in long table linked via record_id
	dlong <- rbind(
		data.frame(record_id = d$record_id, order = 1L, DAP = 65L, SPAD = as.numeric(r[["spad 1"]])),
		data.frame(record_id = d$record_id, order = 2L, DAP = NA_integer_, SPAD = as.numeric(r[["spad 2"]]))
	)
	dlong <- dlong[!is.na(dlong$SPAD), ]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
