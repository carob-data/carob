# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yield_CAS_tha is cane yield in t/ha for plots P1-P6. Border, path and TE plots have no yield row.
# Treatments are the Factor_Trial descriptions joined through Factor_Unit.
# N, P and K are the sum of the two ferti_*_app_rate applications on the row (P*_R). The column names say P and K, not P2O5 and K2O, so they are not converted. K sums to 313 kg/ha.
# The trial is described as irrigated sugarcane at La Mare. Country is geographicCoverage Réunion.
# Coordinates and elevation are unit_lat, unit_long and unit_alt in Experimental_design (same point as the weather station).
# Weather is daily. Global radiation is not labeled; values 3-31 are treated as MJ m-2 day-1 and stored as kJ m-2. Dates in the weather and ITK files are d/m/yyyy (the cycle starts 02/10/2018).
# Cycle start is harvesting_date_ini (02/10/2018) and harvest is harvesting_date (18/09/2019).

## ISSUES
# P2 and P5 are both the chemical-weeding treatment. There is no separate replicate index.
# Herbicide product rates, sugar richness, and the single soil observation are not mapped. Cane moisture is not stated.
# Ground cover is on subplot codes, kept as unit_code_ because those codes are not the yield plot ids.
# write_files: missing yield_moisture; out of bounds K_fertilizer (313, 313); unknown variables unit_code_.

## NEW VARIABLES
# unit_code_: observation unit for ground cover (subplot), not the yield plot id

## NOT PROCESSED
# Herbicide volumes (merlin, prowl, camix, roundup, 2.4D, sencoral, callisto)
# Richness_Sugar_CAS: unit not stated
# IFTH_tot_cycle: treatment frequency index, not an application count
# Accession and weed-species lists
# Soil variables recorded once on P12_2018_2019_Soil (one value for the whole trial, not per plot)

carob_script <- function(path) {

"
Experimental dataset for the evaluation of mechanical weed control methods with and without tillage (2018 - 2019) - P12 trial - Projet AgriecoH

The AgriecoH (Agro-equipment and low Herbicide innovation) project, an Ecophyto experimentation project with innovative micromechanisation techniques to reduce the use of herbicides in cane. This project is based on trials conducted in experimental stations or in co-design with growers in Réunion. This is an experimental dataset from an irrigated sugarcane trial (R1) set up in 2018 and harvested in 2019, at the La Mare experimental site.
"

	uri <- "doi:10.18167/DVN1/0LKVJU"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "AgriecoH",
		design = NA,
		data_type = "experiment",
		treatment_vars = "weeding_method;intercrops",
		response_vars = "yield;ground_cover",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 50,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "Experimental_design.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Factor_Unit")
	r2 <- carobiner::read.excel(f1, sheet="Factor_Trial")
	desc <- r2$description[match(r1$factor_level, r2$factor_level)]
	trt <- data.frame(plot = r1$unit_code, weeding_method = desc, level = r1$factor_level)

	f2 <- ff[basename(ff) == "ITK.xlsx"]
	r3 <- carobiner::read.excel(f2, sheet="Itk")
	fert_vars <- c("ferti_N_app_rate_kg.ha-1", "ferti_P_app_rate_kg.ha-1", "ferti_K_app_rate_kg.ha-1")
	fv <- r3[r3$itk_variable %in% fert_vars, ]
	fv$rate <- as.numeric(fv$itk_value)
	fv$plot <- sub("_R$", "", fv$unit_code)
	fv <- fv[fv$plot %in% paste0("P", 1:6), ]
	fn <- aggregate(rate ~ plot, fv[fv$itk_variable == fert_vars[1], ], sum)
	fp <- aggregate(rate ~ plot, fv[fv$itk_variable == fert_vars[2], ], sum)
	fk <- aggregate(rate ~ plot, fv[fv$itk_variable == fert_vars[3], ], sum)
	names(fn)[2] <- "N_fertilizer"
	names(fp)[2] <- "P_fertilizer"
	names(fk)[2] <- "K_fertilizer"

	f3 <- ff[basename(ff) == "Observation.xlsx"]
	r4 <- carobiner::read.excel(f3, sheet="PlotPlant_data")
	y <- r4[r4$OBS_VARIABLE == "Yield_CAS_tha", ]
	y$plot <- sub("_Cane$", "", y$UNIT_CODE)

	d <- data.frame(
		plot_id = y$plot,
		yield = as.numeric(y$VALEUR_QUANTITATIVE) * 1000,
		harvest_date = "2019-09-18",
		planting_date = "2018-10-02",
		crop = "sugarcane",
		yield_part = "stems",
		country = "Réunion",
		location = "La Mare",
		trial_id = "P12_2018_2019",
		longitude = 55.53255556,
		latitude = -20.9036528,
		elevation = 70,
		geo_from_source = TRUE,
		irrigated = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d <- merge(d, trt, by.x="plot_id", by.y="plot", all.x=TRUE)
	d <- merge(d, fn, by.x="plot_id", by.y="plot", all.x=TRUE)
	d <- merge(d, fp, by.x="plot_id", by.y="plot", all.x=TRUE)
	d <- merge(d, fk, by.x="plot_id", by.y="plot", all.x=TRUE)
	d$treatment <- d$weeding_method
	d$intercrops <- ifelse(d$level == "M2", "cowpea", "none")
	d$level <- NULL
	# M2 is "Vigna unguiculata black sown in the inter-row" in Factor_Trial.
	# irrigated from the dataset description. coordinates from Experimental_design.

	gc <- r4[r4$OBS_VARIABLE == "plant_ground_cover_%", ]
	long <- data.frame(
		trial_id = "P12_2018_2019",
		unit_code_ = gc$UNIT_CODE,
		date = as.character(as.Date(gc$OBS_DATE, "%d/%m/%Y")),
		ground_cover = as.numeric(gc$VALEUR_QUANTITATIVE)
	)
	long <- long[!is.na(long$ground_cover), ]

	f4 <- ff[basename(ff) == "Weather.xlsx"]
	r5 <- carobiner::read.excel(f4, sheet="Weather_day")
	wth <- data.frame(
		station_name = r5$nom_station,
		date = as.character(as.Date(r5$date, "%d/%m/%Y")),
		prec = as.numeric(r5$rain_day_interpol),
		srad = as.numeric(r5$global_radiation_interpol) * 1000,
		tmin = as.numeric(r5$air_temp_min_day_interpol),
		temp = as.numeric(r5$air_temp_mean_day_interpol),
		tmax = as.numeric(r5$air_temp_max_day_interpol),
		ETP = as.numeric(r5$etp_interpol),
		longitude = 55.53255556,
		latitude = -20.9036528,
		elevation = 70
	)

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
