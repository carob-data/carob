# R script for "carob"
# license: GPL (>=3)

## NOTES
# Late blight field experiment at Comas, Junin. The title gives the place. The files do not name the country.
# Late blight scores LB1 to LB19 and canopy cover are repeated measurements. Dates come from the evaluation-date files.
# Canopy cover is a percentage in the variable name (Canopy cover using grid in percentage). A cell with only a dot is missing.
# AUDPC in the file is 3.9 to 80 and rAUDPC is 0.039 to 0.80, which matches the AUDPC and rAUDPC terms.
# Fresh biomass is grams per plot and grams per plant, and plot dry matter is grams per plot, from the biomass date file.
# Emergence is the Emergency_date 1999-01-28. That spelling is the column name in the canopy date file.

## ISSUES
# No coordinates. Comas needs georeferencing.
# Frequency is 0, 4, 7, or 14. The files do not say what was applied.
# TBDMt_ha is named like t/ha, but the date file describes that abbreviation as dry matter per plant. The number is stored as biomass_dm_recorded_ and is not used as yield.
# No fertilizer rate and no planting or harvest date.
# The daily weather file does not state units, so it is not in the weather table.
# AUDPC scale is not stated.
# write_files: , yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: frequency_, scale_r_, biomass_fresh_g_plot_, biomass_fresh_g_plant_, biomass_dm_g_plot_, biomass_dm_recorded_.

## NOT PROCESSED
# 03-DailyWeather-Data-Comas.xlsx: daily weather with no units.
# Scale is not applied to the late blight scores.


carob_script <- function(path) {

"
Dataset for: Effect of potato late blight in the field in Comas - Junin

These datasets correspond to the field experiments in Comas.
"

	uri <- "doi:10.21223/D9AOAZ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;frequency_",
		response_vars = "disease_severity",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.55
	)

	f <- ff[grepl("01_Data_LateBlight", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$Variety))
	freq <- as.character(r$Frequency)
	plot_id <- paste(variety, freq, r$Repetition, sep="_")

	d <- data.frame(
		trial_id = "Comas_1999",
		adm1 = "Junin",
		location = "Comas",
		crop = "potato",
		variety = variety,
		frequency_ = freq,
		treatment = paste(variety, freq, sep="_"),
		rep = as.integer(r$Repetition),
		plot_id = plot_id,
		AUDPC = as.numeric(r$AUDPC),
		rAUDPC = as.numeric(r$RAUDPC),
		scale_r_ = as.character(r[["Scale R"]]),
		stress = "disease",
		emergence_date = "1999-01-28",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	iso <- function(x) as.character(as.Date(x))
	fd <- ff[grepl("02_LateBlight_Evaluation", basename(ff))]
	rd <- carobiner::read.excel(fd)
	lb_date <- iso(rd$Evaluation_date[match(paste0("LB", seq_len(19)), trimws(rd$Abbreviation))])

	parts <- vector("list", 19)
	for (i in seq_len(19)) {
		parts[[i]] <- data.frame(
			plot_id = plot_id,
			date = lb_date[i],
			disease_severity = as.character(r[[paste0("LB", i)]]),
			ground_cover = NA_real_,
			biomass_fresh_g_plot_ = NA_real_,
			biomass_fresh_g_plant_ = NA_real_,
			biomass_dm_g_plot_ = NA_real_,
			biomass_dm_recorded_ = NA_real_
		)
	}

	fc <- ff[grepl("06_Data_Canopy", basename(ff))]
	rc <- carobiner::read.excel(fc, na=c("", "NA"))
	names(rc) <- trimws(names(rc))
	fcd <- ff[grepl("07_canopy", basename(ff))]
	rcd <- carobiner::read.excel(fcd)
	cc_names <- paste0("CC_G_Et_%_", seq_len(13))
	cc_date <- iso(rcd$Evaluation_date_CCG[match(cc_names, trimws(as.character(rcd$Abbreviation)))])
	cc_id <- paste(trimws(as.character(rc$Variety)), as.character(rc$Frequency), rc$Repetition, sep="_")
	cc_num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c(".", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	for (i in seq_along(cc_names)) {
		parts[[length(parts) + 1]] <- data.frame(
			plot_id = cc_id,
			date = cc_date[i],
			disease_severity = NA_character_,
			ground_cover = cc_num(rc[[cc_names[i]]]),
			biomass_fresh_g_plot_ = NA_real_,
			biomass_fresh_g_plant_ = NA_real_,
			biomass_dm_g_plot_ = NA_real_,
			biomass_dm_recorded_ = NA_real_
		)
	}

	fy <- ff[grepl("04_TotalYield", basename(ff))]
	ry <- carobiner::read.excel(fy, na=c("", "NA"))
	names(ry) <- trimws(names(ry))
	when <- c(
		"Inicio" = "1999-01-28",
		"1ra evaluacion" = "1999-02-25",
		"2da evaluacion" = "1999-03-24",
		"3ra evaluacion" = "1999-04-14",
		"4ta evaluacion" = "1999-05-06"
	)
	lab <- trimws(as.character(ry[["Date-evaluation"]]))
	if (any(!lab %in% names(when))) stop("unmapped biomass date")
	yid <- paste(trimws(as.character(ry$Variety)), as.character(ry$Frequency), ry$Repetition, sep="_")
	parts[[length(parts) + 1]] <- data.frame(
		plot_id = yid,
		date = unname(when[lab]),
		disease_severity = NA_character_,
		ground_cover = NA_real_,
		biomass_fresh_g_plot_ = as.numeric(ry$TBFw_Cp_gplot),
		biomass_fresh_g_plant_ = as.numeric(ry$TBFw_Cp_gplant),
		biomass_dm_g_plot_ = as.numeric(ry$TBDMC_Cp_gplot),
		biomass_dm_recorded_ = as.numeric(ry$TBDMt_ha)
	)

	dlong <- do.call(rbind, parts)
	dlong$trial_id <- "Comas_1999"
	keep <- !(is.na(dlong$disease_severity) & is.na(dlong$ground_cover) & is.na(dlong$biomass_fresh_g_plot_) & is.na(dlong$biomass_dm_g_plot_) & is.na(dlong$biomass_dm_recorded_))
	dlong <- dlong[keep, ]
	dlong <- dlong[order(dlong$plot_id, dlong$date), ]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
