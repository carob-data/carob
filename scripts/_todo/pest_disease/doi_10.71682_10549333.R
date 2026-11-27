# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Coordinates are not reported in the data files; approximate locations for the three screening sites (Jashore, Okinawa Uno, Quirusillas) were georeferenced from the site names.
# 15HZAN_WB.csv reports 2023 sowings even though the dataset title specifies the 2024 cropping cycle.

## NEW
# sowing_: sowing cycle at a given location and year ("1st" or "2nd").

## MISSING
# yield, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date.

## NOTES
# Wheat head blast index (%) for four CIMMYT nurseries (15HLBSN, 15HZAN, 41SAWSN, 56IBWSN) evaluated at Quirusillas and Okinawa (Bolivia) and Jashore (Bangladesh) in 2023-2024.
# Wheat head blast index (0-100%, incidence x severity / 100) is stored in disease_severity with disease = "blast severity" and severity_scale = "0-100%". A value of 0 is a recorded score, not missing.
# Nursery codes are stored in trial_name, GID in variety_code, Sel_Hist in variety, and Cross in variety_pedigree.

## NOT PROCESSED
# Data Dictionary_Template_XHe.xlsx: empty dictionary template with no data.
# Cid, Sid in each *_WB.csv: internal CIMMYT cross and selection identifiers (GID is stored in variety_code).
# Trailing blank columns in 15HZAN_WB.csv.


carob_script <- function(path) {

"
Wheat Blast Data for four CIMMYT wheat nurseries during the 2024 cropping cycle

Wheat head blast index (%) data for four CIMMYT nurseries (15HLBSN, 15HZAN, 41SAWSN, and 56IBWSN) is presented. Field trials took place in Quirusillas and Okinawa (Bolivia) and Jashore (Bangladesh) during the 2024 cycles. Two sowings were made in each location/cycle except for Quirusillas where only one sowing was made.
"

	uri <- "doi:10.71682/10549333"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "disease_severity",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 95,
		carob_effort = 0
	)

	# Georeferenced from screening station / locality names in dataset description
	sites <- data.frame(
		location = c("Jashore", "Okinawa", "Quirusillas"),
		country = c("Bangladesh", "Bolivia", "Bolivia"),
		adm1 = c("Khulna", "Santa Cruz", "Santa Cruz"),
		adm2 = c("Jashore", "Warnes", "Florida"),
		latitude = c(23.185, -17.220, -18.338),
		longitude = c(89.188, -62.895, -63.956),
		geo_uncertainty = c(5000, 5000, 5000),
		geo_source = c(
			"BWMRI Regional Agricultural Research Station, Jashore",
			"Okinawa Uno, Santa Cruz, Bolivia",
			"Quirusillas, Santa Cruz, Bolivia"
		)
	)

	one <- function(f) {
		r <- utils::read.csv(f, check.names=FALSE, na.strings=c("", "NA", "."))
		nursery <- sub("_WB\\.csv$", "", basename(f))
		entry <- r$Entry
		if (is.null(entry)) entry <- r$Ent
		scores <- grep("_sowing$", names(r), value=TRUE)
		parts <- lapply(scores, function(nm) {
			bit <- strsplit(nm, "_", fixed=TRUE)[[1]]
			val <- as.numeric(r[[nm]])
			data.frame(
				trial_id = paste(nursery, bit[1], bit[2], bit[3], sep="_"),
				trial_name = nursery,
				plot_id = trimws(as.character(entry)),
				variety_code = trimws(as.character(r$GID)),
				variety_pedigree = trimws(as.character(r$Cross)),
				variety = trimws(as.character(r$Sel_Hist)),
				treatment = trimws(as.character(r$GID)),
				location = bit[1],
				planting_date = bit[2],
				sowing_ = bit[3],
				crop = "wheat",
				yield_part = "grain",
				disease = "blast severity",
				disease_severity = ifelse(is.na(val), NA_character_, as.character(round(val, 4))),
				severity_scale = "0-100%",
				stress = "disease",
				on_farm = FALSE,
				is_survey = FALSE,
				geo_from_source = FALSE
			)
		})
		do.call(rbind, parts)
	}

	ff <- ff[basename(ff) %in% c(
		"15HLBSN_WB.csv",
		"15HZAN_WB.csv",
		"41SAWSN_WB.csv",
		"56IBWSN_WB.csv"
	)]
	d <- do.call(rbind, lapply(ff, one))
	d <- merge(d, sites, by="location", all.x=TRUE, sort=FALSE)
	d$variety[!nzchar(d$variety)] <- NA_character_
	d$variety_pedigree[!nzchar(d$variety_pedigree)] <- NA_character_
	d$variety_code[!nzchar(d$variety_code)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
