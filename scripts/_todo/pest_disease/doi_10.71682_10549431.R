# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Coordinates are not reported in the data files; approximate locations for Jashore (Bangladesh) and Okinawa Uno (Bolivia) were georeferenced from the site names.
# In 16HLBSN_WB.csv, entries 51 and 52 have parentage = "LOCAL CHECK" and blank germplasm name; they are retained with variety = "LOCAL CHECK".

## NEW
# sowing_: sowing cycle at a given location and year ("1st" or "2nd").

## MISSING
# yield, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date.

## NOTES
# Wheat head blast index (%) for four CIMMYT nurseries (16HLBSN, 16HZAN, 42SAWSN, 57IBWSN) evaluated at Jashore (Bangladesh, two sowings) and Okinawa (Bolivia, one sowing) during the 2025 cycle.
# Wheat head blast index (0-100%, incidence x severity / 100) is stored in disease_severity with disease = "blast severity" and severity_scale = "0-100%".
# Nursery codes are stored in trial_name, germplasm code in variety_code, germplasm name in variety, and parentage in variety_pedigree.

## NOT PROCESSED
# Data Dictionary_Template_XHe.xlsx: empty dictionary template with no observation data.


carob_script <- function(path) {

"
Wheat Blast Data for four CIMMYT wheat nurseries during the 2025 cropping cycle

Wheat head blast index (%) data for four CIMMYT nurseries (16HLBSN, 16HZAN, 42SAWSN, and 57IBWSN). Field trials took place in Okinawa (Bolivia) and Jashore (Bangladesh) during the 2025 cycles. Two sowings were made in Jashore while only one was made in Okinawa.
"

	uri <- "doi:10.71682/10549431"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 95,
		carob_effort = 0
	)

	# Georeferenced from screening station / locality names in dataset description
	sites <- data.frame(
		column = c("Jashore_2025_1st_sowing", "Jashore_2025_2nd_sowing", "Okinawa_2025_2nd_sowing"),
		country = c("Bangladesh", "Bangladesh", "Bolivia"),
		adm1 = c("Khulna", "Khulna", "Santa Cruz"),
		adm2 = c("Jashore", "Jashore", "Warnes"),
		location = c("Jashore", "Jashore", "Okinawa"),
		latitude = c(23.185, 23.185, -17.220),
		longitude = c(89.188, 89.188, -62.895),
		geo_uncertainty = c(5000, 5000, 5000),
		geo_source = c(
			"BWMRI Regional Agricultural Research Station, Jashore",
			"BWMRI Regional Agricultural Research Station, Jashore",
			"Okinawa Uno, Santa Cruz, Bolivia"
		),
		sowing = c("1st", "2nd", "2nd")
	)

	one <- function(f) {
		r <- utils::read.csv(f, check.names=FALSE, na.strings=c("", "NA", "."))
		nursery <- sub("_WB\\.csv$", "", basename(f), ignore.case=TRUE)
		vname <- trimws(as.character(r[["germplasm name"]]))
		ped <- trimws(as.character(r$parentage))
		# Retain local checks whose germplasm name cell is blank
		chk <- (is.na(vname) | !nzchar(vname)) & !is.na(ped) & nzchar(ped)
		vname[chk] <- ped[chk]
		parts <- lapply(seq_len(nrow(sites)), function(i) {
			col <- sites$column[i]
			if (!col %in% names(r)) return(NULL)
			val <- as.numeric(r[[col]])
			data.frame(
				country = sites$country[i],
				adm1 = sites$adm1[i],
				adm2 = sites$adm2[i],
				location = sites$location[i],
				latitude = sites$latitude[i],
				longitude = sites$longitude[i],
				geo_uncertainty = sites$geo_uncertainty[i],
				geo_source = sites$geo_source[i],
				geo_from_source = FALSE,
				trial_id = paste(nursery, sites$location[i], "2025", sites$sowing[i], sep="_"),
				trial_name = nursery,
				plot_id = trimws(as.character(r[["entry number"]])),
				variety = vname,
				variety_code = trimws(as.character(r[["germplasm code"]])),
				variety_pedigree = ped,
				treatment = vname,
				planting_date = "2025",
				sowing_ = sites$sowing[i],
				crop = "wheat",
				yield_part = "grain",
				disease = "blast severity",
				disease_severity = ifelse(is.na(val), NA_character_, as.character(round(val, 4))),
				severity_scale = "0-100%",
				stress = "disease",
				on_farm = FALSE,
				is_survey = FALSE
			)
		})
		do.call(rbind, parts)
	}

	ff <- ff[basename(ff) %in% c(
		"16HLBSN_WB.csv",
		"16HZAN_WB.csv",
		"42SAWSN_WB.csv",
		"57IBWSN_WB.csv"
	)]
	d <- do.call(rbind, lapply(ff, one))
	d$variety[!nzchar(d$variety)] <- NA_character_
	d$variety_code[!nzchar(d$variety_code)] <- NA_character_
	d$variety_pedigree[!nzchar(d$variety_pedigree)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
