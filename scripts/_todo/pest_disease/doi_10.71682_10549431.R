# R script for "carob"
# license: GPL (>=3)

## NOTES
# Wheat head blast index (%) for four nurseries in 2025. The description places Jashore in Bangladesh and Okinawa in Bolivia. Two sowings were made in Jashore and one in Okinawa.
# The index is the only measurement in the files. There is no yield.

## ISSUES
# No coordinates.
# write_files: missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: blast_index_.

## NEW VARIABLES
# blast_index_: wheat head blast index, percent, as named in the dataset description.

## NOT PROCESSED
# The data-dictionary template. It describes the columns and has no plot records.


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
		project = "Wheat blast nurseries 2025",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "blast_index_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.3
	)

	sites <- data.frame(
		column = c("Jashore_2025_1st_sowing", "Jashore_2025_2nd_sowing", "Okinawa_2025_2nd_sowing"),
		location = c("Jashore", "Jashore", "Okinawa"),
		country = c("Bangladesh", "Bangladesh", "Bolivia"),
		sowing = c("1", "2", "2")
	)

	one <- function(f) {
		r <- utils::read.csv(f, check.names=FALSE, na.strings=c("", "NA", "."))
		nursery <- sub("_WB\\.csv$", "", basename(f), ignore.case=TRUE)
		parts <- list()
		for (i in seq_len(nrow(sites))) {
			col <- sites$column[i]
			if (!col %in% names(r)) next
			x <- trimws(as.character(r[[col]]))
			x[!nzchar(x)] <- NA_character_
			parts[[i]] <- data.frame(
				country = sites$country[i],
				location = sites$location[i],
				trial_id = paste(nursery, sites$location[i], "sowing", sites$sowing[i]),
				plot_id = trimws(as.character(r[["entry number"]])),
				variety = trimws(as.character(r[["germplasm name"]])),
				variety_code = trimws(as.character(r[["germplasm code"]])),
				variety_pedigree = trimws(as.character(r$parentage)),
				blast_index_ = as.numeric(x)
			)
		}
		do.call(rbind, parts)
	}

	ff <- ff[grepl("\\.csv$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(ff, one))
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d <- d[!duplicated(d), ]
	d$variety_code[!nzchar(d$variety_code)] <- NA_character_
	d$variety_pedigree[!nzchar(d$variety_pedigree)] <- NA_character_
	d$treatment <- d$variety
	d$crop <- "wheat"
	d$disease <- "blast severity"
	d$stress <- "disease"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
