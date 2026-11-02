# R script for "carob"
# license: GPL (>=3)

## NOTES
# Wheat head blast index (%) for four CIMMYT nurseries. The dataset description places Quirusillas and Okinawa in Bolivia and Jashore in Bangladesh. There are no coordinates and no grain yield.
# 15HZAN includes 2023 sowings as well as the 2024 cycle named in the title. A score of 0 is a recorded index, not a missing value.
# The files are not the RawData, Loc_data, and EnvData set used by proc_wheat.

## ISSUES
# Grain yield, fertilizer, irrigation, harvest date, and coordinates are not in these files.
# write_files: missing variables: yield, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: gid_, sowing_, nursery_, blast_index_.

## NEW VARIABLES
# blast_index_: wheat head blast index (%).
# sowing_: first or second sowing at that location.
# nursery_: nursery code (15HLBSN, 15HZAN, 41SAWSN, 56IBWSN).
# gid_: CIMMYT germplasm identifier.

## NOT PROCESSED
# Data Dictionary_Template_XHe.xlsx is an empty dictionary template.
# Blank columns in 15HZAN_WB.csv.


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
		response_vars = "blast_index_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
	)

	country_of <- c(Quirusillas="Bolivia", Okinawa="Bolivia", Jashore="Bangladesh")

	one <- function(f) {
		r <- read.csv(f)
		nursery <- sub("_WB\\.csv$", "", basename(f))
		entry <- r$Entry
		if (is.null(entry)) entry <- r$Ent
		scores <- grep("_sowing$", names(r), value=TRUE)
		parts <- lapply(scores, function(nm) {
			bit <- strsplit(nm, "_", fixed=TRUE)[[1]]
			x <- trimws(as.character(r[[nm]]))
			x[!nzchar(x)] <- NA_character_
			data.frame(
				trial_id = paste(nursery, bit[1], bit[2], bit[3], sep="_"),
				variety_code = as.character(entry),
				variety_pedigree = as.character(r$Cross),
				variety = trimws(as.character(r$Sel_Hist)),
				gid_ = as.character(r$GID),
				location = bit[1],
				country = unname(country_of[bit[1]]),
				planting_date = bit[2],
				sowing_ = bit[3],
				nursery_ = nursery,
				blast_index_ = as.numeric(x),
				crop = "wheat",
				yield_part = "grain",
				stress = "disease",
				on_farm = FALSE,
				is_survey = FALSE,
				geo_from_source = FALSE
			)
		})
		do.call(rbind, parts)
	}

	ff <- ff[grepl("_WB\\.csv$", basename(ff))]
	d <- do.call(rbind, lapply(ff, one))
	d$variety[is.na(d$variety) | !nzchar(d$variety)] <- NA_character_
	d$variety_pedigree[is.na(d$variety_pedigree) | !nzchar(d$variety_pedigree)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
