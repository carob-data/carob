# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain yield is t/ha. Each replicate is a row.
# ZTDSR is zero-till direct-seeded rice with flood irrigation. SSD is the same establishment with subsurface drip. PTR is puddled transplanted rice with flood irrigation.
# N rates in the treatment names are kg N/ha. N0 is 0. The dataset description gives PTR 120 kg N/ha.
# The description lists five treatments. The grain sheet has N rates 0, 75, 112.5, and 150 for ZTDSR and for SSD, plus PTR.
# The description places the work in the western Indo-Gangetic Plains and does not name a country.

## ISSUES
# write_files: missing variables: on_farm, yield_moisture, country, latitude, longitude, K_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# HYDRUS-2D water-balance output. The grain sheet is the measured yield.
# Figures and the paper PDF.


carob_script <- function(path) {

"
Water budgeting in conservation agriculture-based sub-surface drip irrigation using HYDRUS-2D in rice under annual rotation with wheat in Western Indo-Gangetic Plains

Measured rice grain yield for zero-till direct-seeded rice and puddled transplanted rice under flood irrigation and subsurface drip, with several nitrogen rates.
"

	uri <- "hdl:11529/10548769"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;land_prep_method;planting_method;irrigation_method",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "CIMMYT Data-Grain.xlsx"]
	r <- carobiner::read.excel(f, sheet="Grain", col_names=FALSE, col_types="text")
	code <- trimws(as.character(r[4:nrow(r), 1]))
	nrate <- rep(NA_real_, length(code))
	nrate[grepl("N0$", code)] <- 0
	nrate[grepl("N75$", code)] <- 75
	nrate[grepl("N112.5$", code)] <- 112.5
	nrate[grepl("N150$", code)] <- 150
	# Dataset description: PTR received 120 kg N/ha.
	nrate[code == "PTR"] <- 120
	prep <- ifelse(grepl("^PTR", code), "puddled", "none")
	plant <- ifelse(grepl("^PTR", code), "transplanting", "direct seeding")
	water <- ifelse(grepl("^SSD", code), "subsurface drip", "flood")

	one <- function(col, rep) {
		data.frame(
			trial_id = "rice",
			rep = rep,
			treatment = code,
			crop = "rice",
			yield = as.numeric(r[4:nrow(r), col]) * 1000,
			yield_part = "grain",
			N_fertilizer = nrate,
			land_prep_method = prep,
			planting_method = plant,
			irrigation_method = water,
			irrigated = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}
	d <- rbind(one(3, 1L), one(4, 2L), one(5, 3L))
	d$plot_id <- paste(d$treatment, d$rep, sep="_")

	carobiner::write_files(path, meta, d)
}
