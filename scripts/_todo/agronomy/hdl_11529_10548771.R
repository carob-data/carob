# R script for "carob"
# license: GPL (>=3)

## NOTES
# The grain sheet is rice only, three replicates. Yield is t/ha, stored as kg/ha.
# N rates are taken from the treatment code. PTR does not state a nitrogen rate.
# ZTDSR is zero-till direct-seeded rice. SSD is subsurface drip. PTR is puddled transplanted rice.
# The title places the work in the Western Indo-Gangetic Plains. The files do not name a country or give coordinates.

## ISSUES
# No country, coordinates, or dates in the grain sheet. PTR has no nitrogen rate, so N_fertilizer is not a treatment variable.
# write_files: missing variables: yield_moisture, country, latitude, longitude, K_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# FCR-Rana-Hydrus-2022.pdf, Figures.docx, and Tables.doc. They were not read as plot tables.
# HYDRUS-2D water-balance output is a model product.


carob_script <- function(path) {

"
irrigation using HYDRUS-2D in rice under annual rotation with wheat in Western Indo-Gangetic Plains

Asia. Conventional tillage-based and flood irrigated puddled transplanted rice (PTR) is a major contributor to faster depleting aquifers. Conservation agriculture based direct-seeded rice and subsurface drip were compared with PTR. The grain sheet records rice yield.
"

	uri <- "hdl:11529/10548771"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "CIMMYT Data-Grain.xlsx"]
	r <- carobiner::read.excel(f, sheet="Grain", col_names=FALSE)
	lab <- trimws(as.character(r[[1]]))
	keep <- lab %in% c("ZTDSR-N0", "ZTDSR-N75", "ZTDSR-N112.5", "ZTDSR-N150", "SSD-N0", "SSD-N75", "SSD-N112.5", "SSD-N150", "PTR")
	r <- r[keep, ]
	n_from <- function(z) {
		z <- sub("^SSD-", "", z)
		z <- sub("^ZTDSR-", "", z)
		out <- rep(NA_real_, length(z))
		has <- substr(z, 1, 1) == "N"
		out[has] <- as.numeric(sub("^N", "", z[has]))
		out
	}
	one_rep <- function(j, rep) {
		data.frame(
			location = "Western Indo-Gangetic Plains",
			trial_id = "rice",
			rep = rep,
			crop = "rice",
			treatment = as.character(r[[1]]),
			N_fertilizer = n_from(as.character(r[[1]])),
			yield = as.numeric(r[[j]]) * 1000,
			yield_part = "grain",
			on_farm = NA,
			is_survey = FALSE,
			irrigated = TRUE,
			geo_from_source = FALSE
		)
	}
	d <- rbind(one_rep(3, 1L), one_rep(4, 2L), one_rep(5, 3L))
	carobiner::write_files(path, meta, d)
}
