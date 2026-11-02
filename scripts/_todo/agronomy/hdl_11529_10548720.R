# R script for "carob"
# license: GPL (>=3)

## NOTES
# Nitrogen rates are the planting application in Data-Muna. Yield is already kg/ha at 14% moisture.
# Phosphorus is 70 kg/ha P2O5 on every plot in the variable description, stored as elemental P.
# The potassium rate in that sheet is a dash, stored as 0.
# NDVI columns are repeated dates and are kept in the long table. One NDVI header date is before planting.

## ISSUES
# write_files: missing variables: country, latitude, longitude, harvest_date.
# The workbook names the site Muna and does not give a country or coordinates. geographicCoverage is empty.
# One NDVI column is dated before the planting date recorded in the variable description.
## NOT PROCESSED
# Map sheet plot layout.
# Phenology stage dates in the variable description.


carob_script <- function(path) {

"
Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Yucatan.

This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Yucatan.
"

	uri <- "hdl:11529/10548720"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;NDVI",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "GreenSeeker Yucatan 2012-2013.xlsx"]
	raw <- carobiner::read.excel(f, sheet="Data-Muna", col_names=FALSE)
	hdr <- as.character(unlist(raw[1, ]))
	unit <- as.character(unlist(raw[2, ]))
	r <- raw[-(1:2), ]
	r <- r[!is.na(r[[1]]), ]

	info <- carobiner::read.excel(f, sheet="Variable Description", col_names=FALSE)
	lab <- trimws(as.character(info[[2]]))
	val <- trimws(as.character(info[[3]]))
	pick <- function(label) {
		i <- which(lab == label)
		if (length(i) == 0) NA_character_ else val[i[1]]
	}
	excel_date <- function(x) {
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}

	p_txt <- pick("Phosphorus fertilizer rate")
	p_num <- as.numeric(strsplit(p_txt, " ")[[1]][1])
	k_txt <- pick("potassium fertilizer rate")
	# A dash in the variable description is no potassium applied.
	k_num <- if (is.na(k_txt) || k_txt %in% c("-", "")) 0 else as.numeric(k_txt)

	d <- data.frame(
		crop = "maize",
		location = "Muna",
		plot_id = as.character(r[[1]]),
		rep = as.integer(r[[2]]),
		treatment = as.character(r[[4]]),
		N_fertilizer = as.numeric(r[[4]]),
		plot_area = as.numeric(r[[5]]),
		plant_density = as.numeric(r[[6]]) * 10000,
		yield = as.numeric(r[[7]]),
		yield_moisture = 14,
		yield_part = "grain",
		variety = pick("Hibrid/Company"),
		land_prep_method = "conventional",
		planting_date = excel_date(pick("Planting Date")),
		P_fertilizer = p_num / 2.29,
		K_fertilizer = k_num
	)

	ir <- grep("irrigation", lab, ignore.case=TRUE)
	ir_dates <- excel_date(val[ir])
	ir_dates <- ir_dates[!is.na(ir_dates)]
	d$irrigation_dates <- paste(ir_dates, collapse="; ")
	d$irrigation_number <- as.integer(length(ir_dates))
	d$irrigated <- TRUE
	d$trial_id <- "Muna_2012"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$record_id <- seq_len(nrow(d))

	ndvi_cols <- which(hdr == "NDVI")
	ndvi_dates <- excel_date(unit[ndvi_cols])
	long <- NULL
	for (i in seq_along(ndvi_cols)) {
		x <- data.frame(
			record_id = d$record_id,
			date = ndvi_dates[i],
			NDVI = as.numeric(r[[ndvi_cols[i]]])
		)
		long <- if (is.null(long)) x else rbind(long, x)
	}
	long <- long[!is.na(long$NDVI), ]

	carobiner::write_files(path, meta, d, long=long)
}
