# R script for "carob"
# license: GPL (>=3)

## NOTES
# The dataset description places the trial on loamy soil in northern Bangladesh.
# Each row is one season of a cropping sequence. Yield is the rice-equivalent yield column, t/ha converted to kg/ha. It is not the raw yield of wheat, maize, or mung bean.
# Sequences from the description: RR rice-rice, RMb rice-mung bean, RW rice-wheat, RM rice-maize, RWMb rice-wheat-mung bean, RMMb rice-maize-mung bean.
# CT is conventional tillage. CA is untilled. ALT is tilled monsoon rice and untilled winter and summer crops. The description states the winter rule and does not mention the summer crop separately.
# Monsoon is stored as kharif and winter as rabi.
# No coordinates are in the workbook.

## ISSUES
# write_files: missing variables: on_farm, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: cropping_system_ (crop?). invalid terms: season: summer.

## NEW VARIABLES
# cropping_system_: sequence code from the System Yield sheet.

## NOT PROCESSED
# Calorie and protein yields.
# Costs, labor, and gross margins. The currency is not named in the sheet.
# The layout sketch and the pooled summary tables.


carob_script <- function(path) {

"
Reduced tillage and crop diversification can improve productivity and profitability of rice-based rotations of the Eastern Gangetic Plains

A field study in northern Bangladesh compared three tillage options and six rice-based cropping sequences. Grain yield in the table is rice-equivalent yield for the monsoon, winter, and summer crops.
"

	uri <- "hdl:11529/10548910"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;cropping_system_",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[grepl("\\.xlsx$", basename(ff))]
	r <- carobiner::read.excel(f, sheet="System Yield", col_names=FALSE, col_types="text")
	r <- r[2:nrow(r), ]
	# A pooled table sits to the right of column 7. Columns 1 to 7 are the plot table.

	sys <- trimws(as.character(r[[3]]))
	till <- trimws(as.character(r[[4]]))
	year <- trimws(as.character(r[[1]]))
	rep <- as.integer(as.numeric(r[[2]]))
	mon <- as.numeric(r[[5]]) * 1000
	win <- as.numeric(r[[6]]) * 1000
	summ <- as.numeric(r[[7]]) * 1000

	crop_of <- function(code, part) {
		out <- rep(NA_character_, length(code))
		out[part == "kharif"] <- "rice"
		out[part == "rabi" & code == "RR"] <- "rice"
		out[part == "rabi" & code == "RMb"] <- "mung bean"
		out[part == "rabi" & code %in% c("RW", "RWMb")] <- "wheat"
		out[part == "rabi" & code %in% c("RM", "RMMb")] <- "maize"
		out[part == "summer" & code %in% c("RWMb", "RMMb")] <- "mung bean"
		out
	}
	prep_of <- function(till, part) {
		out <- rep("conventional", length(till))
		out[till == "CA"] <- "none"
		out[till == "ALT" & part != "kharif"] <- "none"
		out
	}
	add <- function(part, yield) {
		crop <- crop_of(sys, part)
		keep <- !is.na(crop)
		data.frame(
			country = "Bangladesh",
			trial_id = year[keep],
			rep = rep[keep],
			season = part,
			crop = crop[keep],
			cropping_system_ = sys[keep],
			land_prep_method = prep_of(till, part)[keep],
			treatment = paste(sys[keep], till[keep]),
			yield = yield[keep],
			yield_part = "grain",
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}
	d <- rbind(
		add("kharif", mon),
		add("rabi", win),
		add("summer", summ)
	)
	d$plot_id <- paste(d$trial_id, d$rep, d$cropping_system_, d$treatment, d$season, sep="_")

	carobiner::write_files(path, meta, d)
}
