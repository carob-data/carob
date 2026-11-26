# R script for "carob"
# license: GPL (>=3)

## NOTES
# Site, soil, and design details come from the bundled paper (Hoque et al. 2023, Field Crops Research, doi:10.1016/j.fcr.2022.108791): BARI Regional Agricultural Research Station (RARS), Jamalpur, Mymensingh, Bangladesh (24.9419 N, 89.9279 E, 21.6 m), split-plot design, 120 m2 plots, loam soil (47.3% sand, 30.0% silt, 22.7% clay, pH 6.2, SOM 0.77%, total N 0.042% = 420 mg/kg).
# Each row is one season of a cropping sequence. The System Yield sheet reports rice-equivalent yield (REY, t/ha -> kg/ha). For rice, REY equals actual grain yield and is stored in yield; for wheat, maize, and mung bean, yield is left NA and REY is stored in rice_equivalent_yield_kg_ha_ so price-scaled values are not mixed with actual grain yield.
# Sequences from the description: RR rice-rice, RMb rice-mung bean, RW rice-wheat, RM rice-maize, RWMb rice-wheat-mung bean, RMMb rice-maize-mung bean.
# CT is conventional tillage. CA is untilled. ALT is tilled monsoon rice and untilled winter and spring crops.
# Monsoon is stored as kharif, winter as rabi, and spring/summer mung bean (March-June) as early wet.

## ISSUES
# Actual grain yields of wheat, maize, and mung bean are not reported directly in the System Yield sheet (only rice-equivalent yields are given), so yield is NA for non-rice crops.

## NEW VARIABLES
# cropping_system_: sequence code from the System Yield sheet.
# rice_equivalent_yield_kg_ha_: rice-equivalent yield (kg/ha) from the System Yield sheet.

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
		publication = "doi:10.1016/j.fcr.2022.108791",
		project = "CSISA-BD;SRFSI",
		design = "split-plot",
		data_type = "experiment",
		treatment_vars = "land_prep_method;cropping_system_",
		response_vars = "yield",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "20201227_Figure-CA Arshadul-Dataverse.xlsx"]
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
		out[part == "early wet" & code %in% c("RWMb", "RMMb")] <- "mung bean"
		out
	}
	prep_of <- function(till, part) {
		out <- rep("conventional", length(till))
		out[till == "CA"] <- "none"
		out[till == "ALT" & part != "kharif"] <- "none"
		out
	}
	add <- function(part, rey) {
		crop <- crop_of(sys, part)
		keep <- !is.na(crop)
		cr <- crop[keep]
		ry <- rey[keep]
		data.frame(
			country = "Bangladesh",
			adm1 = "Mymensingh",
			adm2 = "Jamalpur",
			location = "Jamalpur",
			site = "BARI Regional Agricultural Research Station",
			latitude = 24.9419,
			longitude = 89.9279,
			elevation = 21.6,
			trial_id = year[keep],
			rep = rep[keep],
			season = part,
			crop = cr,
			cropping_system_ = sys[keep],
			land_prep_method = prep_of(till, part)[keep],
			treatment = paste(sys[keep], till[keep]),
			plot_area = 120,
			soil_texture = "loam",
			soil_sand = 47.3,
			soil_silt = 30.0,
			soil_clay = 22.7,
			soil_pH = 6.2,
			soil_SOM = 0.77,
			soil_N_total = 420,
			yield = ifelse(cr == "rice", ry, NA_real_),
			rice_equivalent_yield_kg_ha_ = ry,
			yield_part = ifelse(cr == "mung bean", "seed", "grain"),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
	}
	d <- rbind(
		add("kharif", mon),
		add("rabi", win),
		add("early wet", summ)
	)
	d$plot_id <- paste(d$trial_id, d$rep, d$cropping_system_, d$treatment, d$season, sep="_")

	carobiner::write_files(path, meta, d)
}
