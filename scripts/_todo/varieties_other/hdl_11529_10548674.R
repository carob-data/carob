# R script for "carob"
# license: GPL (>=3)

## NOTES
# Wheat yields from three Guanajuato sites in the rendimientos workbook. Geographic coverage includes Guanajuato. The sheets name Irapuato, Yuriria, and Manuel Doblado, GTO.
# Irapuato Renkgha is already kg/ha. Yuriria Rendimiento is t/ha. Manuel Doblado Rend is kg/ha.
# Yuriria fertilizer is written N-P-K, 158-69-30 at sowing and 158-0-0 at the first topdressing. The sheet does not say oxide, so P and K are stored as written. Nitrogen is the sum of the two applications.
# Yuriria sowing and harvest are Excel serials.

## ISSUES
# No coordinates. Nitrogen, phosphorus, potassium, and dates are recorded for Yuriria only.
# write_files: missing variables: yield_moisture, latitude, longitude.

## NOT PROCESSED
# The right-hand block on the Irapuato sheet repeats variety means.
# ADT_GTO cover sheet names triticale and barley entries and has no yield table.
# Quality Analyses Wheat and Trait Names of Quality. The trait file is a dictionary.


carob_script <- function(path) {

"
Evaluation of wheat, barley and triticale elite lines for Bajio region in Central Mexico: OI 2016-17

Evaluation of wheat, barley and triticale elite lines for Bajio region in Central Mexico: OI 2016-17
"

	uri <- "hdl:11529/10548674"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	num <- function(z) {
		z <- trimws(as.character(z))
		z[z %in% c("", "NA")] <- NA_character_
		as.numeric(z)
	}
	excel_date <- function(z) as.character(as.Date(num(z), origin="1899-12-30"))

	f <- ff[grepl("Rendimientos", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Irapuato_OI 16", col_names=FALSE)
	rep <- trimws(as.character(r[[2]]))
	keep <- rep %in% c("1", "2", "3")
	d1 <- data.frame(
		location = "Irapuato",
		rep = as.integer(rep[keep]),
		variety = trimws(as.character(r[[1]][keep])),
		plant_height = num(r[[3]][keep]),
		maturity_days = num(r[[5]][keep]),
		yield = num(r[[7]][keep])
	)

	r <- carobiner::read.excel(f, sheet="Yuriria_OI 16", col_names=FALSE)
	lab <- trimws(as.character(r[[1]]))
	plant <- excel_date(r[[2]][which(lab == "Fecha de siembra:")[1]])
	harv <- excel_date(r[[2]][which(lab == "Cosecha")[1]])
	fert <- trimws(as.character(r[[4]]))
	npk <- fert[which(lab == "Fertilizacion a la siembra:")[1]]
	top <- fert[which(lab == "Fertilizacion 1er Reabone:")[1]]
	p1 <- as.numeric(strsplit(npk, "-", fixed=TRUE)[[1]])
	p2 <- as.numeric(strsplit(top, "-", fixed=TRUE)[[1]])
	hdr <- which(lab == "Variedad")
	sub <- r[(hdr + 1):nrow(r), ]
	variety <- trimws(as.character(sub[[1]]))
	ok <- !is.na(variety) & nzchar(variety)
	d2 <- data.frame(
		location = "Yuriria",
		rep = NA_integer_,
		variety = variety[ok],
		plant_height = NA_real_,
		maturity_days = num(sub[[3]][ok]),
		yield = num(sub[[4]][ok]) * 1000,
		flowering_days = num(sub[[2]][ok]),
		planting_date = plant,
		harvest_date = harv,
		N_fertilizer = p1[1] + p2[1],
		P_fertilizer = p1[2] + p2[2],
		K_fertilizer = p1[3] + p2[3]
	)

	r <- carobiner::read.excel(f, sheet="Manual DobladoOI 16", col_names=FALSE)
	c2 <- trimws(as.character(r[[2]]))
	hdr <- which(c2 == "Material")
	sub <- r[(hdr + 1):nrow(r), ]
	variety <- trimws(as.character(sub[[2]]))
	ok <- !is.na(variety) & nzchar(variety)
	d3 <- data.frame(
		location = "Manuel Doblado",
		rep = NA_integer_,
		variety = variety[ok],
		plant_height = NA_real_,
		maturity_days = NA_real_,
		yield = num(sub[[4]][ok])
	)

	d1$flowering_days <- NA_real_
	d1$planting_date <- NA_character_
	d1$harvest_date <- NA_character_
	d1$N_fertilizer <- NA_real_
	d1$P_fertilizer <- NA_real_
	d1$K_fertilizer <- NA_real_
	d3$flowering_days <- NA_real_
	d3$planting_date <- NA_character_
	d3$harvest_date <- NA_character_
	d3$N_fertilizer <- NA_real_
	d3$P_fertilizer <- NA_real_
	d3$K_fertilizer <- NA_real_
	d <- rbind(d1, d2, d3)
	d$country <- "Mexico"
	d$adm1 <- "Guanajuato"
	d$crop <- "wheat"
	d$treatment <- d$variety
	d$yield_part <- "seed"
	d$trial_id <- d$location
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- FALSE
	carobiner::write_files(path, meta, d)
}
