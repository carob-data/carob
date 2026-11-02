# R script for "carob"
# license: GPL (>=3)

## NOTES
# Variety means of triticale, barley, and wheat from the Datos sheet. Yield is t/ha, stored as kg/ha.
# Geographic coverage is Mexico: Estado de México, Hidalgo, and Tlaxcala. The place names carry those state abbreviations. No coordinates.
# Planting dates are Excel serials. They fall in June-August 2022. The title says PV 2017.
# Days to harvest are stored as maturity_days. Harvest date is planting date plus those days when the day count is present.

## ISSUES
# No coordinates. Excel planting serials are in 2022 while the title says PV 2017.
# write_files: missing variables: yield_moisture, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# Resumen de datos, which repeats means.
# Trigo, Cebada, and Triticale sheets did not read as tables.
# Observaciones, which are qualitative rust and vigor notes without a score.


carob_script <- function(path) {

"
Evaluation of different varieties of wheat, barley and triticale in Valle Altos (PV 2017)

Evaluation of different varieties of barley, triticale and wheat in Valles Altos region during agricultural cycle spring-summer 2017
"

	uri <- "hdl:11529/10548675"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
	)

	f <- ff[grepl("Resultados", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Datos", col_names=FALSE)
	x <- r[-1, ]
	variety <- trimws(as.character(x[[2]]))
	keep <- !is.na(variety) & nzchar(variety)
	x <- x[keep, ]
	variety <- variety[keep]

	crop_raw <- trimws(as.character(x[[1]]))
	crop <- rep(NA_character_, length(crop_raw))
	crop[crop_raw == "Triticale"] <- "triticale"
	crop[crop_raw == "Cebada"] <- "barley"
	crop[crop_raw == "Trigo"] <- "wheat"
	part <- ifelse(crop == "wheat", "seed", "grain")

	place <- trimws(as.character(x[[3]]))
	state <- rep(NA_character_, length(place))
	state[grepl("MEX$", place)] <- "Estado de México"
	state[grepl("HGO$", place)] <- "Hidalgo"
	state[grepl("TLX$", place)] <- "Tlaxcala"
	loc <- sub(" (MEX|HGO|TLX)$", "", place)
	loc <- carobiner::fix_name(loc, "title")

	num <- function(z) {
		z <- trimws(as.character(z))
		z[z %in% c("", "NA")] <- NA_character_
		as.numeric(z)
	}
	excel_date <- function(z) {
		as.character(as.Date(num(z), origin="1899-12-30"))
	}
	plant <- excel_date(x[[5]])
	days <- num(x[[7]])
	harv <- rep(NA_character_, length(plant))
	ok <- !is.na(plant) & !is.na(days)
	harv[ok] <- as.character(as.Date(plant[ok]) + days[ok])

	d <- data.frame(
		country = "Mexico",
		adm1 = state,
		location = loc,
		trial_id = paste(loc, crop),
		crop = crop,
		variety = variety,
		treatment = variety,
		elevation = num(x[[4]]),
		planting_date = plant,
		harvest_date = harv,
		flowering_days = num(x[[6]]),
		maturity_days = days,
		yield = num(x[[8]]) * 1000,
		yield_part = part,
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA,
		geo_from_source = FALSE
	)
	carobiner::write_files(path, meta, d)
}
