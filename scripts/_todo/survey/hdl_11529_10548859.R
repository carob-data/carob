# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household wheat production in Bangladesh for 2010 and 2016. The do-file labels yield as kg/ha and wheha as land under wheat, ha.
# Country is Bangladesh in geographicCoverage. District, division, and upazila are the columns dist, divi, and upa. Names stored in all capitals were changed to title case.
# The file has no coordinates. geo_from_source is FALSE.
# A yield of 0 is a reported zero, not a missing value.

## ISSUES
# missing variables: latitude, longitude
# unknown variable: season_year_ (season?)
# unknown variables: blast_affected_
# high crop yield: wheat: 24710.5

## NEW VARIABLES
# season_year_: year of the wheat season recorded in the household file.
# blast_affected_: 1 if the household is in a blast-affected district in that year, else 0.

## NOT PROCESSED
# Sales, stocks, consumption, prices, and the regression dummy columns.
# Total crop area and insurance expenditure.


carob_script <- function(path) {

"
Quantifying wheat blast disease induced yield and production losses of wheat: A quasi-natural experiment

Household wheat yield in Bangladesh before and after the 2016 wheat blast outbreak.
"

	uri <- "hdl:11529/10548859"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "bldata.dta"]
	r <- carobiner::read.dta(f)

	nice <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		up <- !is.na(x) & x == toupper(x)
		cap <- function(s) {
			w <- strsplit(s, " ", fixed=TRUE)[[1]]
			paste(toupper(substring(w, 1, 1)), substring(w, 2), sep="", collapse=" ")
		}
		x[up] <- vapply(tolower(x[up]), cap, character(1))
		x
	}

	d <- data.frame(
		country = "Bangladesh",
		adm1 = nice(r$divi),
		adm2 = as.character(r$dist),
		location = nice(r$upa),
		hhid = as.character(r$id),
		trial_id = paste(r$id, r$year),
		crop = "wheat",
		yield_part = "grain",
		yield = as.numeric(r$yield),
		field_size = as.numeric(r$wheha),
		season_year_ = as.integer(r$year),
		blast_affected_ = as.integer(r$affected),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
