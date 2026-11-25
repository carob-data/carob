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
# prodlossfinal.do: not read. .do file. Observation records are missing from d until this file is standardized.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
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
		adm2 = trimws(as.character(r$dist)),
		location = nice(r$upa),
		hhid = as.character(r$id),
		trial_id = paste(r$id, r$year),
		crop = "wheat",
		yield_part = "grain",
		yield = as.numeric(r$yield),
		field_size = as.numeric(r$wheha),
		planting_date = as.character(r$year),
		blast_affected_ = as.integer(r$affected),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d$adm2[d$adm2 == "B.Baria"] <- "Brahmanbaria"
	d$adm2[d$adm2 == "ChapaiNG"] <- "Chapai Nawabganj"
	d$adm2[d$adm2 == "Kusthia"] <- "Kushtia"
	d$adm2[d$adm2 == "Mymemsingh"] <- "Mymensingh"
	d$adm2[d$adm2 == "Sirajgonj"] <- "Sirajganj"

	geo <- data.frame(
		adm2 = c("Brahmanbaria", "Bandarban", "Bhola", "Chandpur", "Chapai Nawabganj", "Chuadanga",
			"Comilla", "Faridpur", "Gazipur", "Gopalganj", "Habiganj", "Jamalpur", "Jessore",
			"Jhalokati", "Jhenaidah", "Kishoreganj", "Kushtia", "Madaripur", "Magura", "Manikganj",
			"Meherpur", "Mymensingh", "Naogaon", "Narail", "Natore", "Noakhali", "Pabna", "Rajbari",
			"Rajshahi", "Rangamati", "Satkhira", "Shariatpur", "Sirajganj", "Tangail"),
		latitude = c(23.957, 22.195, 22.686, 23.233, 24.596, 23.640,
			23.461, 23.607, 23.999, 23.005, 24.375, 24.937, 23.166,
			22.641, 23.545, 24.445, 23.901, 23.164, 23.487, 23.864,
			23.762, 24.747, 24.794, 23.173, 24.421, 22.872, 24.006, 23.757,
			24.374, 22.653, 22.719, 23.242, 24.453, 24.251),
		longitude = c(91.112, 92.218, 90.648, 90.671, 88.277, 88.842,
			91.181, 89.843, 90.420, 89.827, 91.416, 89.938, 89.208,
			90.199, 89.153, 90.777, 89.120, 90.190, 89.420, 90.005,
			88.632, 90.420, 88.932, 89.513, 88.986, 91.097, 89.237, 89.644,
			88.604, 92.178, 89.071, 90.435, 89.701, 89.917)
	)
	d <- merge(d, geo, by = "adm2", all.x = TRUE)

	carobiner::write_files(path, meta, d)
}
