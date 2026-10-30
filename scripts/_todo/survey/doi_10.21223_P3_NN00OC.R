# R script for "carob"
# license: GPL (>=3)

## NOTES
# VISTA baseline, Nampula and Zambezia, Mozambique, May-June 2015. Country Mozambique is the title. Province, district, administrative post, and locality are on the household identification file. Labelled SPSS codes are converted with their value labels.
# Sweetpotato field counts are on the sweetpotato production file: upland and lowland fields planted last season. Those counts are not an area and not a yield. The one published bounding-box corner is not assigned to every household.
# Names of respondents and enumerators are not copied.

## ISSUES
# No coordinates and no yield. Fertilizer quantities are on another file and the unit is not applied here.
# write_files: missing carob_contributor, latitude, longitude. unknown variables: upland_fields_, lowland_fields_.

## NEW VARIABLES
# upland_fields_: number of sweetpotato fields on uplands (G02A).
# lowland_fields_: number of sweetpotato fields on lowlands (G03A).

## NOT PROCESSED
# Dietary diversity, seed-system modules, marketing, vitamin A knowledge, and the area and fertilizer file. Fertilizer unit codes are not converted to kg/ha.
# Household member and child modules. Names are not copied.


carob_script <- function(path) {

"
Dataset for: The Viable Sweetpotato Technologies in Africa (VISTA)-Mozambique: The Baseline Survey  for Nampula and Zambezia Provinces

The baseline survey for Viable Sweetpotato Technologies in Africa (VISTA) project in Mozambique was conducted in May-June 2015 in four districts of Nampula province (Monapo, Meconta, Nampula-Rapale and Murrupula) and two of Zambezia province (Alto Molocue and Gurue) in Mozambique.  About 640 households were interviewed. Most of the visited households had at least one child less than 5 years old, and 54% of them were female. The average age of the household heads was 39 years, and most of them with fourth grade education level. Overall, about 10% of the household heads were women. Other relevant items included were related to farmer organizations, crop production, Seed system and related activities, farmers' practices, attitude and perception on sweetpotato, Vitamin A and other nutrition knowledge and household and individually dietary diversity.
"

	uri <- "doi:10.21223/P3/NN00OC"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=7,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "treatment",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.35
	)

	lab <- function(x) {
		z <- trimws(as.character(haven::as_factor(x)))
		z[!nzchar(z) | z %in% c("NA", "NaN")] <- NA_character_
		z
	}
	num <- function(x) {
		z <- trimws(as.character(x))
		ok <- is.na(z) | z == "NA" | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	id <- haven::read_sav(ff[grepl("^PAGE 1_", basename(ff))])
	sp <- haven::read_sav(ff[grepl("^PAGE 6_", basename(ff))])
	d <- data.frame(
		hhid = lab(id$T_RG),
		treatment = lab(id$IN_CT),
		adm1 = lab(id$A01),
		adm2 = lab(id$A02),
		adm3 = lab(id$A03),
		location = lab(id$A04)
	)
	q <- data.frame(
		hhid = lab(sp$T_RG),
		upland_fields_ = num(sp$G02A),
		lowland_fields_ = num(sp$G03A)
	)
	d <- merge(d, q, by="hhid", all.x=TRUE)
	d$country <- "Mozambique"
	d$crop <- "sweetpotato"
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "Mozambique_VISTA_2015"
	d <- d[!is.na(d$hhid), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
