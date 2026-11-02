# R script for "carob"
# license: GPL (>=3)

## NOTES
# AVCD orange-fleshed sweetpotato baseline in Bungoma, Busia, Homa Bay, and Migori, Kenya, March 2016. Kenya is named in the title and in geographic coverage.
# Household id, county, sub-county, ward, and sex of the household head are taken from geninfo.sav. The file labels county Homabay; that is Homa Bay.
# a15_gpsloc is latitude, longitude, altitude, and accuracy, separated by spaces. One row has no accuracy. The crop is sweetpotato.

## ISSUES
# One GPS altitude is about -506 m and is left as recorded. Ward names were not checked against GADM.
# out of bounds: elevation (-505.924, 1990.752)

## NOT PROCESSED
# Names, phone numbers, and national ids.
# Basic informtion.sav, food security.sav, rank characteristics.sav, rose Databaseimmunization.sav.
# rpt_e_childfoodconsume, rpt_e_femalefoodconsume, rpt_f_infantfeeding, rpt_sece_foodcnsmptn.
# Section 2 crop area, harvest, price, sales, fertilizer, labour, manure, pesticide, and seed files, and sec2c plot files.
# Section 3 sweetpotato variety, planting, labour, consumption, production, and transaction files.
# Section 4 assets, livestock, income, and social assets. Section 5 beneficiary files. Section 6 food insecurity.
# secb_screening.sav, secc1_hhhead_roster.sav, secc5_hhmembers_roster.sav, and Data dictionary.xlsx.


carob_script <- function(path) {

"
Dataset for: Accelerated Value Chain Development (AVCD) Program - The Baseline Survey for Orange-Fleshed Sweet Potato in Bungoma, Busia Homa-Bay, and Migori Counties of Kenya

The baseline survey for the Accelerated Value Chain Development (AVCD) program was carried out in some randomly selected wards of Bungoma, Busia, Homa-Bay and Migori counties in March 2016. Average number of members per household was 4.5 with majority being female (56.2). Most households (78.3%) were male headed with average age of household head at 44.9 years.
"

	uri <- "doi:10.21223/P3/KOVABF"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 10,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "geninfo.sav"]
	r <- haven::read_sav(f)
	county <- as.character(haven::as_factor(r$a11_county))
	county[county == "Homabay"] <- "Homa Bay"
	ward <- as.character(haven::as_factor(r$a12_ward))
	ward <- gsub("/", "/ ", ward, fixed = TRUE)
	ward <- carobiner::fix_name(ward, "title")
	ward <- gsub("/ ", "/", ward, fixed = TRUE)
	gps <- trimws(as.character(r$a15_gpsloc))
	parts <- strsplit(gps, " ", fixed = TRUE)
	lat <- as.numeric(vapply(parts, function(p) p[[1]], ""))
	lon <- as.numeric(vapply(parts, function(p) p[[2]], ""))
	alt <- as.numeric(vapply(parts, function(p) p[[3]], ""))
	when <- as.Date(substr(trimws(as.character(r$a2_dateofsurvey)), 1, 10))

	d <- data.frame(
		trial_id = "AVCD_OFSP_Kenya_2016",
		hhid = trimws(as.character(r$hhid)),
		country = "Kenya",
		adm1 = carobiner::fix_name(county, "title"),
		adm2 = carobiner::fix_name(as.character(haven::as_factor(r$a11_subcounty)), "title"),
		location = ward,
		crop = "sweetpotato",
		sex = tolower(as.character(haven::as_factor(r$a6_hhhgender))),
		latitude = lat,
		longitude = lon,
		elevation = alt,
		date = as.character(when),
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
